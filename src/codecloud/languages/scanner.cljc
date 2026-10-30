(ns codecloud.languages.scanner
  "Token streams for adapters, as {:kind :text :line :column} with
  one-based positions, lexed as highlighting is, so both agree."
  (:require [codecloud.languages.lexer :as lexer]
            [codecloud.languages.lexicons :as lexicons]))

(defn- marked-continuation
  "A line's placed tokens with the first one that is not space marked
  :continues?."
  [placed]
  (if-let [index (first (keep-indexed (fn [index token] (when-not (= :space (:kind token)) index)) placed))]
    (assoc-in placed [index :continues?] true)
    placed))

(defn token-stream
  "Every token of `text-lines`, spaces and comments included, with
  :continues? on a line's first token inside an earlier span or region."
  [language-id text-lines]
  (into []
        cat
        (map (fn [line-number {:keys [tokens continued?]}]
               (let [placed (first (reduce (fn [[placed column] [kind text]]
                                             [(conj placed {:kind kind :text text :line line-number :column column})
                                              (+ column (count text))])
                                           [[] 1]
                                           tokens))]
                 (if continued? (marked-continuation placed) placed)))
             (iterate inc 1)
             (lexer/tokenize-lines-marked (lexicons/lexicon-for language-id) text-lines))))

(defn significant
  "The tokens that are neither space nor comment."
  [tokens]
  (filterv #(not (contains? #{:space :comment} (:kind %))) tokens))

(defn unquoted
  "The text of a string token without the quotes around it."
  [text]
  (let [quote-characters #{"\"" "'" "`"}
        first-character (subs text 0 (min 1 (count text)))]
    (if (and (>= (count text) 2)
             (contains? quote-characters first-character)
             (= first-character (subs text (dec (count text)))))
      (subs text 1 (dec (count text)))
      text)))

(defn token-at
  "The token at `index`, or nil past the end."
  [tokens index]
  (when (< -1 index (count tokens)) (nth tokens index)))

(defn token-is?
  "True when the token at `index` has `kind` and, when given, `text`."
  ([tokens index kind]
   (= kind (:kind (token-at tokens index))))
  ([tokens index kind text]
   (let [token (token-at tokens index)]
     (and (= kind (:kind token)) (= text (:text token))))))

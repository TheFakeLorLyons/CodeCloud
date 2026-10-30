(ns codecloud.presentation.highlight
  "Colored tokens and minimap runs for code panels, lexed with the lexicon
  of the file's language."
  (:require [codecloud.languages.lexer :as lexer]
            [codecloud.languages.lexicons :as lexicons]
            [codecloud.presentation.page :as page]))

(defn tokenize-lines
  "Vector of token vectors, one per line, each token [kind text], for
  lines of `language-id`, Clojure when none is given."
  ([lines] (tokenize-lines :clojure lines))
  ([language-id lines]
   (lexer/tokenize-lines (lexicons/lexicon-for language-id) lines)))

(def ^:private closing-brackets #{")" "]" "}"})

(defn bracket-step
  "[token-depth next-depth] of a token met at bracket nesting `depth`. An
  opening bracket stands at `depth` and opens one more, and a closing one
  stands at the depth of the bracket it closes."
  [depth [kind text]]
  (cond
    (not= :bracket kind) [depth depth]
    (contains? closing-brackets text) (let [closed (max 0 (dec depth))] [closed closed])
    :else [depth (inc depth)]))

(defn line-bracket-depths
  "Bracket nesting at the start of each of a file's token lines."
  [token-lines]
  (vec (take (count token-lines)
             (reductions (fn [depth tokens] (reduce #(second (bracket-step %1 %2)) depth tokens))
                         0
                         token-lines))))

(def minimap-kinds
  "Token kinds a page's minimap colors, in the order its shader numbers
  them. Constants share the color of numbers."
  [:text :comment :string :keyword :number :special :definition :reader :bracket :discard])

(def ^:private minimap-kind-index
  (let [indexes (zipmap minimap-kinds (range))]
    (assoc indexes :constant (:number indexes))))

(defn- advance-column
  "Column after the whitespace `text` starting at `column`; tabs move to
  the next multiple of `tab-size`."
  [column text tab-size]
  (reduce (fn [current character]
            (if (= "\t" (str character))
              (* tab-size (inc (quot current tab-size)))
              (inc current)))
          column
          text))

(defn- capped-runs
  "At most `most-runs` runs within `width` columns; when there are more,
  the last kept run stretches to cover the rest."
  [runs width most-runs]
  (let [clipped (into []
                      (comp (filter (fn [[start]] (< start width)))
                            (map (fn [[start length kind-index]]
                                   [start (min length (- width start)) kind-index])))
                      runs)]
    (if (<= (count clipped) most-runs)
      clipped
      (let [kept (subvec clipped 0 (dec most-runs))
            [first-start _ kind-index] (nth clipped (dec most-runs))
            [last-start last-length] (peek clipped)]
        (conj kept [first-start (- (+ last-start last-length) first-start) kind-index])))))

(defn line-runs
  "A tokenized line's minimap runs as [start-column length kind-index],
  merging neighbors of one kind, cut at `width` columns. Tabs reach the
  next of the page's tab stops."
  [tokens width most-runs]
  (loop [remaining tokens
         column 0
         runs []]
    (if-let [[kind text] (first remaining)]
      (if (= :space kind)
        (recur (rest remaining) (advance-column column text page/tab-size) runs)
        (let [length (count text)
              kind-index (get minimap-kind-index kind 0)
              [last-start last-length last-kind] (peek runs)
              touching? (and last-kind (= last-kind kind-index) (= (+ last-start last-length) column))]
          (recur (rest remaining)
                 (+ column length)
                 (if touching?
                   (conj (pop runs) [last-start (+ last-length length) kind-index])
                   (conj runs [column length kind-index])))))
      (capped-runs runs width most-runs))))

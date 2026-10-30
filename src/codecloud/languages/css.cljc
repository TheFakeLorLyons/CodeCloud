(ns codecloud.languages.css
  "Reads style sheets in CSS, SCSS, Sass, and Less: the sheets each one
  imports, uses, or forwards."
  (:require [clojure.string :as string]
            [codecloud.languages.scanner :as scanner]
            [codecloud.languages.structure :as structure]))

(defn- url-argument
  "[address end] for url(...) whose `url` word sits at `index`: the quoted
  string inside it, or its unquoted text."
  [tokens index]
  (let [closing (some #(when (= ")" (:text (nth tokens %))) %) (range (+ 2 index) (count tokens)))
        inside (when closing (subvec tokens (+ 2 index) closing))]
    (when closing
      [(if (and (= 1 (count inside)) (= :string (:kind (first inside))))
         (scanner/unquoted (:text (first inside)))
         (apply str (map :text inside)))
       (inc closing)])))

(defn- rule-addresses
  "The addresses an @import rule at `index` names, which may be several
  in Sass, up to the semicolon that ends it."
  [tokens index first-only?]
  (loop [position (inc index)
         found []]
    (let [{:keys [kind text] :as token} (scanner/token-at tokens position)]
      (cond
        (or (nil? token) (= ";" text) (and first-only? (seq found))) found
        (= :string kind) (recur (inc position) (conj found (scanner/unquoted text)))
        (= "," text) (recur (inc position) found)
        (and (= "url" text) (= "(" (:text (scanner/token-at tokens (inc position)))))
        (if-let [[address end] (url-argument tokens position)]
          (recur end (conj found address))
          found)
        (and (= "(" text) (empty? found))
        (recur (or (some #(when (= ")" (:text (nth tokens %))) (inc %)) (range position (count tokens)))
                   (count tokens))
               found)
        :else found))))

(defn imports
  "Every sheet a style sheet's significant tokens import."
  [tokens]
  (into []
        (comp (keep-indexed (fn [index {:keys [kind text line]}]
                              (when (= :special kind)
                                (case (string/lower-case text)
                                  "@import" [index line false]
                                  ("@use" "@forward") [index line true]
                                  nil))))
              (mapcat (fn [[index line first-only?]]
                        (for [address (rule-addresses tokens index first-only?)]
                          {:specifier address :kind :import :resolver :css :line line :binds []}))))
        tokens))

(defn read-style-sheet
  "Facts of a style sheet: the sheets it imports, its rules as top-level
  forms, and its Sass mixins and functions."
  [path text options]
  (structure/read-structured :css path text options
                             (comp imports scanner/significant)
                             structure/style-sheet-structure))

(ns codecloud.model.uses
  "Where a definition is used, read from the text of the files that may
  use it, through the aliases and refers of Clojure files using a Clojure
  namespace, and by name elsewhere, outside strings and comments."
  (:require [clojure.string :as string]
            [codecloud.languages.lexer :as lexer]
            [codecloud.languages.lexicons :as lexicons]
            [codecloud.model.parse :as parse]
            [codecloud.model.resolution :as resolution]
            [codecloud.model.source :as source]))

(defn named-places
  "Where a file's `text` names `identifier` outside strings and comments,
  one-based. In Clojure an alias may qualify the name, as plot/plot."
  [language-id text identifier]
  (let [lines (source/split-lines (source/normalize-text text))
        qualified-ending (str "/" identifier)
        clojure? (= :clojure language-id)
        names-it? (fn [[kind token-text]]
                    (and (= :text kind)
                         (or (= identifier token-text)
                             (and clojure? (string/ends-with? token-text qualified-ending)))))]
    (into []
          (comp (map-indexed (fn [line-index tokens]
                               (loop [column 0
                                      remaining tokens
                                      found []]
                                 (if-let [[_kind token-text :as token] (first remaining)]
                                   (recur (+ column (count token-text))
                                          (rest remaining)
                                          (if (names-it? token)
                                            (conj found {:line (inc line-index) :column (inc column)})
                                            found))
                                   found))))
                cat)
          (lexer/tokenize-lines (lexicons/lexicon-for language-id) lines))))

(defn- clojure-namespace?
  "True for a page that is a Clojure namespace, whose vars other Clojure
  files reach through their requires."
  [page]
  (and (= :namespace (:kind page)) (= :clojure (:language page :clojure))))

(defn- module-targets
  "The strings the Clojure file at `path` requires any of `page-paths`
  by, as src/app/core.cljs requires web/js/plot.js by
  \"../../web/js/plot.js\"."
  [path text page-paths]
  (let [wanted (set page-paths)
        directory (source/directory-of path)]
    (into []
          (comp (filter :js-module?)
                (map :target)
                (filter #(contains? wanted (resolution/joined directory %)))
                (distinct))
          (:requirements (parse/analyze-file path text)))))

(defn places
  "Where `file` uses `definition` of `page`, as one-based {:line :column}.
  Clojure reaches vars through aliases and refers; elsewhere a use names
  the definition."
  [{:keys [path language text]} page definition]
  (let [file-language (or language :clojure)
        {definition-name :name declared-path :path declared-line :line} definition
        by-name (fn []
                  (cond->> (named-places file-language text definition-name)
                    (= path declared-path) (into [] (remove #(= declared-line (:line %))))))]
    (cond
      (not= :clojure file-language) (by-name)
      (clojure-namespace? page) (parse/uses-of path text (:name page) definition-name)
      :else (let [targets (module-targets path text (map :path (:files page)))]
              (if (seq targets)
                (->> targets
                     (mapcat #(parse/uses-of path text % definition-name))
                     distinct
                     (sort-by (juxt :line :column))
                     vec)
                (by-name))))))

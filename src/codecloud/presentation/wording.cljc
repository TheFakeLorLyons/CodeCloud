(ns codecloud.presentation.wording
  "Words for a cloud's pages, following its languages: namespaces for
  Clojure, modules for Python or a mix."
  (:require [codecloud.languages.catalog :as catalog]))

(defn languages-present
  "Ids of the languages a snapshot's pages are in."
  [snapshot]
  (into (sorted-set) (map #(:language % :clojure)) (vals (:namespaces snapshot))))

(defn unit-words
  "[singular plural] naming a snapshot's pages: the word of its one
  language, or module when it holds several languages or none."
  [snapshot]
  (let [present (languages-present snapshot)]
    (if (= 1 (count present))
      (:unit (:words (catalog/language (first present))))
      ["module" "modules"])))

(defn node-words
  "The words for a page's own language: its :unit, what it :uses, and
  what it is :used-by."
  [node]
  (:words (catalog/language (:language node :clojure))))

(defn kind-description
  "A line saying what a page is when its name alone does not, such as
  Python module, or nil for a Clojure namespace."
  [node]
  (case (:kind node)
    :script "Script without a namespace"
    :module (let [{:keys [label words]} (catalog/language (:language node :clojure))]
              (str label " " (first (:unit words))))
    nil))

(defn cycle-heading
  "What the inspector calls a cycle of `severity`."
  [severity]
  (if (= :warning severity) "↻ Cyclic import" "↻ Cyclic dependency"))

(defn cycle-explanation
  "Why a cycle on `platforms` matters, in the words of its language."
  [{:keys [severity platforms]}]
  (let [platform (first (sort platforms))]
    (cond
      (and (= :warning severity) (= :python platform))
      (str "Python allows it, but a module using another's names mid-load fails. Moving one import into "
           "the function that needs it breaks the loop.")

      (and (= :warning severity) (= :javascript platform))
      (str "JavaScript allows it, but a value read before its module loads is undefined. Moving the shared "
           "part into its own module breaks the loop.")

      (and (= :warning severity) (= :shell platform))
      "Only a guard stops scripts that source each other."

      (= :warning severity)
      "Allowed by the language, with care."

      (contains? catalog/clojure-platforms platform)
      "Clojure refuses to load it. Removing any one require along the loop breaks it."

      :else
      (str (:label (catalog/language platform)) " refuses to build it. Removing any one import along the "
           "loop breaks it."))))

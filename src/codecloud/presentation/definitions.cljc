(ns codecloud.presentation.definitions
  "Where a name written on a page is defined among the pages shown, on the
  page itself, on the pages it uses that define the name, or failing
  both, on any page shown. Names match as code writes them, so a call to
  read finds Board::read."
  (:require [clojure.string :as string]))

(defn- last-part
  "A name's last part, after any ::, /, or ., as in Board::read,
  str/join, or Stream.read."
  [definition-name]
  (or (peek (string/split definition-name #"::|/|\.")) definition-name))

(defn- names-match?
  "True when `written`, a name as code writes it, can mean
  `definition-name`, being the same name or ending in the same last part."
  [written definition-name]
  (or (= written definition-name) (= (last-part written) (last-part definition-name))))

(defn- defined-on
  "The definitions on page `id` that `written` can mean, each as
  {:namespace-id :path :name :line}."
  [namespaces id written]
  (let [{:keys [definitions files]} (get namespaces id)]
    (for [{definition-name :name :keys [path line]} definitions
          :when (names-match? written definition-name)]
      {:namespace-id id :path (or path (:path (first files))) :name definition-name :line line})))

(defn choices
  "Definitions `written` can mean on page `page-id`, most likely first,
  the page's own before those of pages it uses, then other shown pages',
  at most `limit`."
  [snapshot page-id written limit]
  (let [namespaces (:namespaces snapshot)
        uses-of (fn [uses] (reduce + 0 (keep (fn [[definition-name use-count]]
                                               (when (names-match? written definition-name) use-count))
                                             uses)))
        used-ids (->> (:edges snapshot)
                      (keep (fn [{:keys [from to uses]}]
                              (when (and (= page-id from) (not= page-id to))
                                (let [use-count (uses-of uses)]
                                  (when (pos? use-count) [to use-count])))))
                      (sort-by (fn [[id use-count]] [(- use-count) id]))
                      (map first))
        near (distinct (mapcat #(defined-on namespaces % written) (cons page-id used-ids)))
        found (if (seq near)
                near
                (mapcat #(defined-on namespaces % written) (sort (remove #{page-id} (keys namespaces)))))]
    (vec (take limit found))))

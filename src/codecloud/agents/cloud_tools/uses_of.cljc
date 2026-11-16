(ns codecloud.agents.cloud-tools.uses-of
  "The uses_of tool, every line using a definition."
  (:require [clojure.string :as string]
            [codecloud.agents.cloud-tools.common :as common]
            [codecloud.counting :refer [counted]]
            [codecloud.model.uses :as uses]
            [codecloud.presentation.page :as page]))

;; uses_of

(defn- uses-files
  "The files that may use a definition of the page `id`, which are its own
  and those of every page requiring it."
  [snapshot id]
  (let [nodes (:namespaces snapshot)]
    (into [] (comp (mapcat #(get-in nodes [% :files])) (map :path) (distinct))
          (cons id (get-in nodes [id :required-by])))))

(defn- through-requires?
  "True when uses of the page's definitions are read through requires
  rather than found by name: for a Clojure namespace."
  [node]
  (and (= :namespace (:kind node)) (= :clojure (page/language-of node))))

(defn- uses-answer [{:keys [snapshot texts] :as context} arguments]
  (let [{:keys [id definition error]} (common/find-definition context arguments)]
    (if error
      {:error error}
      (let [nodes (:namespaces snapshot)
            node (get nodes id)
            files (common/file-index snapshot)
            limit (common/count-argument arguments "limit" 100 1 500)
            longest-code 120
            definition-name (:name definition)
            places (for [file-path (uses-files snapshot id)
                         :let [text (get texts file-path)]
                         :when (and text (string/includes? text definition-name))
                         place (uses/places {:path file-path :language (get-in files [file-path :language]) :text text}
                                            node definition)]
                     (assoc place :path file-path))
            lines-of (memoize (fn [file-path] (common/text-lines (get texts file-path))))
            own-paths (into #{} (map :path) (:files node))
            own-count (count (filter #(contains? own-paths (:path %)) places))
            other-count (- (count places) own-count)
            heading (str (if (through-requires? node) (str id "/" definition-name) (str definition-name " in " id)) ": "
                         (string/replace (name (or (:kind definition) :definition)) "-" " ")
                         (when (and (:path definition) (not= id (:path definition))) (str " in " (:path definition)))
                         (when (:line definition) (str ", " (common/line-span (:line definition) (:end-line definition))))
                         ".")
            ;; Several uses on one line share a row.
            line-row (fn [[{:keys [path line comment? declaration?]} :as line-places]]
                       (let [around (when-not (or comment? declaration?)
                                      (common/definition-around (get nodes (get-in files [path :page-id])) path line))
                             code (get (lines-of path) (dec line))]
                         (str "  " path ":" line " "
                              (cond
                                declaration? "in its require"
                                comment? "in a comment form"
                                around (str "in " (:name around))
                                :else "at the top level")
                              (when (> (count line-places) 1) (str ", " (count line-places) " times"))
                              (when code (str ": " (common/clipped code longest-code))))))
            line-groups (partition-by (juxt :path :line) (sort-by (juxt :path :line :column) places))]
        {:text
         (string/join
          "\n"
          (concat
           [heading]
           (if (empty? places)
             [(str "No uses found in its own page or the " (counted (count (remove #{id} (:required-by node))) "page")
                   " requiring it.")]
             (concat [(str "Found " (counted (count places) "use")
                           (cond
                             (zero? own-count) ", all in other pages:"
                             (zero? other-count) ", all within its own page:"
                             :else (str ": " other-count " in other pages and " own-count " within its own page:")))]
                     (map line-row (take limit line-groups))
                     (when (> (count line-groups) limit)
                       [(str "  and " (counted (- (count line-groups) limit) "more line" "more lines"))])))
           (when-not (through-requires? node)
             [(str "Uses were found by name in the files of " id " and the pages requiring it, "
                   "so another thing of the same name may be among them.")])))}))))

(def tool
  {:spec {"name" "uses_of"
          "title" "Uses of a definition"
          "description" (str "Every file and line using a definition, with the code on each line: read through aliases and "
                             "refers in Clojure, and by name in other languages.")
          "inputSchema" {"type" "object"
                         "properties" {"name" {"type" "string"
                                               "description" (str "The definition, qualified by its page as app.db/save! or "
                                                                  "web/js/plot.js/plot, or bare with page")}
                                       "page" (assoc common/page-property "description" "The page defining a bare name")
                                       "limit" {"type" "integer" "minimum" 1 "maximum" 500 "default" 100
                                                "description" "Most lines of uses to list"}}
                         "required" ["name"]}
          "annotations" common/read-only}
   :answer uses-answer
   :paths-to-read (fn [{:keys [snapshot] :as context} arguments]
                    (let [{:keys [id]} (common/find-definition context arguments)]
                      (if id (uses-files snapshot id) [])))
   :touched (fn [context arguments]
              (let [{:keys [definition]} (common/find-definition context arguments)]
                (when (:path definition)
                  [{:path (:path definition) :start (:line definition) :end (or (:end-line definition) (:line definition))
                    :doing (str "finding uses of " (:name definition))}])))})

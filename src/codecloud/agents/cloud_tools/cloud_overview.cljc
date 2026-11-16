(ns codecloud.agents.cloud-tools.cloud-overview
  "The cloud_overview tool, the codebase at a glance."
  (:require [clojure.string :as string]
            [codecloud.agents.cloud-tools.common :as common]
            [codecloud.counting :refer [counted]]
            [codecloud.presentation.page :as page]))

;; cloud_overview

(defn- languages-line
  "The languages of `nodes` with their page counts, most pages first."
  [nodes]
  (let [most-listed 8
        by-count (sort-by (fn [[language-id page-count]] [(- page-count) (name language-id)])
                          (frequencies (map page/language-of nodes)))]
    (if (= 1 (count by-count))
      (str "all " (common/language-label (ffirst by-count)))
      (str "in " (common/some-of (map (fn [[language-id page-count]] (str (common/language-label language-id) " " page-count)) by-count)
                                 most-listed)))))

(defn- under-folder? [folder node]
  (let [directory (page/directory node)]
    (or (= "" folder) (= folder directory) (string/starts-with? directory (str folder "/")))))

(defn- page-row
  "One line about a page for a list of pages, giving its size, functions,
  dependents, importance, and troubles."
  [node]
  (let [{:keys [functions dependents complection]} (:metrics node)]
    (str "  " (common/page-label node) ": "
         (string/join ", " (remove nil? [(counted (common/page-lines node) "line")
                                         (when (pos? (or functions 0)) (counted functions "function"))
                                         (when (pos? (or dependents 0)) (str "required by " (counted dependents "page")))
                                         (str "importance " (common/hundredths (:importance node 0)))
                                         (when (pos? (or complection 0)) (str "complection " (common/hundredths complection)))
                                         (when (:cycle-id node) "in a cycle")
                                         (when (:parse-error? node) "does not parse")])))))

(defn- parse-error-rows [nodes]
  (for [node (sort-by :id nodes)
        {:keys [path parse-error]} (:files node)
        :when parse-error]
    (str "  " (:id node) (when-not (= path (:id node)) (str " (" path ")")) ": " (:message parse-error)
         (when (:line parse-error) (str ", at line " (:line parse-error))))))

(defn- folder-argument [context arguments]
  (let [given (common/forward-slashes (common/text-argument arguments "folder"))]
    (-> (if (re-find common/absolute-start given) (or (first (common/cloud-paths context given)) given) given)
        (string/replace #"^\./?|/+$" ""))))

(defn- overview [{:keys [snapshot cloud-name] :as context} arguments]
  (let [folder (folder-argument context arguments)
        limit (common/count-argument arguments "limit" 15 1 100)
        most-folders 12
        most-cycles 10
        all-nodes (vals (:namespaces snapshot))
        nodes (if (string/blank? folder) all-nodes (filter #(under-folder? folder %) all-nodes))
        node-ids (into #{} (map :id) nodes)
        {:keys [files definitions edges]} (:totals snapshot)
        folders (->> (group-by page/directory nodes)
                     (map (fn [[directory folder-nodes]]
                            {:folder directory :pages (count folder-nodes) :lines (reduce + 0 (map common/page-lines folder-nodes))}))
                     (sort-by (juxt (comp - :lines) :folder)))
        cycles (filter #(some node-ids (:members %)) (:cycles snapshot))
        [failing allowed] ((juxt remove filter) #(= :warning (:severity %)) cycles)
        unparsed (filter :parse-error? nodes)]
    (cond
      (empty? all-nodes) {:text (str (or cloud-name "The cloud") " holds no pages: CodeCloud found no source files to read.")}
      (empty? nodes) {:error (str "No page lies under " folder ".")}
      :else
      {:text (string/join
              "\n"
              (concat
               [(if (string/blank? folder)
                  (str (or cloud-name "The cloud") ": " (counted (count nodes) "page") " in " (counted (or files 0) "file") ", "
                       (counted (reduce + 0 (map common/page-lines nodes)) "line") ", " (counted (or definitions 0) "definition") ", and "
                       (counted (or edges 0) "dependency" "dependencies") " between pages, " (languages-line nodes) ".")
                  (str (or cloud-name "The cloud") ", under " folder ": " (count nodes) " of " (counted (count all-nodes) "page") ", "
                       (counted (reduce + 0 (map common/page-lines nodes)) "line") ", " (languages-line nodes) "."))
                ""
                "Folders, by lines:"]
               (for [{:keys [folder pages lines]} (take most-folders folders)]
                 (str "  " (common/folder-label folder) ": " (counted pages "page") ", " (counted lines "line")))
               (when (> (count folders) most-folders)
                 [(str "  and " (counted (- (count folders) most-folders) "more folder" "more folders"))])
               ["" "Most important pages:"]
               (map page-row (take limit (sort-by (juxt (comp - #(:importance % 0)) :id) nodes)))
               (when (seq failing)
                 (concat ["" (str "Cycles that fail to load (" (count failing) "):")]
                         (map #(str "  " (common/cycle-chain %)) (take most-cycles failing))))
               (when (seq allowed)
                 (concat ["" (str "Loops their languages allow (" (count allowed) "):")]
                         (map #(str "  " (common/cycle-chain %)) (take most-cycles allowed))))
               (when (seq unparsed)
                 (concat ["" "Files that do not parse:"] (parse-error-rows unparsed)))))})))

(def tool
  {:spec {"name" "cloud_overview"
          "title" "The codebase at a glance"
          "description" (str "The codebase's shape from CodeCloud's analysis: its folders, its most important pages with their "
                             "lines, functions, dependents, and complection, its dependency cycles, and files that do not parse. "
                             "A page is a namespace, or a file in a language without namespaces.")
          "inputSchema" {"type" "object"
                         "properties" {"folder" {"type" "string"
                                                 "description" "Only pages under this folder, relative to the cloud's folder"}
                                       "limit" {"type" "integer" "minimum" 1 "maximum" 100 "default" 15
                                                "description" "Most pages to list"}}}
          "annotations" common/read-only}
   :answer overview
   :touched (fn [_context _arguments] [{:doing "surveying the codebase"}])})

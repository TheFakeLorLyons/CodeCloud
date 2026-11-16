(ns codecloud.agents.cloud-tools.cloud-view
  "The cloud_view tool, what the person's filter shows."
  (:require [clojure.string :as string]
            [codecloud.agents.cloud-tools.common :as common]
            [codecloud.counting :refer [counted]]
            [codecloud.presentation.page :as page]
            [codecloud.presentation.visibility :as visibility]))

;; cloud_view

(defn- filter-words
  "What the filter hides, in words, or nil when it hides nothing."
  [{:keys [active-views only hidden hidden-languages]}]
  (let [parts (remove nil? [(when (seq active-views)
                              (str (if (= 1 (count active-views)) "the view " "the views ")
                                   (string/join ", " (sort active-views)) " on"))
                            (when (seq only) (str "only " (string/join ", " only) " shown"))
                            (when (seq hidden) (str (string/join ", " hidden) " hidden"))
                            (when (seq hidden-languages)
                              (str (string/join ", " (sort (map common/language-label hidden-languages))) " hidden"))])]
    (when (seq parts) (string/join "; " parts))))

(defn- cloud-view [{:keys [snapshot named-views focus open?]} arguments]
  (let [limit (common/count-argument arguments "limit" 200 1 1000)
        nodes (:namespaces snapshot)]
    (if-not focus
      {:text (str (if open? "The cloud has not reported its view yet" "No cloud is open in VS Code")
                  ", so no filter applies: all " (counted (count nodes) "page") " count. cloud_overview describes them.")}
      (let [filter-state (merge visibility/no-filter (:view-filter focus))
            shown (visibility/visible-ids snapshot (or named-views {}) filter-state)
            words (filter-words filter-state)
            folders (sort-by key (group-by #(page/directory (get nodes %)) (sort shown)))
            listed (reduce (fn [{:keys [rows room] :as so-far} [directory ids]]
                             (if (pos? room)
                               {:rows (conj rows (str "  " (common/folder-label directory) " (" (count ids) "): "
                                                      (common/some-of ids room)))
                                :room (- room (min room (count ids)))}
                               (update so-far :rows conj (str "  " (common/folder-label directory) " (" (count ids) ")"))))
                           {:rows [] :room limit}
                           folders)]
        {:text (string/join
                "\n"
                (concat
                 [(if words
                    (str "The cloud shows " (count shown) " of " (counted (count nodes) "page") ", with " words ".")
                    (str "Nothing is filtered out: all " (counted (count nodes) "page") " show."))]
                 (when (and (:snapshot-id focus) (not= :working-tree (:snapshot-id focus)))
                   [(str "The cloud shows " (or (:snapshot-label focus) (str "commit " (:snapshot-id focus)))
                         " rather than the working tree; these tools read the working tree.")])
                 (when (seq shown) (cons "Shown pages by folder:" (:rows listed)))))}))))

(def tool
  {:spec {"name" "cloud_view"
          "title" "What the person's view shows"
          "description" (str "The pages the person's filter and named views show in the cloud, by folder, so work can stay "
                             "in the part of the codebase they are working on.")
          "inputSchema" {"type" "object"
                         "properties" {"limit" {"type" "integer" "minimum" 1 "maximum" 1000 "default" 200
                                                "description" "Most pages to list"}}}
          "annotations" common/read-only}
   :answer cloud-view
   :touched (fn [_context _arguments] [{:doing "checking what the person's view shows"}])})

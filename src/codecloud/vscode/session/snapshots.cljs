(ns codecloud.vscode.session.snapshots
  "A session's reading of the codebase, the working tree analyzed as it
  changes and the commits of its history, with the messages asking for
  snapshots, sources, and what the cloud leaves out."
  (:require ["path" :as path]
            [clojure.string :as string]
            [codecloud.io.analyzer :as analyzer]
            [codecloud.io.files :as files]
            [codecloud.io.task :as task]
            [codecloud.model.config :as config]
            [codecloud.protocol :as protocol]
            [codecloud.vscode.agent-edits :as agent-edits]
            [codecloud.vscode.api :as api]
            [codecloud.vscode.clouds :as clouds]
            [codecloud.vscode.session.common :as common]
            [codecloud.vscode.settings :as settings]))

(defn views-message
  "The parts of a configuration the view shows, its named views and its
  color layers."
  [configuration]
  (protocol/message :host/views {:views (:views configuration)
                                 :color-layers (config/color-layer-specs configuration)}))

(declare analyze!)

(defn- unsaved-text-lookup
  "Function from a path relative to the cloud's root to the text of that
  file when it is open with unsaved changes, or nil."
  [{:keys [root-directory]}]
  (let [texts (into {} (map (fn [[file-path text]] [(clouds/comparison-path file-path) text])) (api/unsaved-texts))]
    (fn [relative-path]
      (get texts (clouds/comparison-path (path/join root-directory relative-path))))))

(defn- finish-analysis! [{:keys [state] :as session}]
  (swap! state #(assoc % :analyzing? false :analysis-milliseconds (- (js/Date.now) (:analysis-started %))))
  (common/post-status! session nil nil)
  (when (:analyze-again? @state)
    (swap! state assoc :analyze-again? false)
    (analyze! session)))

(defn analyze!
  "Analyzes the working tree and sends the snapshot when it is a new one.
  A request while an analysis runs queues one more, and a result read
  with a configuration replaced meanwhile is dropped for that one."
  [{:keys [analyzer state] :as session}]
  (cond
    (:disposed? @state) nil
    (:analyzing? @state) (swap! state assoc :analyze-again? true)
    :else
    (let [configuration (:configuration @state)]
      (swap! state assoc :analyzing? true :analysis-started (js/Date.now))
      (common/post-status! session "Reading the codebase" nil)
      (-> (analyzer/analyze-working-tree analyzer configuration
                                         {:on-progress (fn [{:keys [done total]}]
                                                         (common/post-status! session (str "Reading " done " of " total " files")
                                                                              (/ done (max 1 total))))
                                          :open-text (unsaved-text-lookup session)})
          (task/then (fn [{:keys [snapshot notices]}]
                       (when (identical? configuration (:configuration @state))
                         (when-not (identical? snapshot (:working-snapshot @state))
                           (swap! state assoc :working-snapshot snapshot)
                           (common/post! session (protocol/message :host/snapshot {:snapshot snapshot})))
                         (doseq [notice notices] (common/post-notice! session notice)))))
          (task/recover (fn [error]
                          (common/log! session (str "Analysis failed: " (task/error-text error)))
                          (common/post-notice! session (str "CodeCloud could not read this folder: " (task/error-text error)))))
          (task/then (fn [_] (finish-analysis! session)))))))

(defn load-history!
  "Reads the commits that changed the cloud's files and sends them to the
  view. Only the latest of overlapping reads counts."
  [{:keys [analyzer state] :as session}]
  (let [load-number (:history-loads (swap! state update :history-loads (fnil inc 0)))]
    (-> (analyzer/repository-root analyzer)
        (task/then (fn [repository-root]
                     (if repository-root
                       (task/then (analyzer/history analyzer (:configuration @state) (settings/history-limit))
                                  (fn [commits] {:commits commits :available? true}))
                       {:commits [] :available? false})))
        (task/then (fn [history]
                     (when (= load-number (:history-loads @state))
                       (swap! state assoc :history history)
                       (common/post! session (protocol/message :host/history history)))))
        (task/recover (fn [error] (common/log! session (str "Reading history failed: " (task/error-text error))))))))

(defn- watch-sources!
  "Reads the code again shortly after a source file or .gitignore changes,
  and tells codecloud.vscode.agent-edits of each source changed on disk."
  [{:keys [host root-directory state] :as session}]
  (api/dispose! (:source-watchers @state))
  (let [configuration (:configuration @state)
        wanted? (analyzer/wanted-path-predicate configuration)
        settle-milliseconds 400
        relative (fn [changed-path] (string/replace (path/relative root-directory changed-path) "\\" "/"))
        on-source (fn [changed-path]
                    (when (wanted? (relative changed-path))
                      (agent-edits/disk-changed! host changed-path)
                      (common/debounce! session :analyze settle-milliseconds #(analyze! session))))
        on-rules (fn [_] (common/debounce! session :analyze settle-milliseconds #(analyze! session)))]
    (swap! state assoc :source-watchers
           [(api/watch! root-directory "**/*" on-source)
            (api/watch! root-directory "**/.gitignore" on-rules)])))

(defn load-configuration!
  "Reads the cloud file and deps.edn, tells the view about named views and
  any problems, and analyzes when the configuration changed. Only the
  latest of overlapping reads counts."
  [{:keys [analyzer document state] :as session}]
  (let [load-number (:configuration-loads (swap! state update :configuration-loads (fnil inc 0)))]
    (task/then (analyzer/read-configuration analyzer (api/document-text document))
               (fn [{:keys [config problems]}]
                 (when (and (= load-number (:configuration-loads @state)) (not (:disposed? @state)))
                   (let [previous (:configuration @state)
                         config (assoc config :shown-ignored (:shown-ignored @state))]
                     (when (not= problems (:problems @state))
                       (doseq [problem problems] (common/post-notice! session problem)))
                     (swap! state assoc :problems problems)
                     (common/post! session (views-message config))
                     (when (not= config previous)
                       (swap! state assoc :configuration config)
                       (analyzer/forget-snapshots! analyzer)
                       (watch-sources! session)
                       (analyze! session)
                       (load-history! session))))))))

(defn show-ignored!
  "Reads the codebase again with `shown-paths`, the ignored folders and
  files the viewer chose to see, among the cloud's pages."
  [{:keys [analyzer state] :as session} shown-paths]
  (swap! state assoc :shown-ignored shown-paths)
  (when (:configuration @state)
    (swap! state update :configuration assoc :shown-ignored shown-paths)
    (analyzer/forget-snapshots! analyzer)
    (watch-sources! session)
    (analyze! session)
    (load-history! session)))

(defmethod common/handle-message :view/list-ignored [{:keys [analyzer state] :as session} {folder :path}]
  (when-let [configuration (:configuration @state)]
    (-> (analyzer/ignored-entries analyzer configuration (or folder ""))
        (task/then (fn [{:keys [entries more]}]
                     (common/post! session (protocol/message :host/ignored {:path folder :entries entries :more more}))))
        (task/recover (fn [error]
                        (common/log! session (str "Listing what the cloud leaves out failed: " (task/error-text error))))))))

(defn- snapshot-task [{:keys [analyzer state]} snapshot-id]
  (if (= :working-tree snapshot-id)
    (task/resolved (:working-snapshot @state))
    (if-let [remembered (analyzer/remembered-snapshot analyzer snapshot-id)]
      (task/resolved remembered)
      (let [commit (or (some #(when (= snapshot-id (:sha %)) %) (get-in @state [:history :commits]))
                       {:sha snapshot-id})]
        (task/then (analyzer/analyze-commit analyzer (:configuration @state) commit) :snapshot)))))

(defmethod common/handle-message :view/select-snapshot [session {:keys [snapshot-id]}]
  (when-not (= :working-tree snapshot-id)
    (let [sha (str snapshot-id)
          short-sha-length 7]
      (common/post-status! session (str "Reading commit " (subs sha 0 (min short-sha-length (count sha)))) nil)))
  (-> (snapshot-task session snapshot-id)
      (task/then (fn [snapshot]
                   (when snapshot
                     (common/post! session (protocol/message :host/snapshot {:snapshot snapshot})))))
      (task/recover (fn [error]
                      (common/post-notice! session (str "CodeCloud could not read that commit: " (task/error-text error)))))
      (task/then (fn [_] (when-not (= :working-tree snapshot-id) (common/post-status! session nil nil))))))

(defmethod common/handle-message :view/request-sources [{:keys [analyzer host root-directory state] :as session} {:keys [snapshot-id namespaces]}]
  (-> (snapshot-task session snapshot-id)
      (task/then (fn [snapshot]
                   (when snapshot
                     (let [open-text (unsaved-text-lookup session)]
                       ;; Text read now may be newer than the snapshot, so
                       ;; each entry carries the key of the text it holds.
                       (task/all (for [{:keys [namespace-id]} namespaces]
                                   (task/then (analyzer/namespace-sources analyzer (:configuration @state) snapshot
                                                                          namespace-id open-text)
                                              (fn [files]
                                                {:namespace-id namespace-id
                                                 :source-key [namespace-id (mapv :content-key files)]
                                                 :files (mapv #(dissoc % :content-key) files)}))))))))
      (task/then (fn [entries]
                   (when (= :working-tree snapshot-id)
                     (agent-edits/remember-texts! host (for [{source-files :files} entries
                                                             {source-path :path :keys [text]} source-files]
                                                         [(files/absolute-path root-directory source-path) text])))
                   (when (seq entries)
                     (common/post! session (protocol/message :host/sources {:snapshot-id snapshot-id :entries entries})))))
      (task/recover (fn [error] (common/log! session (str "Reading sources failed: " (task/error-text error)))))))

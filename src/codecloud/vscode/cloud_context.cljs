(ns codecloud.vscode.cloud-context
  "What the agent hub's cloud tools read in VS Code, which is the open
  cloud the person used last, with its snapshot, named views, and the
  focus its view reports, or with no cloud open the workspace read in the
  background, and the person's caret. The context is the one
  codecloud.agents.cloud-tools/answer takes, with :read-text resolving a
  path of the cloud to its text."
  (:require ["path" :as path]
            [clojure.string :as string]
            [codecloud.io.analyzer :as analyzer]
            [codecloud.io.files :as files]
            [codecloud.io.task :as task]
            [codecloud.model.config :as config]
            [codecloud.model.workspace :as workspace]
            [codecloud.vscode.api :as api]
            [codecloud.vscode.clouds :as clouds]
            [codecloud.vscode.session.common :as session-common]))

(defn create
  "The background reading of a host, before any."
  []
  (atom nil))

(defn- cloud-name [cloud-file]
  (workspace/cloud-name (string/replace cloud-file "\\" "/") config/cloud-file-name))

(defn- read-snapshot
  "Resolves to `open-session`'s working-tree snapshot once its first
  reading of the codebase finishes, or rejects after a while saying it
  still reads."
  [{:keys [state]}]
  (let [patience-milliseconds 60000]
    (if-let [snapshot (:working-snapshot @state)]
      (task/resolved snapshot)
      (js/Promise.
       (fn [resolve reject]
         (let [watch-key (gensym "agent-snapshot")
               timer (js/setTimeout (fn []
                                      (remove-watch state watch-key)
                                      (reject (js/Error. "The cloud is still reading the codebase. Ask again in a moment.")))
                                    patience-milliseconds)]
           (add-watch state watch-key
                      (fn [_key _reference _previous current]
                        (when-let [snapshot (:working-snapshot current)]
                          (js/clearTimeout timer)
                          (remove-watch state watch-key)
                          (resolve snapshot))))))))))

(defn- session-context [{:keys [carets]} {:keys [cloud-file root-directory state] :as open-session}]
  (task/then (read-snapshot open-session)
             (fn [snapshot]
               (let [{:keys [configuration focus visible?]} @state]
                 {:snapshot snapshot
                  :cloud-name (cloud-name cloud-file)
                  :root root-directory
                  :workspace-roots (api/workspace-folder-paths)
                  :named-views (or (:views configuration) {})
                  :focus focus
                  :open? true
                  :visible? visible?
                  :carets @carets
                  :read-text (session-common/text-reader open-session)}))))

(defn- unsaved-text-lookup [root]
  (let [texts (into {} (map (fn [[file-path text]] [(clouds/comparison-path file-path) text])) (api/unsaved-texts))]
    (fn [relative-path]
      (get texts (clouds/comparison-path (files/absolute-path root relative-path))))))

(defn- background-reading!
  "Resolves to {:snapshot :configuration} for the cloud at `root`. A
  reading a few seconds old serves again, and calls during a reading
  share it."
  [{:keys [background git-executable extension-uri]} root cloud-file]
  (let [fresh-milliseconds 10000
        now (js/Date.now)
        {:keys [reading started] :as current} (when (= root (:root @background)) @background)]
    (if (and reading (< (- now started) fresh-milliseconds))
      reading
      (let [reader (or (:analyzer current)
                       (analyzer/create-analyzer {:root-directory root
                                                  :git-executable @git-executable
                                                  :grammar-directory (api/uri-path (api/child-uri extension-uri "dist"))}))
            reading (-> (if cloud-file (files/read-text cloud-file) (task/resolved nil))
                        (task/then #(analyzer/read-configuration reader %))
                        (task/then (fn [{:keys [config]}]
                                     (task/then (analyzer/analyze-working-tree reader config {:open-text (unsaved-text-lookup root)})
                                                (fn [{:keys [snapshot]}] {:snapshot snapshot :configuration config}))))
                        (task/recover (fn [error]
                                        (swap! background #(when (= root (:root %)) (dissoc % :reading)))
                                        (task/rejected error))))]
        (reset! background {:root root :analyzer reader :reading reading :started now})
        reading))))

(defn- background-context
  "Resolves to the context of the cloud covering the first workspace
  folder, or the workspace's first cloud, or the folder itself, read in
  the background; nil with no folder open."
  [{:keys [index carets] :as host}]
  (if-let [workspace-root (first (api/workspace-folder-paths))]
    (let [cloud-file (or (clouds/cloud-file-for index (path/join workspace-root config/cloud-file-name))
                         (first @(:paths index)))
          root (if cloud-file (path/dirname cloud-file) workspace-root)]
      (task/then (background-reading! host root cloud-file)
                 (fn [{:keys [snapshot configuration]}]
                   {:snapshot snapshot
                    :cloud-name (if cloud-file (cloud-name cloud-file) (path/basename root))
                    :root root
                    :workspace-roots (api/workspace-folder-paths)
                    :named-views (or (:views configuration) {})
                    :focus nil
                    :open? false
                    :visible? false
                    :carets @carets
                    :read-text (session-common/text-reader {:root-directory root})})))
    (task/resolved nil)))

(defn current
  "Resolves to the context the hub's cloud tools answer from, that of the
  open cloud the person used last, or another open cloud, or else the
  workspace read in the background, and nil with no folder open."
  [{:keys [sessions active-session] :as host}]
  (if-let [open-session (or @active-session (first (mapcat val @sessions)))]
    (session-context host open-session)
    (background-context host)))

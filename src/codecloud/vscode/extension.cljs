(ns codecloud.vscode.extension
  "Entry point of the VS Code extension: the cloud editor for *.cld files
  and CodeCloud's commands."
  (:require ["fs" :as fs]
            ["path" :as path]
            [clojure.string :as string]
            [codecloud.io.task :as task]
            [codecloud.model.selection :as selection]
            [codecloud.presentation.visibility :as visibility]
            [codecloud.vscode.api :as api]
            [codecloud.vscode.clouds :as clouds]
            [codecloud.vscode.editor-zoom :as editor-zoom]
            [codecloud.vscode.session :as session]))

(defonce ^:private running-host (atom nil))

(defn- create-host [^js context]
  (let [channel (api/output-channel "CodeCloud")]
    {:extension-uri (.-extensionUri context)
     :global-state (.-globalState context)
     :workspace-state (.-workspaceState context)
     :channel channel
     :log! (fn [text] (api/append-line! channel (str (.toLocaleTimeString (js/Date.) "en-US") "  " text)))
     :index (clouds/create-index!)
     :sessions (atom {})
     :pending-arrivals (atom {})
     :analyzers (atom {})
     :active-session (atom nil)
     :zoom-steps (atom 0)
     :git (atom nil)
     :git-executable (atom "git")
     :deliver-arrival! session/deliver-arrival!}))

(defn- all-sessions [host]
  (mapcat val @(:sessions host)))

(defn- argument-path [^js uri]
  (when (and uri (.-fsPath uri)) (api/uri-path uri)))

(defn- cloud-file-of
  "The cloud file a command is about, which is the one it was invoked on,
  the active cloud, or the one covering the active editor."
  [host ^js uri]
  (let [chosen (argument-path uri)]
    (or (when (and chosen (string/ends-with? (string/lower-case chosen) ".cld")) chosen)
        (:cloud-file @(:active-session host))
        (some->> (api/active-editor-view) :path (clouds/cloud-file-for (:index host))))))

(defn- edit-cloud-file! [host uri]
  (clouds/after-first-search
   (:index host)
   (fn []
     (if-let [cloud-file (cloud-file-of host uri)]
       (api/open-as-text! (api/file-uri cloud-file))
       (api/info-message "Open a cloud first, or choose a .cld file in the explorer.")))))

(defn- create-cloud-file! [host uri]
  (let [chosen (argument-path uri)]
    (task/then (if chosen
                 (task/then (.stat (.-promises fs) chosen)
                            (fn [^js stats] (if (.isDirectory stats) chosen (path/dirname chosen))))
                 (api/pick-folder (first (api/workspace-folder-paths)) "Create Cloud Here"))
               (fn [directory]
                 (when directory (clouds/create-and-open! host directory nil nil))))))

(defn- refresh! [host]
  (if-let [active @(:active-session host)]
    (session/refresh! active)
    (doseq [open-session (all-sessions host)] (session/refresh! open-session))))

(defn- selection-patterns
  "Patterns naming the chosen explorer entries, whole folders or single
  files, relative to `cloud-file`'s folder."
  [cloud-file chosen-paths]
  (let [root (path/dirname cloud-file)]
    (task/all (for [chosen chosen-paths]
                (task/then (.stat (.-promises fs) chosen)
                           (fn [^js stats]
                             (let [relative (string/replace (path/relative root chosen) "\\" "/")]
                               (cond
                                 (string/starts-with? relative "..") nil
                                 (.isDirectory stats) (selection/directory-pattern relative)
                                 :else (selection/file-pattern relative)))))))))

(defn- apply-explorer-filter!
  "Changes the remembered filter of the cloud covering `chosen-paths`
  with `change-filter`, then shows that cloud."
  [host chosen-paths change-filter]
  (if-let [cloud-file (some #(clouds/cloud-file-for (:index host) %) chosen-paths)]
    (task/then (selection-patterns cloud-file chosen-paths)
               (fn [patterns]
                 (let [memory-key (session/preference-key cloud-file :view-filter)
                       current (merge visibility/no-filter (api/remembered (:workspace-state host) memory-key))]
                   (task/then (api/remember! (:workspace-state host) memory-key
                                             (change-filter current (remove nil? patterns)))
                              (fn [_]
                                (doseq [open-session (get @(:sessions host) (clouds/comparison-path cloud-file))]
                                  (session/resend-init! open-session))
                                (clouds/open-cloud! host cloud-file nil nil))))))
    (api/info-message "No cloud covers these files yet. Create one with CodeCloud: Enter Cloud View.")))

(defn- filter-from-explorer!
  "Applies explorer entries to the cloud covering them, as the only things
  shown or as hidden ones, then shows the cloud."
  [host ^js clicked ^js selected change-filter]
  (let [chosen-paths (vec (or (some->> selected array-seq (keep argument-path) seq)
                              (some-> (argument-path clicked) vector)))]
    (clouds/after-first-search (:index host) #(apply-explorer-filter! host chosen-paths change-filter))))

(defn- commands [host]
  {"codecloud.enter" (fn [& _] (editor-zoom/enter-cloud! host 1))
   "codecloud.zoomOut" (fn [& _] (editor-zoom/zoom-out! host))
   "codecloud.zoomIn" (fn [& _] (editor-zoom/zoom-in! host))
   "codecloud.createCloudFile" (fn [& [uri]] (create-cloud-file! host uri))
   "codecloud.editCloudFile" (fn [& [uri]] (edit-cloud-file! host uri))
   "codecloud.refresh" (fn [& _] (refresh! host))
   "codecloud.showOnlyInCloud" (fn [& [clicked selected]]
                                 (filter-from-explorer! host clicked selected (fn [_current patterns] (visibility/show-only patterns))))
   "codecloud.hideInCloud" (fn [& [clicked selected]]
                             (filter-from-explorer! host clicked selected visibility/hide))
   "codecloud.showEverything" (fn [& _]
                                (doseq [open-session (all-sessions host)]
                                  (task/then (api/remember! (:workspace-state host)
                                                            (session/preference-key (:cloud-file open-session) :view-filter)
                                                            visibility/no-filter)
                                             (fn [_] (session/resend-init! open-session)))))})

(defn- with-time-limit
  "Resolves like `promise`, or to nil after `milliseconds`."
  [promise milliseconds]
  (js/Promise.race #js [promise (js/Promise. (fn [resolve _] (js/setTimeout #(resolve nil) milliseconds)))]))

(defn activate
  "Called when CodeCloud is first needed. Finds the git extension's git
  before any cloud opens, so history works when git is off the PATH."
  [^js context]
  (let [host (create-host context)
        push! (fn [item] (.push (.-subscriptions context) item))
        git-wait-milliseconds 2500]
    (reset! running-host host)
    (push! (:channel host))
    (push! (:disposable (:index host)))
    (api/set-context! "codecloud.zoomedOut" false)
    (let [follow-active-file! (fn []
                                (api/set-context! "codecloud.inCloud"
                                                  (boolean (some->> (api/active-editor-view) :path
                                                                    (clouds/cloud-file-for (:index host))))))]
      (push! (api/on-active-editor-change! follow-active-file!))
      (add-watch (:paths (:index host)) ::in-cloud (fn [_ _ _ _] (follow-active-file!)))
      (follow-active-file!))
    (-> (with-time-limit (api/git-api) git-wait-milliseconds)
        (task/recover (fn [_error] nil))
        (task/then (fn [git]
                     (when git
                       (reset! (:git host) git)
                       (reset! (:git-executable host) (api/git-path git)))
                     (push! (api/register-custom-editor! clouds/view-type
                                                         (fn [document panel] (session/start! host document panel))))
                     (doseq [[command-id handler] (commands host)]
                       (push! (api/register-command! command-id handler)))
                     ((:log! host) (str "CodeCloud is ready; git is " @(:git-executable host) "."))
                     nil)))))

(defn deactivate []
  (when-let [host @running-host]
    (doseq [open-session (all-sessions host)] (session/stop! open-session))
    (reset! running-host nil)))

(set! (.-exports js/module) #js {:activate activate :deactivate deactivate})

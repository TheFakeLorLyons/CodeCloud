(ns codecloud.vscode.extension
  "Entry point of the VS Code extension, registering the cloud editor for
  *.cld files and CodeCloud's commands."
  (:require ["fs" :as fs]
            ["path" :as path]
            [clojure.string :as string]
            [codecloud.io.task :as task]
            [codecloud.model.selection :as selection]
            [codecloud.presentation.visibility :as visibility]
            [codecloud.presentation.windows-state :as windows-state]
            [codecloud.protocol :as protocol]
            [codecloud.vscode.agent-edits :as agent-edits]
            [codecloud.vscode.agents :as agents]
            [codecloud.vscode.api :as api]
            [codecloud.vscode.assistant-host :as assistant-host]
            [codecloud.vscode.cloud-context :as cloud-context]
            [codecloud.vscode.clouds :as clouds]
            [codecloud.vscode.editor-evaluation :as editor-evaluation]
            [codecloud.vscode.editor-zoom :as editor-zoom]
            [codecloud.vscode.session :as session]
            [codecloud.vscode.session.command-messages :as command-messages]
            [codecloud.vscode.session.common :as common]
            [codecloud.vscode.session.editing-messages :as editing-messages]
            [codecloud.vscode.session.repl-messages :as repl-messages]
            [codecloud.vscode.session.window-messages :as window-messages]))

(defonce ^:private running-host (atom nil))

(defn- create-host [^js context]
  (let [channel (api/output-channel "CodeCloud")]
    {:extension-uri (.-extensionUri context)
     :version (some-> context .-extension .-packageJSON .-version)
     :global-state (.-globalState context)
     :workspace-state (.-workspaceState context)
     :secrets (.-secrets context)
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
     :agents (agents/create)
     :agent-summary (atom {:serving? false :url nil :agents [] :problem nil})
     :presence-summary (atom {:agents [] :pins []})
     :assistant-settings (assistant-host/create (.-globalState context))
     :agent-edits (agent-edits/create)
     :carets (atom {:latest nil :by-file {}})
     :background (cloud-context/create)
     :cloud-context cloud-context/current
     :editor-results (editor-evaluation/create)
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

(defn- session-showing [host cloud-file]
  (let [candidates (get @(:sessions host) (clouds/comparison-path cloud-file))
        active @(:active-session host)]
    (if (contains? candidates active) active (first candidates))))

(defn- evaluate-in-editor!
  "Evaluates `scope` from the active editor through the REPL of the cloud
  holding its file, offering to open that cloud when it is closed."
  [host scope]
  (let [{:keys [notice path] :as request} (editor-evaluation/request scope)
        cloud-file (when path (clouds/cloud-file-for (:index host) path))]
    (cond
      notice (api/info-message notice)
      (nil? cloud-file) (api/info-message "This file lies in no cloud, so CodeCloud has no REPL for it.")
      :else (if-let [open-session (session-showing host cloud-file)]
              (repl-messages/evaluate-from-editor! open-session (editor-evaluation/remember! (:editor-results host) request))
              (task/then (api/info-message "Open this file's cloud to evaluate through its REPL." "Open Cloud")
                         (fn [choice]
                           (when choice (clouds/open-cloud! host cloud-file nil nil))))))))

(defn- interrupt-evaluations!
  "Stops the evaluations of the REPL tabs of the open cloud holding the
  active editor's file."
  [host]
  (some->> (api/active-editor-view)
           :path
           (clouds/cloud-file-for (:index host))
           (session-showing host)
           repl-messages/interrupt-evaluations!))

(defn- open-file-in-cloud!
  "Shows a file in a window of the open cloud holding it, offering to open
  that cloud when it is closed. Returns nil without waiting on the
  notice it shows, since a notice stays open until dismissed and a page
  command running this one waits for it to finish."
  [host file-path]
  (let [cloud-file (when file-path (clouds/cloud-file-for (:index host) file-path))
        open-session (or (when cloud-file (session-showing host cloud-file)) @(:active-session host))
        window (when (and open-session file-path) (window-messages/file-window open-session file-path))]
    (cond
      (nil? file-path) (api/info-message "Choose a page, document, or image to show in the cloud.")
      (nil? (windows-state/file-kind file-path)) (api/info-message "CodeCloud's windows show web pages, Markdown, PDFs, and images.")
      window (window-messages/open-window! open-session window)
      cloud-file (task/then (api/info-message "Open this file's cloud to show the file in it." "Open Cloud")
                            (fn [choice] (when choice (clouds/open-cloud! host cloud-file nil nil))))
      :else (api/info-message "This file lies in no open cloud's workspace folder."))
    nil))

(defn- show-web-page!
  "Asks for a web address and shows it in a window of the cloud in front."
  [host]
  (if-let [open-session (or @(:active-session host) (first (all-sessions host)))]
    (task/then (api/input-box {:title "Show a Web Page in the Cloud"
                               :prompt "A port on this computer, such as 1971, or a web address"
                               :placeholder "1971"
                               :validate (fn [text] (when-not (windows-state/web-address text) "Enter a port or a web address"))})
               (fn [text]
                 (when-let [address (windows-state/web-address text)]
                   (window-messages/open-window! open-session {:kind :web :source address}))))
    (api/info-message "Open a cloud first, then show a web page in it.")))

(defn- run-command-for-page!
  "Offers the commands extensions contribute and runs the one picked for
  the page in hand in the cloud in front, or for the file in the editor
  when no cloud is in front."
  [host]
  (let [open-session @(:active-session host)]
    (if (and open-session (api/panel-active? (:panel open-session)))
      (common/post! open-session (protocol/message :host/pick-page-command {}))
      (command-messages/pick-for-editor!))))

(defn- commands [host]
  {"codecloud.enter" (fn [& _] (editor-zoom/enter-cloud! host 1))
   "codecloud.evaluateForm" (fn [& _] (evaluate-in-editor! host :form))
   "codecloud.evaluateEnclosingForm" (fn [& _] (evaluate-in-editor! host :enclosing))
   "codecloud.evaluateTopLevelForm" (fn [& _] (evaluate-in-editor! host :top-level))
   "codecloud.evaluateToCaret" (fn [& _] (evaluate-in-editor! host :to-caret))
   "codecloud.evaluateTopLevelFormToCaret" (fn [& _] (evaluate-in-editor! host :top-level-to-caret))
   "codecloud.evaluateFileToCaret" (fn [& _] (evaluate-in-editor! host :file-to-caret))
   "codecloud.loadFile" (fn [& _] (evaluate-in-editor! host :file))
   "codecloud.loadFileWithRequires" (fn [& _] (evaluate-in-editor! host :file-with-requires))
   "codecloud.interruptEvaluations" (fn [& _] (interrupt-evaluations! host))
   "codecloud.clearInlineResults" (fn [& _] (editor-evaluation/clear! (:editor-results host)))
   "codecloud.zoomOut" (fn [& _] (editor-zoom/zoom-out! host))
   "codecloud.zoomIn" (fn [& _] (editor-zoom/zoom-in! host))
   "codecloud.createCloudFile" (fn [& [uri]] (create-cloud-file! host uri))
   "codecloud.editCloudFile" (fn [& [uri]] (edit-cloud-file! host uri))
   "codecloud.refresh" (fn [& _] (refresh! host))
   "codecloud.connectAgent" (fn [& _] (agents/connect-agent! host))
   "codecloud.openInCloud" (fn [& [uri]] (open-file-in-cloud! host (or (some-> uri api/uri-path) (:path (api/active-editor-view)))))
   "codecloud.showWebPage" (fn [& _] (show-web-page! host))
   "codecloud.runCommandForPage" (fn [& _] (run-command-for-page! host))
   "codecloud.chooseAssistantModel" (fn [& _] (assistant-host/choose-model! host))
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

(defn- with-time-limit [pending milliseconds]
  (js/Promise.race #js [pending (js/Promise. (fn [resolve _] (js/setTimeout #(resolve nil) milliseconds)))]))

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
                     (doseq [item (agents/activate! host)]
                       (push! item))
                     (push! (agents/watch-carets! host))
                     (doseq [item (agent-edits/activate! host (fn [file-path]
                                                                (some #(editing-messages/applying-edit? % file-path) (all-sessions host))))]
                       (push! item))
                     (doseq [item (editor-evaluation/activate! (:editor-results host))]
                       (push! item))
                     ((:log! host) (str "CodeCloud is ready; git is " @(:git-executable host) "."))
                     nil)))))

(defn deactivate
  "Ends every session and stops the agent hub; VS Code waits for the
  returned promise."
  []
  (when-let [host @running-host]
    (doseq [open-session (all-sessions host)] (session/stop! open-session))
    (reset! running-host nil)
    (agents/stop! host)))

(set! (.-exports js/module) #js {:activate activate :deactivate deactivate})

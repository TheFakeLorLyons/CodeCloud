(ns codecloud.vscode.session
  "One open cloud, its panel, and its lifetime. Panels on one cloud file
  share an analyzer. The namespaces under this one handle the view's
  messages by feature, each adding its methods to
  codecloud.vscode.session.common/handle-message as it loads."
  (:require ["path" :as path]
            [clojure.string :as string]
            [codecloud.io.analyzer :as analyzer]
            [codecloud.io.task :as task]
            [codecloud.model.config :as config]
            [codecloud.model.source :as source]
            [codecloud.model.workspace :as workspace]
            [codecloud.protocol :as protocol]
            [codecloud.vscode.api :as api]
            [codecloud.vscode.assistant-host :as assistant-host]
            [codecloud.vscode.calva :as calva]
            [codecloud.vscode.clouds :as clouds]
            [codecloud.vscode.editor-theme :as editor-theme]
            [codecloud.vscode.editor-zoom :as editor-zoom]
            [codecloud.vscode.repl-host :as repl-host]
            [codecloud.vscode.session.agent-messages :as agent-messages]
            [codecloud.vscode.session.common :as common]
            [codecloud.vscode.session.editing-messages :as editing-messages]
            [codecloud.vscode.session.repl-messages :as repl-messages]
            [codecloud.vscode.session.snapshots :as snapshots]
            [codecloud.vscode.settings :as settings]
            [codecloud.vscode.shells :as shells]
            [codecloud.vscode.webview :as webview]))

(defn preference-key [cloud-file preference]
  (str "codecloud." (name preference) ":" (clouds/comparison-path cloud-file)))

(def ^:private cloud-preferences
  "Preferences remembered for each cloud file, since each codebase may be
  best seen its own way."
  [:view-filter :arrangement :header-mode :color-layers :color-overrides :spread])

(def ^:private viewer-preferences
  "Preferences remembered for every cloud, as how the viewer likes to work."
  [:display-mode :totals-open? :tint-clouds? :inspector-minimized? :repl-geometry :dock])

(defn- viewer-preference-key [preference]
  (str "codecloud." (name preference)))

(defn- init-message [{:keys [host cloud-file]}]
  (let [described-spaces (settings/spaces)]
    (protocol/message :host/init
                      (into {:settings {:zoom-out-threshold (settings/zoom-out-threshold)
                                        :editor-colors? (settings/editor-colors?)
                                        ;; Brackets come in pairs in the cloud as in
                                        ;; VS Code's Clojure editors, Calva's settings included.
                                        :pairs-brackets? (not= "never" (api/editor-setting "clojure" "autoClosingBrackets"
                                                                                           "languageDefined"))}
                             :theme-kind (api/theme-kind)
                             :sky-mode (settings/sky-mode described-spaces)
                             :spaces described-spaces
                             :display-mode (or (api/remembered (:global-state host) (viewer-preference-key :display-mode)) :code)
                             :totals-open? (api/remembered (:global-state host) (viewer-preference-key :totals-open?))
                             :tint-clouds? (api/remembered (:global-state host) (viewer-preference-key :tint-clouds?))
                             :inspector-minimized? (api/remembered (:global-state host) (viewer-preference-key :inspector-minimized?))
                             :repl-geometry (api/remembered (:global-state host) (viewer-preference-key :repl-geometry))
                             :dock (api/remembered (:global-state host) (viewer-preference-key :dock))
                             :cloud-name (workspace/cloud-name (string/replace cloud-file "\\" "/") config/cloud-file-name)
                             :editor-font (api/editor-font (or (api/editor-language-id) "clojure"))}
                            (map (fn [preference]
                                   [preference (api/remembered (:workspace-state host) (preference-key cloud-file preference))]))
                            cloud-preferences))))

;; Editor colors

(defn- send-code-colors!
  "Reads the code colors of the editor's theme and sends them to the
  view. Only the latest of overlapping reads counts."
  [{:keys [state] :as session}]
  (let [read-number (:code-color-reads (swap! state update :code-color-reads (fnil inc 0)))]
    (task/then (editor-theme/read-code-colors (or (api/editor-language-id) "clojure"))
               (fn [code-colors]
                 (when (= read-number (:code-color-reads @state))
                   (common/post! session (protocol/message :host/editor-colors {:code-colors code-colors})))))))

(defn resend-init!
  "Sends settings and preferences again, for example after a command
  changed the view filter."
  [session]
  (when (:ready? @(:state session))
    (common/post! session (init-message session))))

(defn deliver-arrival!
  "Starts the view at a line the viewer zoomed out of, now or once the view
  is ready."
  [{:keys [state] :as session} arrival]
  (if (:ready? @state)
    (common/post! session (protocol/message :host/arrive arrival))
    (swap! state assoc :pending-arrival arrival)))

(defn- cloud-problems
  "The problems VS Code knows of in the files below the cloud's folder,
  by path relative to it."
  [{:keys [root-directory]}]
  (into {}
        (keep (fn [[file-path problems]]
                (let [relative (string/replace (path/relative root-directory file-path) "\\" "/")]
                  (when-not (or (string/starts-with? relative "..") (path/isAbsolute relative))
                    [relative problems]))))
        (api/diagnostics)))

(defn- post-problems!
  "Sends the view the problems in the cloud's files when they differ from
  those it has."
  [{:keys [state] :as session}]
  (let [problems (cloud-problems session)]
    (when (and (:ready? @state) (not= problems (:posted-problems @state)))
      (swap! state assoc :posted-problems problems)
      (common/post! session (protocol/message :host/diagnostics {:files problems})))))

;; Messages from the view

(defmethod common/handle-message :view/ready [{:keys [state host] :as session} _message]
  (swap! state assoc :ready? true)
  (common/post! session (init-message session))
  (send-code-colors! session)
  (doseq [tab (keys @(:repls session))]
    (repl-host/announce! (repl-messages/repl-context session tab)))
  (doseq [tab (keys @(:calva-tabs session))]
    (repl-messages/with-calva-tab session tab calva/announce!))
  (shells/announce! (repl-messages/shell-context session))
  (common/post! session (protocol/message :host/agents @(:agent-summary host)))
  (common/post! session (agent-messages/presence-message session @(:presence-summary host)))
  (assistant-host/announce! (agent-messages/assistant-context session))
  (if-let [configuration (:configuration @state)]
    (do (common/post! session (snapshots/views-message configuration))
        (when-let [snapshot (:working-snapshot @state)]
          (common/post! session (protocol/message :host/snapshot {:snapshot snapshot})))
        (when-let [history (:history @state)]
          (common/post! session (protocol/message :host/history history))))
    (snapshots/load-configuration! session))
  (when-let [arrival (:pending-arrival @state)]
    (swap! state dissoc :pending-arrival)
    (common/post! session (protocol/message :host/arrive arrival)))
  (swap! state dissoc :posted-problems)
  (post-problems! session))

(defmethod common/handle-message :view/open-location [{:keys [root-directory panel] :as session} {:keys [path line column]}]
  (-> (api/show-text-document! (path/join root-directory path) (or line 1) (or column 1) (api/panel-view-column panel))
      (task/then (fn [_] (editor-zoom/hint-way-back!)))
      (task/recover (fn [error] (common/post-notice! session (str "CodeCloud could not open " path ": " (task/error-text error)))))))

(defmethod common/handle-message :view/save-preference [{:keys [host cloud-file] :as session} {preference :key :keys [value]}]
  (cond
    (= :sky-mode preference) (settings/save-sky-mode! value)
    (= :editor-colors? preference) (settings/save-editor-colors! value)
    (some #{preference} viewer-preferences) (api/remember! (:global-state host) (viewer-preference-key preference) value)
    (some #{preference} cloud-preferences) (api/remember! (:workspace-state host) (preference-key cloud-file preference) value)
    :else (common/log! session (str "Ignored the preference " preference))))

(defmethod common/handle-message :view/save-view [{:keys [document] :as session} {view-name :name :keys [selection]}]
  (let [text (source/normalize-text (api/document-text document))
        {:keys [line column error] :as insertion} (config/view-insertion text view-name selection)]
    (if error
      (common/post-notice! session error)
      (-> (api/insert-and-save! document line column
                                (cond-> (:text insertion)
                                  (api/crlf-document? document) (string/replace "\n" "\r\n")))
          (task/then (fn [saved?]
                       (common/post-notice! session (if saved?
                                               (str "Saved the view " (pr-str view-name) " in the cloud file.")
                                               "VS Code did not save the view."))))
          (task/recover (fn [error] (common/post-notice! session (str "Saving the view failed: " (task/error-text error)))))))))

(defmethod common/handle-message :view/log [session {:keys [message]}]
  (common/log! session message))

;; Lifetime

(defn- follow-problems!
  "Sends the view the problems in the cloud's files as linters and
  compilers report them, gathering reports that come together. Returns
  the disposable that stops following."
  [session]
  (let [settle-milliseconds 250]
    (api/on-diagnostics-change! #(common/debounce! session :problems settle-milliseconds (fn [] (post-problems! session))))))

(defn- analyzer-for
  "The analyzer of a cloud file, shared by every panel showing it, reading
  C and C++ with the grammars shipped beside the extension's bundle."
  [{:keys [analyzers git-executable extension-uri]} cloud-file root-directory]
  (let [cloud-key (clouds/comparison-path cloud-file)]
    (or (get @analyzers cloud-key)
        (let [created (analyzer/create-analyzer {:root-directory root-directory
                                                 :git-executable @git-executable
                                                 :grammar-directory (api/uri-path (api/child-uri extension-uri "dist"))})]
          (swap! analyzers assoc cloud-key created)
          created))))

(defn- register! [{:keys [host cloud-file] :as session}]
  (let [cloud-key (clouds/comparison-path cloud-file)]
    (swap! (:sessions host) update cloud-key (fnil conj #{}) session)
    (when-let [arrival (get @(:pending-arrivals host) cloud-key)]
      (swap! (:pending-arrivals host) dissoc cloud-key)
      (deliver-arrival! session arrival))))

(defn- unregister!
  "Forgets a closed session. The last session of a cloud takes the cloud's
  analyzer and its caches with it."
  [{:keys [host cloud-file] :as session}]
  (let [cloud-key (clouds/comparison-path cloud-file)
        remaining (disj (get @(:sessions host) cloud-key #{}) session)]
    (if (seq remaining)
      (swap! (:sessions host) assoc cloud-key remaining)
      (do (swap! (:sessions host) dissoc cloud-key)
          (swap! (:analyzers host) dissoc cloud-key)))
    (when (= session @(:active-session host))
      (reset! (:active-session host) nil))))

(defn stop!
  "Ends a session when its panel closes, with its REPL connections, and
  lets go of its shells' terminals."
  [{:keys [state disposables] :as session}]
  (swap! state assoc :disposed? true)
  (doseq [[_ timer] (:timers @state)] (js/clearTimeout timer))
  (api/dispose! (:source-watchers @state))
  (api/dispose! @disposables)
  (doseq [tab (keys @(:repls session))]
    (repl-host/close! (repl-messages/repl-context session tab)))
  (doseq [tab (keys @(:calva-tabs session))]
    (repl-messages/with-calva-tab session tab calva/close!))
  (shells/release-all! (repl-messages/shell-context session))
  (assistant-host/close! (agent-messages/assistant-context session))
  (unregister! session))

(defn- same-path? [first-path second-path]
  (= (clouds/comparison-path first-path) (clouds/comparison-path second-path)))

(defn start!
  "Starts a session for a cloud file shown in `panel`, a webview panel VS
  Code created for the custom editor."
  [host document panel]
  (let [cloud-file (api/document-path document)
        root-directory (path/dirname cloud-file)
        session {:host host
                 :document document
                 :panel panel
                 :cloud-file cloud-file
                 :root-directory root-directory
                 :analyzer (analyzer-for host cloud-file root-directory)
                 :state (atom {:ready? false :timers {} :visible? true})
                 :repls (atom {})
                 :calva-tabs (atom {})
                 :shells (shells/create)
                 :chat (assistant-host/create-chat)
                 :disposables (atom [])}
        add! (fn [item] (swap! (:disposables session) conj item))
        settle-milliseconds 300]
    (webview/prepare! panel (:extension-uri host))
    (add! (api/on-panel-message! panel (fn [text]
                                         (when-let [message (protocol/decode text)]
                                           (common/handle-message session message)))))
    (add! (api/on-panel-dispose! panel #(stop! session)))
    (add! (agent-messages/follow-agents! session))
    (add! (agent-messages/follow-presence! session))
    (add! (agent-messages/follow-assistant! session))
    (add! (follow-problems! session))
    (doseq [item (shells/watch! (repl-messages/shell-context session))]
      (add! item))
    (add! (api/on-panel-view-state! panel (fn [changed-panel]
                                            (when (api/panel-active? changed-panel)
                                              (reset! (:active-session host) session))
                                            ;; Coming back into view after the cloud opened a
                                            ;; file, the view draws back from that file's page.
                                            (let [visible? (api/panel-visible? changed-panel)]
                                              (when (and visible? (not (:visible? @(:state session))))
                                                (common/post! session (protocol/message :host/shown {})))
                                              (swap! (:state session) assoc :visible? visible?)))))
    (add! (api/on-theme-change! (fn [theme-kind]
                                  (common/post! session (protocol/message :host/theme {:theme-kind theme-kind}))
                                  (common/debounce! session :code-colors settle-milliseconds #(send-code-colors! session)))))
    (add! (api/on-settings-change! (fn [affects?]
                                     (when (:ready? @(:state session))
                                       (when (or (affects? "codecloud") (affects? "editor"))
                                         (common/post! session (init-message session)))
                                       (when (or (affects? "workbench") (affects? "editor") (affects? "window"))
                                         (common/debounce! session :code-colors settle-milliseconds
                                                           #(send-code-colors! session)))))))
    (add! (api/on-document-change! (fn [changed text-changed?]
                                     (if (same-path? (api/document-path changed) cloud-file)
                                       (when-not (api/document-dirty? changed)
                                         (common/debounce! session :configuration settle-milliseconds
                                                           #(snapshots/load-configuration! session)))
                                       (editing-messages/follow-edited-document! session changed text-changed?)))))
    (add! (api/on-document-save! (fn [saved]
                                   (editing-messages/follow-edited-document! session saved false)
                                   (when (same-path? (api/document-path saved) (path/join root-directory "deps.edn"))
                                     (common/debounce! session :configuration settle-milliseconds
                                                       #(snapshots/load-configuration! session))))))
    (when-let [git @(:git host)]
      (add! (api/watch-head! git
                             (fn [repository-root]
                               (let [relative (path/relative repository-root root-directory)]
                                 (not (or (string/starts-with? relative "..") (path/isAbsolute relative)))))
                             #(common/debounce! session :history settle-milliseconds (fn [] (snapshots/load-history! session))))))
    (when (api/panel-active? panel)
      (reset! (:active-session host) session))
    (register! session)
    session))

(defn refresh!
  "Reads the configuration and the working tree again."
  [{:keys [state analyzer] :as session}]
  (swap! state assoc :configuration nil)
  (analyzer/forget-snapshots! analyzer)
  (snapshots/load-configuration! session))

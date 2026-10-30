(ns codecloud.vscode.session
  "One open cloud: its panel, the analysis behind it, and the messages
  between them. Panels on one cloud file share an analyzer."
  (:require ["path" :as path]
            [clojure.string :as string]
            [codecloud.io.analyzer :as analyzer]
            [codecloud.io.files :as files]
            [codecloud.io.task :as task]
            [codecloud.model.config :as config]
            [codecloud.model.source :as source]
            [codecloud.model.workspace :as workspace]
            [codecloud.protocol :as protocol]
            [codecloud.vscode.api :as api]
            [codecloud.vscode.clouds :as clouds]
            [codecloud.vscode.editor-theme :as editor-theme]
            [codecloud.vscode.editor-zoom :as editor-zoom]
            [codecloud.vscode.settings :as settings]
            [codecloud.vscode.webview :as webview]))

(defn- error-text [error]
  (or (some-> error .-message) (str error)))

(defn- post! [{:keys [panel state]} message]
  (when-not (:disposed? @state)
    (api/post-message! panel (protocol/encode message))))

(defn- log! [{:keys [host cloud-file]} text]
  ((:log! host) (str (path/basename cloud-file) ": " text)))

(defn- post-status! [session message progress]
  (post! session (protocol/message :host/status {:message message :progress progress})))

(defn- post-notice! [session message]
  (post! session (protocol/message :host/notice {:message message})))

(defn- debounce!
  "Runs `action` once `milliseconds` pass without another call for the
  same `timer-key`."
  [{:keys [state]} timer-key milliseconds action]
  (js/clearTimeout (get-in @state [:timers timer-key]))
  (swap! state assoc-in [:timers timer-key] (js/setTimeout action milliseconds)))

(defn preference-key
  "Workspace memory key of one cloud file's `preference`."
  [cloud-file preference]
  (str "codecloud." (name preference) ":" (clouds/comparison-path cloud-file)))

(def ^:private cloud-preferences
  "Preferences remembered for each cloud file, since each codebase may be
  best seen its own way."
  [:view-filter :arrangement :color-layers :color-overrides :spread])

(def ^:private viewer-preferences
  "Preferences remembered for every cloud, as how the viewer likes to work."
  [:display-mode :totals-open? :tint-clouds?])

(defn- viewer-preference-key [preference]
  (str "codecloud." (name preference)))

(defn- init-message [{:keys [host cloud-file]}]
  (protocol/message :host/init
                    (into {:settings {:zoom-out-threshold (settings/zoom-out-threshold)
                                      :editor-colors? (settings/editor-colors?)}
                           :theme-kind (api/theme-kind)
                           :sky-mode (settings/sky-mode)
                           :display-mode (or (api/remembered (:global-state host) (viewer-preference-key :display-mode)) :code)
                           :totals-open? (api/remembered (:global-state host) (viewer-preference-key :totals-open?))
                           :tint-clouds? (api/remembered (:global-state host) (viewer-preference-key :tint-clouds?))
                           :cloud-name (workspace/cloud-name (string/replace cloud-file "\\" "/") config/cloud-file-name)
                           :editor-font (api/editor-font (or (api/editor-language-id) "clojure"))}
                          (map (fn [preference]
                                 [preference (api/remembered (:workspace-state host) (preference-key cloud-file preference))]))
                          cloud-preferences)))

;; Editor colors

(defn- send-code-colors!
  "Reads the code colors of the editor's theme and sends them to the
  view. Only the latest of overlapping reads counts."
  [{:keys [state] :as session}]
  (let [read-number (:code-color-reads (swap! state update :code-color-reads (fnil inc 0)))]
    (task/then (editor-theme/read-code-colors (or (api/editor-language-id) "clojure"))
               (fn [code-colors]
                 (when (= read-number (:code-color-reads @state))
                   (post! session (protocol/message :host/editor-colors {:code-colors code-colors})))))))

;; Analysis

(defn- views-message
  "The parts of a configuration the view shows: its named views and its
  color layers."
  [configuration]
  (protocol/message :host/views {:views (:views configuration)
                                 :color-layers (config/color-layer-specs configuration)}))

(defn- snapshot-fingerprint
  "What a working-tree snapshot was built from, so an unchanged rebuild
  sends nothing."
  [snapshot]
  (into (sorted-set)
        (mapcat (fn [[_id node]] (map (juxt :path :content-key) (:files node))))
        (:namespaces snapshot)))

(declare analyze!)

(defn- unsaved-text-lookup
  "Function from a path relative to the cloud's root to the text of that
  file when it is open with unsaved changes, or nil."
  [{:keys [root-directory]}]
  (let [texts (into {} (map (fn [[file-path text]] [(clouds/comparison-path file-path) text])) (api/unsaved-texts))]
    (fn [relative-path]
      (get texts (clouds/comparison-path (path/join root-directory relative-path))))))

(defn- finish-analysis! [{:keys [state] :as session}]
  (swap! state assoc :analyzing? false)
  (post-status! session nil nil)
  (when (:analyze-again? @state)
    (swap! state assoc :analyze-again? false)
    (analyze! session)))

(defn analyze!
  "Analyzes the working tree and sends the snapshot when it changed. A
  request while an analysis runs queues one more, and a result read with
  a configuration replaced meanwhile is dropped for that one."
  [{:keys [analyzer state] :as session}]
  (cond
    (:disposed? @state) nil
    (:analyzing? @state) (swap! state assoc :analyze-again? true)
    :else
    (let [configuration (:configuration @state)]
      (swap! state assoc :analyzing? true)
      (post-status! session "Reading the codebase" nil)
      (-> (analyzer/analyze-working-tree analyzer configuration
                                         {:on-progress (fn [{:keys [done total]}]
                                                         (post-status! session (str "Reading " done " of " total " files")
                                                                       (/ done (max 1 total))))
                                          :open-text (unsaved-text-lookup session)})
          (task/then (fn [{:keys [snapshot notices]}]
                       (when (identical? configuration (:configuration @state))
                         (let [fingerprint (snapshot-fingerprint snapshot)]
                           (when (not= fingerprint (:fingerprint @state))
                             (swap! state assoc :working-snapshot snapshot :fingerprint fingerprint)
                             (post! session (protocol/message :host/snapshot {:snapshot snapshot}))))
                         (doseq [notice notices] (post-notice! session notice)))))
          (task/recover (fn [error]
                          (log! session (str "Analysis failed: " (error-text error)))
                          (post-notice! session (str "CodeCloud could not read this folder: " (error-text error)))))
          (task/then (fn [_] (finish-analysis! session)))))))

(defn- load-history!
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
                       (post! session (protocol/message :host/history history)))))
        (task/recover (fn [error] (log! session (str "Reading history failed: " (error-text error))))))))

(defn- watch-sources!
  "Re-analyzes shortly after a source file or a .gitignore changes,
  skipping files the cloud ignores. Saving deps.edn reloads the
  configuration instead, through `start!`."
  [{:keys [root-directory state] :as session}]
  (api/dispose! (:source-watchers @state))
  (let [configuration (:configuration @state)
        wanted? (analyzer/wanted-path-predicate configuration)
        settle-milliseconds 400
        relative (fn [changed-path] (string/replace (path/relative root-directory changed-path) "\\" "/"))
        on-source (fn [changed-path]
                    (when (wanted? (relative changed-path))
                      (debounce! session :analyze settle-milliseconds #(analyze! session))))
        on-rules (fn [_] (debounce! session :analyze settle-milliseconds #(analyze! session)))]
    (swap! state assoc :source-watchers
           [(api/watch! root-directory "**/*" on-source)
            (api/watch! root-directory "**/.gitignore" on-rules)])))

(defn- load-configuration!
  "Reads the cloud file and deps.edn, tells the view about named views and
  any problems, and analyzes when the configuration changed. Only the
  latest of overlapping reads counts."
  [{:keys [analyzer document state] :as session}]
  (let [load-number (:configuration-loads (swap! state update :configuration-loads (fnil inc 0)))]
    (task/then (analyzer/read-configuration analyzer (api/document-text document))
               (fn [{:keys [config problems]}]
                 (when (and (= load-number (:configuration-loads @state)) (not (:disposed? @state)))
                   (let [previous (:configuration @state)]
                     (when (not= problems (:problems @state))
                       (doseq [problem problems] (post-notice! session problem)))
                     (swap! state assoc :problems problems)
                     (post! session (views-message config))
                     (when (not= config previous)
                       (swap! state assoc :configuration config :fingerprint nil)
                       (analyzer/forget-snapshots! analyzer)
                       (watch-sources! session)
                       (analyze! session)
                       (load-history! session))))))))

;; Messages from the view

(defn- snapshot-task
  "Resolves to the snapshot `snapshot-id` names."
  [{:keys [analyzer state]} snapshot-id]
  (if (= :working-tree snapshot-id)
    (task/resolved (:working-snapshot @state))
    (if-let [remembered (analyzer/remembered-snapshot analyzer snapshot-id)]
      (task/resolved remembered)
      (let [commit (or (some #(when (= snapshot-id (:sha %)) %) (get-in @state [:history :commits]))
                       {:sha snapshot-id})]
        (task/then (analyzer/analyze-commit analyzer (:configuration @state) commit) :snapshot)))))

(defmulti handle-message
  "Methods take the session and a message from the view."
  (fn [_session message] (:type message)))

(defmethod handle-message :default [session message]
  (log! session (str "Ignored a message of type " (:type message))))

(defn resend-init!
  "Sends settings and preferences again, for example after a command
  changed the view filter."
  [session]
  (when (:ready? @(:state session))
    (post! session (init-message session))))

(defn deliver-arrival!
  "Starts the view at a line the viewer zoomed out of, now or once the view
  is ready."
  [{:keys [state] :as session} arrival]
  (if (:ready? @state)
    (post! session (protocol/message :host/arrive arrival))
    (swap! state assoc :pending-arrival arrival)))

(defmethod handle-message :view/ready [{:keys [state] :as session} _message]
  (swap! state assoc :ready? true)
  (post! session (init-message session))
  (send-code-colors! session)
  (if-let [configuration (:configuration @state)]
    (do (post! session (views-message configuration))
        (when-let [snapshot (:working-snapshot @state)]
          (post! session (protocol/message :host/snapshot {:snapshot snapshot})))
        (when-let [history (:history @state)]
          (post! session (protocol/message :host/history history))))
    (load-configuration! session))
  (when-let [arrival (:pending-arrival @state)]
    (swap! state dissoc :pending-arrival)
    (post! session (protocol/message :host/arrive arrival))))

(defmethod handle-message :view/select-snapshot [session {:keys [snapshot-id]}]
  (when-not (= :working-tree snapshot-id)
    (let [sha (str snapshot-id)
          short-sha-length 7]
      (post-status! session (str "Reading commit " (subs sha 0 (min short-sha-length (count sha)))) nil)))
  (-> (snapshot-task session snapshot-id)
      (task/then (fn [snapshot]
                   (when snapshot
                     (post! session (protocol/message :host/snapshot {:snapshot snapshot})))))
      (task/recover (fn [error]
                      (post-notice! session (str "CodeCloud could not read that commit: " (error-text error)))))
      (task/then (fn [_] (when-not (= :working-tree snapshot-id) (post-status! session nil nil))))))

(defmethod handle-message :view/request-sources [{:keys [analyzer state] :as session} {:keys [snapshot-id namespaces]}]
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
                   (when (seq entries)
                     (post! session (protocol/message :host/sources {:snapshot-id snapshot-id :entries entries})))))
      (task/recover (fn [error] (log! session (str "Reading sources failed: " (error-text error)))))))

(defmethod handle-message :view/open-location [{:keys [root-directory panel] :as session} {:keys [path line column]}]
  (-> (api/show-text-document! (path/join root-directory path) (or line 1) (or column 1) (api/panel-view-column panel))
      (task/then (fn [_] (editor-zoom/hint-way-back!)))
      (task/recover (fn [error] (post-notice! session (str "CodeCloud could not open " path ": " (error-text error)))))))

(defn- document-message
  "A :host/document message carrying `document`'s text for `path`, which
  edits from the view build on from now, as their new :epoch."
  [{:keys [state]} path document]
  (let [epoch (get-in (swap! state update-in [:edits path :epoch] (fnil inc 0)) [:edits path :epoch])
        text (api/document-text document)]
    (protocol/message :host/document {:path path
                                      :epoch epoch
                                      :text (source/normalize-text text)
                                      :content-key (files/content-key text)
                                      :dirty? (api/document-dirty? document)})))

(defn- in-turn!
  "Runs `action`, a function returning a task, once every edit before it
  finished, so edits reach the document in the order they were made."
  [{:keys [state]} action]
  (swap! state update :edit-turn
         (fn [previous] (-> (or previous (task/resolved nil))
                            (task/then (fn [_] (action)))
                            (task/recover (fn [_error] nil))))))

(defn- apply-edit!
  "Applies one edit from the view to its document unless it builds on
  text the view has since replaced, or the document no longer holds the
  text it replaces, in which case the view gets the document's text."
  [{:keys [root-directory state] :as session} {:keys [path epoch revision range text replaced]}]
  (if (not= epoch (get-in @state [:edits path :epoch] 0))
    (task/resolved nil)
    (task/then
     (api/open-document (path/join root-directory path))
     (fn [document]
       (if (not= replaced (source/normalize-text (api/text-in-range document range)))
         (post! session (document-message session path document))
         (let [;; The cloud reads the code again after this much quiet, so
               ;; typing redraws dependencies at its pauses.
               edit-settle-milliseconds 700]
           (swap! state update-in [:edits path :applying] (fnil inc 0))
           (-> (api/replace-range! document range (cond-> text
                                                    (api/crlf-document? document) (string/replace "\n" "\r\n")))
               (task/recover (fn [_error] false))
               (task/then (fn [applied?]
                            (swap! state update-in [:edits path :applying] dec)
                            (if applied?
                              (do (post! session (protocol/message :host/edited
                                                                   {:path path
                                                                    :revision revision
                                                                    :content-key (files/content-key (api/document-text document))
                                                                    :dirty? (api/document-dirty? document)}))
                                  (debounce! session :analyze edit-settle-milliseconds #(analyze! session)))
                              (post! session (document-message session path document))))))))))))

(defmethod handle-message :view/edit [session message]
  (in-turn! session #(apply-edit! session message)))

(defmethod handle-message :view/save [{:keys [root-directory] :as session} {:keys [path]}]
  (in-turn! session
            (fn []
              (-> (api/open-document (path/join root-directory path))
                  (task/then api/save-document!)
                  (task/recover (fn [error] (post-notice! session (str "Saving " path " failed: " (error-text error)))))))))

(defn- edited-path
  "The path relative to the cloud's root of `document` when the view has
  edited it, or nil."
  [{:keys [root-directory state]} document]
  (let [relative (string/replace (path/relative root-directory (api/document-path document)) "\\" "/")]
    (when (contains? (:edits @state) relative) relative)))

(defn- follow-edited-document!
  "Keeps the view in step with a document it edits: a change from
  elsewhere sends the new text, and a save or a return to the saved text
  sends whether it is unsaved. The view's own edits report themselves."
  [{:keys [state] :as session} document text-changed?]
  (when-let [path (edited-path session document)]
    (cond
      (pos? (get-in @state [:edits path :applying] 0)) nil
      text-changed? (post! session (document-message session path document))
      :else (post! session (protocol/message :host/dirty {:path path :dirty? (api/document-dirty? document)})))))

(defmethod handle-message :view/save-preference [{:keys [host cloud-file] :as session} {preference :key :keys [value]}]
  (cond
    (= :sky-mode preference) (settings/save-sky-mode! value)
    (= :editor-colors? preference) (settings/save-editor-colors! value)
    (some #{preference} viewer-preferences) (api/remember! (:global-state host) (viewer-preference-key preference) value)
    (some #{preference} cloud-preferences) (api/remember! (:workspace-state host) (preference-key cloud-file preference) value)
    :else (log! session (str "Ignored the preference " preference))))

(defmethod handle-message :view/save-view [{:keys [document] :as session} {view-name :name :keys [selection]}]
  (let [text (source/normalize-text (api/document-text document))
        {:keys [line column error] :as insertion} (config/view-insertion text view-name selection)]
    (if error
      (post-notice! session error)
      (-> (api/insert-and-save! document line column
                                (cond-> (:text insertion)
                                  (api/crlf-document? document) (string/replace "\n" "\r\n")))
          (task/then (fn [saved?]
                       (post-notice! session (if saved?
                                               (str "Saved the view " (pr-str view-name) " in the cloud file.")
                                               "VS Code did not save the view."))))
          (task/recover (fn [error] (post-notice! session (str "Saving the view failed: " (error-text error)))))))))

(defmethod handle-message :view/log [session {:keys [message]}]
  (log! session message))

;; Lifetime

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
  "Ends a session when its panel closes."
  [{:keys [state disposables] :as session}]
  (swap! state assoc :disposed? true)
  (doseq [[_ timer] (:timers @state)] (js/clearTimeout timer))
  (api/dispose! (:source-watchers @state))
  (api/dispose! @disposables)
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
                 :disposables (atom [])}
        add! (fn [item] (swap! (:disposables session) conj item))
        settle-milliseconds 300]
    (webview/prepare! panel (:extension-uri host))
    (add! (api/on-panel-message! panel (fn [text]
                                         (when-let [message (protocol/decode text)]
                                           (handle-message session message)))))
    (add! (api/on-panel-dispose! panel #(stop! session)))
    (add! (api/on-panel-view-state! panel (fn [changed-panel]
                                            (when (api/panel-active? changed-panel)
                                              (reset! (:active-session host) session))
                                            ;; Coming back into view after the cloud opened a
                                            ;; file, the view draws back from that file's page.
                                            (let [visible? (api/panel-visible? changed-panel)]
                                              (when (and visible? (not (:visible? @(:state session))))
                                                (post! session (protocol/message :host/shown {})))
                                              (swap! (:state session) assoc :visible? visible?)))))
    (add! (api/on-theme-change! (fn [theme-kind]
                                  (post! session (protocol/message :host/theme {:theme-kind theme-kind}))
                                  (debounce! session :code-colors settle-milliseconds #(send-code-colors! session)))))
    (add! (api/on-settings-change! (fn [affects?]
                                     (when (:ready? @(:state session))
                                       (when (or (affects? "codecloud") (affects? "editor"))
                                         (post! session (init-message session)))
                                       (when (or (affects? "workbench") (affects? "editor") (affects? "window"))
                                         (debounce! session :code-colors settle-milliseconds
                                                    #(send-code-colors! session)))))))
    (add! (api/on-document-change! (fn [changed text-changed?]
                                     (if (same-path? (api/document-path changed) cloud-file)
                                       (when-not (api/document-dirty? changed)
                                         (debounce! session :configuration settle-milliseconds
                                                    #(load-configuration! session)))
                                       (follow-edited-document! session changed text-changed?)))))
    (add! (api/on-document-save! (fn [saved]
                                   (follow-edited-document! session saved false)
                                   (when (same-path? (api/document-path saved) (path/join root-directory "deps.edn"))
                                     (debounce! session :configuration settle-milliseconds
                                                #(load-configuration! session))))))
    (when-let [git @(:git host)]
      (add! (api/watch-head! git
                             (fn [repository-root]
                               (let [relative (path/relative repository-root root-directory)]
                                 (not (or (string/starts-with? relative "..") (path/isAbsolute relative)))))
                             #(debounce! session :history settle-milliseconds (fn [] (load-history! session))))))
    (when (api/panel-active? panel)
      (reset! (:active-session host) session))
    (register! session)
    session))

(defn refresh!
  "Reads the configuration and the working tree again."
  [{:keys [state analyzer] :as session}]
  (swap! state assoc :configuration nil :fingerprint nil)
  (analyzer/forget-snapshots! analyzer)
  (load-configuration! session))

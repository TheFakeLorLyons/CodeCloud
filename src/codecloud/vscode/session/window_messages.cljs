(ns codecloud.vscode.session.window-messages
  "What each window in the view shows, and keeping it current. A web page
  loads from its address, an HTML file from a small server for the
  workspace folder, and Markdown, PDFs, and images from the webview's own
  view of the folder. Markdown follows its editor as it is typed in; the
  rest follow their files on disk. A web page on this computer waits for
  its server to answer, so a window can open as the server starts."
  (:require ["path" :as path]
            [clojure.string :as string]
            [codecloud.io.file-server :as file-server]
            [codecloud.io.files :as files]
            [codecloud.io.ports :as ports]
            [codecloud.io.task :as task]
            [codecloud.presentation.windows-state :as windows-state]
            [codecloud.protocol :as protocol]
            [codecloud.vscode.api :as api]
            [codecloud.vscode.session.common :as common]))

(defn content-folder
  "The folder a cloud's windows may show files from, the workspace folder
  holding the cloud, or else the cloud's own folder."
  [{:keys [root-directory]}]
  (or (api/workspace-folder-path-of root-directory) root-directory))

(defn- inside? [folder file-path]
  (let [relative (path/relative folder file-path)]
    (not (or (string/starts-with? relative "..") (path/isAbsolute relative)))))

(defn- source-path
  "A file's path as a window's source, relative to the cloud's folder with
  forward slashes."
  [{:keys [root-directory]} file-path]
  (string/replace (path/relative root-directory file-path) "\\" "/"))

(defn- post-window!
  "Sends what window `window-id` shows, numbering each sending so the view
  shows each change once."
  [{:keys [state] :as session} window-id fields]
  (let [version (get-in (swap! state update-in [:windows window-id :version] (fnil inc 0)) [:windows window-id :version])]
    (common/post! session (protocol/message :host/window (assoc fields :id window-id :version version)))))

(defn- follow!
  "Keeps window `window-id` following `watcher`, letting go of what it
  followed before."
  [{:keys [state]} window-id watcher]
  (some-> (get-in @state [:windows window-id :watcher]) vector api/dispose!)
  (swap! state assoc-in [:windows window-id :watcher] watcher))

(defn- file-server
  "Resolves to the session's file server for its content folder, started
  the first time an HTML window asks."
  [{:keys [state] :as session}]
  (or (:file-server @state)
      (let [started (file-server/start! (content-folder session))]
        (swap! state assoc :file-server started)
        started)))

(defn- send-markdown!
  "Sends the Markdown's text, unsaved changes included, with the address
  its images are relative to."
  [{:keys [panel] :as session} window-id file-path]
  (-> ((common/text-reader session) (source-path session file-path))
      (task/then (fn [text]
                   (post-window! session window-id
                                 (if (some? text)
                                   {:text text :base-url (api/webview-uri panel (api/file-uri (path/dirname file-path)))}
                                   {:error (str "No file at " (source-path session file-path))}))))
      (task/recover (fn [error] (post-window! session window-id {:error (task/error-text error)})))))

(defn- send-address!
  "Sends `address` as a window's page, as it reaches VS Code's window."
  [session window-id address]
  (-> (api/external-uri address)
      (task/then (fn [external] (post-window! session window-id {:url external})))
      (task/recover (fn [error] (post-window! session window-id {:error (task/error-text error)})))))

(defn- wait-for-server!
  "Calls `ready!` once the server that web address `address` names on
  this computer answers, at once for an address elsewhere. Meanwhile the
  window says what it waits for, and after five minutes it says nothing
  answered. A window closed or loaded again stops the wait."
  [{:keys [state] :as session} window-id address ready!]
  (if-let [[host port] (ports/local-port address)]
    (let [load-number (get-in (swap! state update-in [:windows window-id :loads] (fnil inc 0)) [:windows window-id :loads])
          current? #(= load-number (get-in @state [:windows window-id :loads]))
          place (str host ":" port)
          retry-milliseconds 1000
          attempts 300]
      (letfn [(attempt [remaining]
                (task/then (ports/answers? host port)
                           (fn [answered?]
                             (when (current?)
                               (cond
                                 answered? (ready!)
                                 (zero? remaining) (post-window! session window-id
                                                                 {:error (str "Nothing answered at " place
                                                                              " for five minutes. Reload the window to try again.")})
                                 :else (do (when (= attempts remaining)
                                             (post-window! session window-id {:waiting (str "Waiting for " place " to answer")}))
                                           (js/setTimeout #(attempt (dec remaining)) retry-milliseconds)))))))]
        (attempt attempts)))
    (ready!)))

(defn- load-file-window!
  "Sends a file window its content and follows the file, or its folder
  for an HTML page, whose scripts and styles change too."
  [{:keys [panel] :as session} {:keys [id kind source]}]
  (let [file-path (path/resolve (:root-directory session) source)
        settle-milliseconds 150
        resend! (fn []
                  (case kind
                    :markdown (send-markdown! session id file-path)
                    :html (-> (file-server session)
                              (task/then (fn [{:keys [url]}]
                                           (send-address! session id (str url (string/join "/" (map js/encodeURIComponent
                                                                                                     (string/split (path/relative (content-folder session) file-path) #"[/\\]"))))))))
                    (post-window! session id {:url (api/webview-uri panel (api/file-uri file-path))})))]
    (cond
      (not (inside? (content-folder session) file-path))
      (post-window! session id {:error (str source " lies outside the workspace folder, which windows show files from.")})

      (not (files/path-exists? file-path)) (post-window! session id {:error (str "No file at " source)})

      :else (let [refresh! (fn [_changed] (common/debounce! session [:window id] settle-milliseconds resend!))]
              (resend!)
              (follow! session id (if (= :html kind)
                                    (api/watch! (path/dirname file-path) "**/*" refresh!)
                                    (api/watch! (path/dirname file-path) (path/basename file-path) refresh!)))))))

(defmethod common/handle-message :view/window-open [{:keys [state] :as session} {:keys [id kind source] :as window}]
  (swap! state assoc-in [:windows id :window] window)
  (if (= :web kind)
    (do (follow! session id nil)
        (wait-for-server! session id source #(send-address! session id source)))
    (load-file-window! session window)))

(defmethod common/handle-message :view/window-close [{:keys [state] :as session} {:keys [id]}]
  (follow! session id nil)
  (swap! state update :windows dissoc id))

(defmethod common/handle-message :view/window-pick-file [{:keys [root-directory] :as session} _message]
  (task/then (api/pick-file root-directory "Pages, documents, and images" (clj->js windows-state/file-extensions))
             (fn [file-path]
               (cond
                 (nil? file-path) nil
                 (not (inside? (content-folder session) file-path))
                 (common/post-notice! session "Windows show files from the workspace folder that holds the cloud.")
                 :else (common/post! session (protocol/message :host/open-window
                                                               {:kind (windows-state/file-kind file-path)
                                                                :source (source-path session file-path)}))))))

(defmethod common/handle-message :view/window-reveal [{:keys [root-directory]} {:keys [kind source]}]
  (if (= :web kind)
    (api/open-external! source)
    (api/open-with-default-editor! (path/resolve root-directory source))))

(defn follow-documents!
  "Sends Markdown windows their file's text as it is typed in VS Code's
  editor. Returns a disposable."
  [{:keys [state] :as session}]
  (let [settle-milliseconds 150]
    (api/on-document-change! (fn [document text-changed?]
                               (when text-changed?
                                 (let [file-path (api/document-path document)]
                                   (doseq [[window-id {{:keys [kind source]} :window}] (:windows @state)
                                           :when (and (= :markdown kind)
                                                      (= (path/resolve (:root-directory session) source) file-path))]
                                     (common/debounce! session [:window window-id] settle-milliseconds
                                                       #(send-markdown! session window-id file-path)))))))))

(defn open-window!
  "Opens a window in the session's view and brings the cloud forward."
  [{:keys [panel] :as session} window]
  (common/post! session (protocol/message :host/open-window window))
  (api/reveal-panel! panel))

(defn file-window
  "The window that shows `file-path` in the session's view, or nil when no
  window shows files of its kind or the file lies outside its folder."
  [session file-path]
  (when-let [kind (windows-state/file-kind file-path)]
    (when (inside? (content-folder session) file-path)
      {:kind kind :source (source-path session file-path)})))

(defn stop!
  "Lets go of every window's watcher and the file server."
  [{:keys [state]}]
  (doseq [[_window-id {:keys [watcher]}] (:windows @state)
          :when watcher]
    (api/dispose! [watcher]))
  (some-> (:file-server @state) (task/then (fn [{:keys [close!]}] (close!)))))

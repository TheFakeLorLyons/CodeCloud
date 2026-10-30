(ns codecloud.vscode.api
  "The VS Code API slice CodeCloud uses, as functions over Clojure data.
  Only this namespace touches the vscode module."
  (:require ["path" :as path]
            [cljs.reader :as reader]))

(def ^:private ^js vscode (js/require "vscode"))

;; Disposables

(defn disposable
  "A VS Code disposable that runs `dispose!` once."
  [dispose!]
  (let [Disposable (.-Disposable vscode)]
    (Disposable. dispose!)))

(defn dispose!
  "Disposes every disposable given, skipping nils."
  [disposables]
  (doseq [^js item disposables]
    (when item (.dispose item))))

;; Files and URIs

(defn file-uri [file-path]
  (.file (.-Uri vscode) file-path))

(defn uri-path [^js uri]
  (.-fsPath uri))

(defn on-disk?
  "True when a URI names a file that Node can read directly."
  [^js uri]
  (= "file" (.-scheme uri)))

(defn child-uri [^js base & segments]
  (.apply (.. vscode -Uri -joinPath) nil (into-array (cons base segments))))

;; Commands

(defn register-command!
  "Registers `handler` for a command; it receives the command's arguments."
  [command-id handler]
  (.registerCommand (.-commands vscode) command-id (fn [& arguments] (apply handler arguments))))

(defn execute-command
  "Runs a command and resolves to its result."
  [command-id & arguments]
  (let [^js commands (.-commands vscode)]
    (.apply (.-executeCommand commands) commands (into-array (cons command-id arguments)))))

(defn set-context!
  "Sets a when-clause context key."
  [context-key value]
  (execute-command "setContext" context-key value))

;; Messages and pickers

(defn info-message
  "Shows an information message; resolves to the chosen button's label,
  or nil when dismissed."
  [text & choices]
  (.then (.apply (.-showInformationMessage (.-window vscode)) (.-window vscode) (into-array (cons text choices)))
         #(when % %)))

(defn status-message!
  "Shows text in the status bar for `milliseconds`."
  [text milliseconds]
  (.setStatusBarMessage (.-window vscode) text milliseconds))

(defn pick-folder
  "Asks for a folder, starting at `starting-path`; resolves to its path or
  nil."
  [starting-path button-label]
  (.then (.showOpenDialog (.-window vscode)
                          #js {:canSelectFiles false
                               :canSelectFolders true
                               :canSelectMany false
                               :defaultUri (when starting-path (file-uri starting-path))
                               :openLabel button-label})
         (fn [^js chosen] (when (and chosen (pos? (.-length chosen))) (uri-path (aget chosen 0))))))

(defn quick-pick
  "Offers `items`, each {:label :description :value}; resolves to the
  chosen item's :value or nil."
  [items placeholder]
  (.then (.showQuickPick (.-window vscode)
                         (clj->js (map-indexed (fn [index {:keys [label description]}]
                                                 {:label label :description (or description "") :index index})
                                               items))
                         #js {:placeHolder placeholder})
         (fn [^js chosen] (when chosen (:value (nth items (.-index chosen)))))))

(defn output-channel [channel-name]
  (.createOutputChannel (.-window vscode) channel-name))

(defn append-line! [^js channel text]
  (.appendLine channel text))

;; Editors and documents

(defn active-editor-view
  "What the active editor shows, or nil: :path, :on-disk?, the one-based
  :cursor-line and visible lines, and :view-column."
  []
  (when-let [^js editor (.. vscode -window -activeTextEditor)]
    (let [^js document (.-document editor)
          ^js visible (first (.-visibleRanges editor))]
      {:path (.. document -uri -fsPath)
       :on-disk? (on-disk? (.-uri document))
       :cursor-line (inc (.. editor -selection -active -line))
       :first-visible-line (if visible (inc (.. visible -start -line)) 1)
       :last-visible-line (if visible (inc (.. visible -end -line)) 1)
       :view-column (.-viewColumn editor)})))

(defn- position [line column]
  (let [Position (.-Position vscode)]
    (Position. (max 0 (dec line)) (max 0 (dec column)))))

(defn show-text-document!
  "Opens the file at `file-path` in `view-column` with the cursor at the
  one-based `line` and `column`, centered in view."
  [file-path line column view-column]
  (let [Selection (.-Selection vscode)
        cursor (position line column)
        ^js selection (Selection. cursor cursor)]
    (.then (.showTextDocument (.-window vscode) (file-uri file-path)
                              #js {:viewColumn view-column :selection selection :preview false})
           (fn [^js editor]
             (.revealRange editor selection (.. vscode -TextEditorRevealType -InCenter))
             editor))))

(defn open-with!
  "Opens `uri` with the editor registered for `view-type`."
  [^js uri view-type view-column]
  (execute-command "vscode.openWith" uri view-type #js {:viewColumn view-column :preview false}))

(defn open-as-text!
  "Opens `uri` in the plain text editor beside the current one."
  [^js uri]
  (execute-command "vscode.openWith" uri "default" (.. vscode -ViewColumn -Beside)))

(defn open-document [file-path]
  (.openTextDocument (.-workspace vscode) (file-uri file-path)))

(defn document-text [^js document]
  (.getText document))

(defn document-path [^js document]
  (.. document -uri -fsPath))

(defn document-dirty? [^js document]
  (.-isDirty document))

(defn crlf-document?
  "True when the document ends its lines with CRLF."
  [^js document]
  (= (.-eol document) (.. vscode -EndOfLine -CRLF)))

(defn- zero-based-range [[line column end-line end-column]]
  (let [Range (.-Range vscode)
        Position (.-Position vscode)]
    (Range. (Position. line column) (Position. end-line end-column))))

(defn text-in-range
  "Text of a document within `span`, zero-based [line column end-line
  end-column]."
  [^js document span]
  (.getText document (zero-based-range span)))

(defn replace-range!
  "Replaces a zero-based `span` of a document with `text`, leaving it
  unsaved, as typing in an editor does. Resolves to true once applied."
  [^js document span text]
  (let [WorkspaceEdit (.-WorkspaceEdit vscode)
        ^js edit (WorkspaceEdit.)]
    (.replace edit (.-uri document) (zero-based-range span) text)
    (.applyEdit (.-workspace vscode) edit)))

(defn save-document!
  "Saves a document; resolves to true when it saved."
  [^js document]
  (.save document))

(defn- apply-and-save! [^js edit ^js document]
  (.then (.applyEdit (.-workspace vscode) edit)
         (fn [applied?] (if applied? (.save document) false))))

(defn insert-and-save!
  "Inserts text at a one-based position and saves the file. Resolves to
  true when both succeed."
  [^js document line column text]
  (let [WorkspaceEdit (.-WorkspaceEdit vscode)
        ^js edit (WorkspaceEdit.)]
    (.insert edit (.-uri document) (position line column) text)
    (apply-and-save! edit document)))

(defn on-document-change!
  "Calls `handler` with each document whose text or unsaved state changes,
  and whether its text changed."
  [handler]
  (.onDidChangeTextDocument (.-workspace vscode)
                            (fn [^js event] (handler (.-document event) (pos? (.. event -contentChanges -length))))))

(defn unsaved-texts
  "Map of the path of each file open with unsaved changes to its text."
  []
  (into {}
        (keep (fn [^js document]
                (when (and (.-isDirty document) (on-disk? (.-uri document)))
                  [(.. document -uri -fsPath) (.getText document)])))
        (array-seq (.. vscode -workspace -textDocuments))))

(defn on-document-save! [handler]
  (.onDidSaveTextDocument (.-workspace vscode) handler))

;; Theme and fonts

(defn theme-kind-name
  "The kind of the editor theme: :light, :dark, :high-contrast, or
  :high-contrast-light."
  []
  (let [kind (.. vscode -window -activeColorTheme -kind)
        ^js kinds (.-ColorThemeKind vscode)]
    (condp = kind
      (.-Light kinds) :light
      (.-HighContrast kinds) :high-contrast
      (.-HighContrastLight kinds) :high-contrast-light
      :dark)))

(defn theme-kind
  "The editor theme as :light or :dark; high contrast themes count by
  their background."
  []
  (if (contains? #{:light :high-contrast-light} (theme-kind-name)) :light :dark))

(defn theme-contributions
  "Every color theme the installed extensions contribute, as {:id :label
  :ui-theme :path}, :path being the theme file's absolute path."
  []
  (for [^js extension (array-seq (or (some-> vscode .-extensions .-all) #js []))
        :let [^js themes (some-> extension .-packageJSON .-contributes .-themes)]
        :when (array? themes)
        ^js theme (array-seq themes)
        :when (string? (.-path theme))]
    {:id (.-id theme)
     :label (.-label theme)
     :ui-theme (.-uiTheme theme)
     :path (path/join (.-extensionPath extension) (.-path theme))}))

(defn on-theme-change! [handler]
  (.onDidChangeActiveColorTheme (.-window vscode) (fn [_] (handler (theme-kind)))))

(defn editor-language-id
  "VS Code's id for the language of the active text editor, or of the
  first visible one while a cloud has focus, or nil."
  []
  (some-> (or (.. vscode -window -activeTextEditor)
              (first (.. vscode -window -visibleTextEditors)))
          .-document
          .-languageId))

(defn editor-setting
  "Value of the editor setting `setting-key` as code in `language-id`
  sees it, that language's own settings included, or `fallback`."
  [language-id setting-key fallback]
  (let [value (.get (.getConfiguration (.-workspace vscode) "editor" #js {:languageId language-id}) setting-key)]
    (if (nil? value) fallback value)))

(defn editor-font
  "The editor's font family, size, and line height as code in
  `language-id` shows them, including that language's own settings."
  [language-id]
  {:family (editor-setting language-id "fontFamily" nil)
   :size (editor-setting language-id "fontSize" nil)
   :line-height (editor-setting language-id "lineHeight" nil)})

;; Settings and stored state

(defn setting
  "Value of the setting `setting-key` in `section`, or `fallback`."
  [section setting-key fallback]
  (let [value (.get (.getConfiguration (.-workspace vscode) section) setting-key)]
    (if (nil? value) fallback value)))

(defn data-setting
  "Value of a setting holding JSON, such as a map of customizations, as
  Clojure data with string keys, or nil."
  [section setting-key]
  (some-> (setting section setting-key nil) js/JSON.stringify js/JSON.parse js->clj))

(defn update-setting!
  "Changes a setting where it is currently defined: in the workspace when
  the workspace sets it, otherwise for the user."
  [section setting-key value]
  (let [^js configuration (.getConfiguration (.-workspace vscode) section)
        ^js inspection (.inspect configuration setting-key)
        ^js targets (.-ConfigurationTarget vscode)]
    (.update configuration setting-key value
             (if (and inspection (some? (.-workspaceValue inspection)))
               (.-Workspace targets)
               (.-Global targets)))))

(defn on-settings-change!
  "Calls `handler` with a predicate telling whether a settings section
  changed."
  [handler]
  (.onDidChangeConfiguration (.-workspace vscode)
                             (fn [^js event] (handler (fn [section] (.affectsConfiguration event section))))))

(defn remembered
  "Value stored under `memory-key` in an extension memento, or nil."
  [^js memento memory-key]
  (try
    (some-> (.get memento memory-key) reader/read-string)
    (catch :default _error nil)))

(defn remember!
  "Stores `value` under `memory-key` in an extension memento."
  [^js memento memory-key value]
  (.update memento memory-key (pr-str value)))

;; Workspace folders and files

(defn workspace-folder-paths []
  (mapv #(uri-path (.-uri ^js %)) (or (.. vscode -workspace -workspaceFolders) #js [])))

(defn workspace-folder-path-of
  "Path of the workspace folder holding `file-path`, or nil."
  [file-path]
  (some-> (.getWorkspaceFolder (.-workspace vscode) (file-uri file-path)) .-uri .-fsPath))

(defn find-files
  "Resolves to the paths of workspace files matching `glob`."
  [glob exclude-glob]
  (.then (.findFiles (.-workspace vscode) glob exclude-glob) #(mapv uri-path %)))

(defn- watcher-listeners [^js watcher on-change]
  (let [notify (fn [^js uri] (on-change (uri-path uri)))]
    [watcher
     (.onDidChange watcher notify)
     (.onDidCreate watcher notify)
     (.onDidDelete watcher notify)]))

(defn watch!
  "Watches files matching `glob` under `directory`, calling `on-change`
  with each changed path. Returns a disposable."
  [directory glob on-change]
  (let [RelativePattern (.-RelativePattern vscode)
        parts (watcher-listeners (.createFileSystemWatcher (.-workspace vscode)
                                                           (RelativePattern. (file-uri directory) glob))
                                 on-change)]
    (disposable #(dispose! parts))))

(defn watch-workspace!
  "Watches workspace files matching `glob` anywhere. Returns a disposable."
  [glob on-change]
  (let [parts (watcher-listeners (.createFileSystemWatcher (.-workspace vscode) glob) on-change)]
    (disposable #(dispose! parts))))

;; Webview panels and the custom editor

(defn register-custom-editor!
  "Registers `resolve!` for each `view-type` document and its panel.
  Hidden panels keep running, so a cloud keeps its place."
  [view-type resolve!]
  (.registerCustomEditorProvider (.-window vscode) view-type
                                 #js {:resolveCustomTextEditor (fn [document panel _cancellation]
                                                                 (resolve! document panel)
                                                                 nil)}
                                 #js {:webviewOptions #js {:retainContextWhenHidden true}
                                      :supportsMultipleEditorsPerDocument true}))

(defn configure-panel!
  "Lets a panel run scripts and load files only from `resource-uris`."
  [^js panel resource-uris]
  (set! (.. panel -webview -options) #js {:enableScripts true
                                          :localResourceRoots (into-array resource-uris)}))

(defn set-panel-html! [^js panel html]
  (set! (.. panel -webview -html) html))

(defn webview-uri [^js panel ^js uri]
  (.toString (.asWebviewUri (.-webview panel) uri)))

(defn csp-source [^js panel]
  (.. panel -webview -cspSource))

(defn post-message! [^js panel text]
  (.postMessage (.-webview panel) text))

(defn on-panel-message! [^js panel handler]
  (.onDidReceiveMessage (.-webview panel) handler))

(defn on-panel-dispose! [^js panel handler]
  (.onDidDispose panel handler))

(defn on-panel-view-state! [^js panel handler]
  (.onDidChangeViewState panel (fn [_] (handler panel))))

(defn panel-view-column [^js panel]
  (.-viewColumn panel))

(defn panel-active? [^js panel]
  (.-active panel))

(defn panel-visible? [^js panel]
  (.-visible panel))

(defn on-active-editor-change!
  "Calls `handler` whenever another text editor becomes active."
  [handler]
  (.onDidChangeActiveTextEditor (.-window vscode) (fn [_] (handler))))

;; Git

(defn git-api
  "Resolves to the built-in git extension's API, or nil when that
  extension is missing or turned off."
  []
  (if-let [^js extension (.getExtension (.-extensions vscode) "vscode.git")]
    (-> (.activate extension)
        (.then (fn [^js exports] (when (and exports (.-enabled exports)) (.getAPI exports 1))))
        (.catch (fn [_error] nil)))
    (js/Promise.resolve nil)))

(defn git-path
  "Path of the git program the git extension found, or \"git\"."
  [^js git]
  (or (some-> git .-git .-path) "git"))

(defn watch-head!
  "Calls `on-change` whenever the commit checked out changes in a
  repository whose root `holds?` accepts. Returns a disposable."
  [^js git holds? on-change]
  (let [subscriptions (atom [])
        follow! (fn [^js repository]
                  (when (holds? (uri-path (.-rootUri repository)))
                    (let [last-head (atom (some-> repository .-state .-HEAD .-commit))]
                      (swap! subscriptions conj
                             (.onDidChange (.-state repository)
                                           (fn []
                                             (let [head (some-> repository .-state .-HEAD .-commit)]
                                               (when (not= head @last-head)
                                                 (reset! last-head head)
                                                 (on-change)))))))))]
    (doseq [repository (array-seq (.-repositories git))]
      (follow! repository))
    (swap! subscriptions conj (.onDidOpenRepository git follow!))
    (disposable #(dispose! @subscriptions))))


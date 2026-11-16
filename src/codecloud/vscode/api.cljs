(ns codecloud.vscode.api
  "The VS Code API slice CodeCloud uses, as functions over Clojure data.
  Only this namespace touches the vscode module."
  (:require ["path" :as path]
            [cljs.reader :as reader]
            [clojure.string :as string]))

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

(defn register-command! [command-id handler]
  (.registerCommand (.-commands vscode) command-id (fn [& arguments] (apply handler arguments))))

(defn execute-command [command-id & arguments]
  (let [^js commands (.-commands vscode)]
    (.apply (.-executeCommand commands) commands (into-array (cons command-id arguments)))))

(defn set-context! [context-key value]
  (execute-command "setContext" context-key value))

;; Messages and pickers

(defn info-message
  "Shows an information message; resolves to the chosen button's label,
  or nil when dismissed."
  [text & choices]
  (.then (.apply (.-showInformationMessage (.-window vscode)) (.-window vscode) (into-array (cons text choices)))
         #(when % %)))

(defn status-message! [text milliseconds]
  (.setStatusBarMessage (.-window vscode) text milliseconds))

(defn pick-folder [starting-path button-label]
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

(defn choose
  "Offers `items` under `title`, each {:label :description :detail
  :value} or {:separator label}, with `placeholder` in the filter box;
  resolves to the chosen item's :value or nil."
  [title placeholder items]
  (let [separator-kind (.. vscode -QuickPickItemKind -Separator)]
    (.then (.showQuickPick (.-window vscode)
                           (clj->js (map-indexed (fn [index {:keys [separator label description detail]}]
                                                   (if separator
                                                     {:label separator :kind separator-kind}
                                                     {:label label :description (or description "") :detail detail :index index}))
                                                 items))
                           #js {:title title :placeHolder placeholder :matchOnDescription true})
           (fn [^js chosen] (when chosen (:value (nth items (.-index chosen))))))))

(defn choose-from
  "Offers the items `items-task` resolves to, as `choose` offers items,
  showing the picker busy until they arrive; resolves to the chosen
  item's :value or nil."
  [title placeholder items-task]
  (let [separator-kind (.. vscode -QuickPickItemKind -Separator)
        arrived (atom [])]
    (.then (.showQuickPick (.-window vscode)
                           (.then items-task
                                  (fn [items]
                                    (reset! arrived (vec items))
                                    (clj->js (map-indexed (fn [index {:keys [separator label description detail]}]
                                                            (if separator
                                                              {:label separator :kind separator-kind}
                                                              {:label label :description (or description "") :detail detail :index index}))
                                                          items))))
                           #js {:title title :placeHolder placeholder :matchOnDescription true})
           (fn [^js chosen] (when chosen (:value (nth @arrived (.-index chosen))))))))

(defn input-box
  "Asks for one line of text, hidden when `password?`, resolving to it or
  nil. `validate` says why a value cannot be taken, and `selection`
  selects part of `value`."
  [{:keys [title prompt placeholder value password? validate selection]}]
  (.then (.showInputBox (.-window vscode)
                        (clj->js (cond-> {:title title :prompt prompt :placeHolder (or placeholder "") :value (or value "")
                                          :password (boolean password?) :ignoreFocusOut true}
                                   selection (assoc :valueSelection selection)
                                   validate (assoc :validateInput (fn [text] (or (validate text) js/undefined))))))
         #(when (some? %) %)))

(defn confirm
  "Asks `question` in a modal dialog explained by `detail`, with one
  `action` button; resolves to true when the action is chosen."
  [question detail action]
  (.then (.showWarningMessage (.-window vscode) question #js {:modal true :detail detail} action)
         #(= action %)))

(defn warning-message
  "Shows a warning; resolves to the chosen button's label, or nil."
  [text & choices]
  (.then (.apply (.-showWarningMessage (.-window vscode)) (.-window vscode) (into-array (cons text choices)))
         #(when % %)))

(defn output-channel [channel-name]
  (.createOutputChannel (.-window vscode) channel-name))

(defn append-line! [^js channel text]
  (.appendLine channel text))

(defn show-channel!
  "Shows an output channel without taking the keyboard from the editor."
  [^js channel]
  (.show channel true))

;; Clipboard, terminals, and the status bar

(defn write-clipboard!
  "Puts `text` on the clipboard; resolves once it is there."
  [text]
  (.writeText (.. vscode -env -clipboard) text))

(defn show-terminal!
  "Shows `terminal` in VS Code's panel, leaving the focus where it is."
  [^js terminal]
  (.show terminal true))

(defn run-in-terminal!
  "Opens a terminal called `terminal-name` in `directory`, shows it, and
  runs each of `command-lines` in turn."
  [terminal-name directory command-lines]
  (let [^js terminal (.createTerminal (.-window vscode) #js {:name terminal-name :cwd directory})]
    (.show terminal)
    (doseq [line command-lines]
      (.sendText terminal line true))
    terminal))

(defn status-bar-item
  "A status bar item on the right, hidden until shown; `priority` places
  it among the others there."
  [item-id item-name priority]
  (let [^js item (.createStatusBarItem (.-window vscode) item-id (.. vscode -StatusBarAlignment -Right) priority)]
    (set! (.-name item) item-name)
    item))

(defn show-status! [^js item {:keys [text tooltip command]}]
  (set! (.-text item) text)
  (set! (.-tooltip item) tooltip)
  (set! (.-command item) command)
  (.show item))

(defn hide-status! [^js item]
  (.hide item))

;; Terminals

(defn default-shell-name
  "The name of the shell VS Code's terminals start, such as bash, or nil."
  []
  (some-> (.. vscode -env -shell) path/basename (string/replace #"(?i)\.exe$" "") not-empty))

(defn create-terminal
  "Opens a terminal called `terminal-name` in `directory`, leaving VS
  Code's terminal panel as it is."
  [terminal-name directory]
  (.createTerminal (.-window vscode) #js {:name terminal-name :cwd directory}))

(defn open-terminals []
  (vec (array-seq (or (.. vscode -window -terminals) #js []))))

(defn terminal-name [^js terminal]
  (.-name terminal))

(defn pseudo-terminal?
  "True for a terminal an extension draws itself, such as Calva's output
  terminal, with no shell behind it to run commands."
  [^js terminal]
  (some? (some-> terminal .-creationOptions .-pty)))

(defn shell-integration-supported?
  "True when this VS Code reports what runs in its terminals, which it
  does from 1.93."
  []
  (boolean (.. vscode -window -onDidStartTerminalShellExecution)))

(defn shell-integrated?
  "True when VS Code's shell integration is active in `terminal`."
  [^js terminal]
  (some? (.-shellIntegration terminal)))

(defn terminal-cwd
  "The working directory shell integration reports for `terminal`, or nil."
  [^js terminal]
  (some-> terminal .-shellIntegration .-cwd .-fsPath))

(defn execute-in-terminal!
  "Runs `command` through `terminal`'s shell integration, returning the
  execution."
  [^js terminal command]
  (.executeCommand (.-shellIntegration terminal) command))

(defn send-to-terminal! [^js terminal text enter?]
  (.sendText terminal text enter?))

(defn dispose-terminal! [^js terminal]
  (.dispose terminal))

(defn execution-command
  "The command line an execution runs, as the shell reported it."
  [^js execution]
  (some-> execution .-commandLine .-value))

(defn read-execution!
  "Calls `on-data` with each chunk of raw output `execution` writes, then
  `on-end` once it writes no more."
  [^js execution on-data on-end]
  (let [^js stream (.read execution)
        ^js iterator (.call (aget stream js/Symbol.asyncIterator) stream)]
    (letfn [(pump []
              (-> (.next iterator)
                  (.then (fn [^js step]
                           (if (.-done step)
                             (on-end)
                             (do (on-data (.-value step))
                                 (pump)))))
                  (.catch (fn [_error] (on-end)))))]
      (pump))))

(defn on-shell-execution-start!
  "Calls `handler` with the terminal and the execution whenever a command
  starts in a terminal with shell integration. Returns a disposable, or
  nil when this VS Code cannot report it."
  [handler]
  (when (shell-integration-supported?)
    (.onDidStartTerminalShellExecution (.-window vscode) (fn [^js event] (handler (.-terminal event) (.-execution event))))))

(defn on-shell-execution-end!
  "Calls `handler` with the terminal, the execution, and its exit code
  whenever a command ends. Returns a disposable, or nil."
  [handler]
  (when (shell-integration-supported?)
    (.onDidEndTerminalShellExecution (.-window vscode)
                                     (fn [^js event] (handler (.-terminal event) (.-execution event) (.-exitCode event))))))

(defn on-shell-integration-change!
  "Calls `handler` with a terminal whose shell integration began or
  changed. Returns a disposable, or nil."
  [handler]
  (when-let [subscribe (.. vscode -window -onDidChangeTerminalShellIntegration)]
    (.call subscribe (.-window vscode) (fn [^js event] (handler (.-terminal event))))))

(defn on-terminal-close! [handler]
  (.onDidCloseTerminal (.-window vscode) handler))

;; Secrets

(defn secret [^js secrets secret-key]
  (.then (.get secrets secret-key) #(when % %)))

(defn store-secret!
  "Stores `value` under `secret-key` in VS Code's secret storage, which
  keeps it in the operating system's keychain."
  [^js secrets secret-key value]
  (.store secrets secret-key value))

(defn delete-secret! [^js secrets secret-key]
  (.delete secrets secret-key))

;; VS Code's own agents

(defn mcp-servers-supported?
  "True when this VS Code lets extensions offer MCP servers to its agents,
  which it does from 1.101."
  []
  (boolean (some-> vscode .-lm .-registerMcpServerDefinitionProvider)))

(defn offer-mcp-server!
  "Offers VS Code's agents an MCP server under `provider-id`, which
  `describe` returns as {:label :url :headers :version}. Returns
  {:disposable :changed!}, or nil when VS Code cannot take it."
  [provider-id describe]
  (when (mcp-servers-supported?)
    (let [EventEmitter (.-EventEmitter vscode)
          McpHttpServerDefinition (.-McpHttpServerDefinition vscode)
          ^js emitter (EventEmitter.)
          definitions (fn [_cancellation]
                        (if-let [{:keys [label url headers version]} (describe)]
                          #js [(McpHttpServerDefinition. label (.parse (.-Uri vscode) url) (clj->js headers) version)]
                          #js []))
          ^js registration (.registerMcpServerDefinitionProvider (.-lm vscode) provider-id
                                                                 #js {:onDidChangeMcpServerDefinitions (.-event emitter)
                                                                      :provideMcpServerDefinitions definitions
                                                                      :resolveMcpServerDefinition (fn [definition _cancellation] definition)})]
      {:disposable (disposable (fn []
                                 (.dispose registration)
                                 (.dispose emitter)))
       :changed! #(.fire emitter)})))

;; Editors and documents

(defn active-editor-view
  "What the active editor shows, its :path, :on-disk?, the one-based
  :cursor-line and visible lines, and its :view-column, or nil."
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

(defn open-with! [^js uri view-type view-column]
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

(defn crlf-document? [^js document]
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

(defn save-document! [^js document]
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

(defn- edit-reason
  "What an edit to a document did beyond changing its text, :undo or :redo
  when it undid or redid an earlier edit, else nil."
  [^js event]
  (let [^js reasons (.-TextDocumentChangeReason vscode)
        reason (.-reason event)]
    (cond
      (and reasons (some? reason) (= reason (.-Undo reasons))) :undo
      (and reasons (some? reason) (= reason (.-Redo reasons))) :redo
      :else nil)))

(defn on-document-edit!
  "Calls `handler` with each document on disk whose text changes and
  {:reason :spans}, each span the one-based first and last line a change
  covers in the new text."
  [handler]
  (.onDidChangeTextDocument (.-workspace vscode)
                            (fn [^js event]
                              (let [^js document (.-document event)
                                    changes (array-seq (.-contentChanges event))]
                                (when (and (seq changes) (on-disk? (.-uri document)))
                                  (handler document
                                           {:reason (edit-reason event)
                                            :spans (mapv (fn [^js change]
                                                           (let [first-line (inc (.. change -range -start -line))]
                                                             [first-line (+ first-line (count (re-seq #"\n" (.-text change))))]))
                                                         changes)}))))))

(defn on-document-will-save!
  "Calls `handler` with each document on disk VS Code is about to save,
  before formatters and other save actions run."
  [handler]
  (.onWillSaveTextDocument (.-workspace vscode)
                           (fn [^js event]
                             (when (on-disk? (.. event -document -uri))
                               (handler (.-document event))))))

(defn on-document-open!
  "Calls `handler` with each document on disk VS Code opens."
  [handler]
  (.onDidOpenTextDocument (.-workspace vscode)
                          (fn [^js document]
                            (when (on-disk? (.-uri document))
                              (handler document)))))

(defn on-document-close!
  "Calls `handler` with each document on disk VS Code closes."
  [handler]
  (.onDidCloseTextDocument (.-workspace vscode)
                           (fn [^js document]
                             (when (on-disk? (.-uri document))
                               (handler document)))))

(defn open-documents
  "The documents on disk VS Code holds open."
  []
  (filterv #(on-disk? (.-uri ^js %)) (array-seq (.. vscode -workspace -textDocuments))))

(defn window-focused?
  "True while this VS Code window has the keyboard."
  []
  (boolean (some-> (.. vscode -window -state) .-focused)))

(defn active-caret-lines
  "The lines, counted from zero, of both ends of every selection in the
  active editor when it shows `document`, else nil."
  [^js document]
  (let [^js editor (.. vscode -window -activeTextEditor)]
    (when (and editor (identical? document (.-document editor)))
      (into []
            (mapcat (fn [^js selection] [(.. selection -active -line) (.. selection -anchor -line)]))
            (array-seq (or (.-selections editor) #js [(.-selection editor)]))))))

;; Theme and fonts

(defn theme-kind-name
  "The kind of the editor theme, one of :light, :dark, :high-contrast, and
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

(defn setting [section setting-key fallback]
  (let [value (.get (.getConfiguration (.-workspace vscode) section) setting-key)]
    (if (nil? value) fallback value)))

(defn data-setting
  "Value of a setting holding JSON, such as a map of customizations, as
  Clojure data with string keys, or nil."
  [section setting-key]
  (some-> (setting section setting-key nil) js/JSON.stringify js/JSON.parse js->clj))

(defn update-setting!
  "Changes a setting where it is currently defined, in the workspace when
  the workspace sets it and otherwise for the user."
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

(defn remember! [^js memento memory-key value]
  (.update memento memory-key (pr-str value)))

;; Workspace folders and files

(defn workspace-folder-paths []
  (mapv #(uri-path (.-uri ^js %)) (or (.. vscode -workspace -workspaceFolders) #js [])))

(defn workspace-folder-path-of [file-path]
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

(defn- editor-selection
  "`editor`'s caret and selection for a file on disk, as {:path :text
  :language-id :line :column :anchor-line :anchor-column}, zero-based, or
  nil."
  [^js editor]
  (when editor
    (let [^js document (.-document editor)
          ^js selection (.-selection editor)]
      (when (on-disk? (.-uri document))
        {:path (.. document -uri -fsPath)
         :text (.getText document)
         :language-id (.-languageId document)
         :line (.. selection -active -line)
         :column (.. selection -active -character)
         :anchor-line (.. selection -anchor -line)
         :anchor-column (.. selection -anchor -character)}))))

(defn active-editor-selection
  "Where the active text editor's caret and selection are, as
  editor-selection gives them, or nil."
  []
  (editor-selection (.. vscode -window -activeTextEditor)))

(defn on-selection-change!
  "Calls `handler` with {:path :line :column :anchor-line :anchor-column},
  counted from zero, whenever the caret or the selection moves in an
  editor showing a file on disk."
  [handler]
  (.onDidChangeTextEditorSelection (.-window vscode)
                                   (fn [^js event]
                                     (when-let [moved (editor-selection (.-textEditor event))]
                                       (handler (dissoc moved :text :language-id))))))

(defn on-visible-editors-change!
  "Calls `handler` whenever the text editors in view change."
  [handler]
  (.onDidChangeVisibleTextEditors (.-window vscode) (fn [_] (handler))))

(defn line-end-decoration-type
  "A decoration type writing text after the end of a line, in the color
  of the theme color `color-id`."
  [color-id]
  (.createTextEditorDecorationType (.-window vscode)
                                   #js {:after #js {:color (let [ThemeColor (.-ThemeColor vscode)] (ThemeColor. color-id))
                                                    :margin "0 0 0 1.5em"}
                                        :rangeBehavior (.. vscode -DecorationRangeBehavior -ClosedClosed)}))

(defn show-line-ends!
  "Shows `entries`, each {:line :text} with the line counted from zero,
  after the ends of their lines in every visible editor of the file at
  `file-path`, with `decoration-type`, replacing what it showed there."
  [file-path decoration-type entries]
  (doseq [^js editor (array-seq (.. vscode -window -visibleTextEditors))
          :let [^js document (.-document editor)]
          :when (= file-path (.. document -uri -fsPath))]
    (.setDecorations editor decoration-type
                     (into-array
                      (keep (fn [{:keys [line text]}]
                              (when (< line (.-lineCount document))
                                (let [line-end (.. document (lineAt line) -range -end)
                                      Range (.-Range vscode)]
                                  #js {:range (Range. line-end line-end)
                                       :renderOptions #js {:after #js {:contentText text}}})))
                            entries)))))

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

(defn git-path [^js git]
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

;; Formatting and problems

(defn format-edits
  "Resolves to the edits the editor's formatter makes to `document`, each
  {:range :text} zero-based, for lines `start-line` to `end-line`, or the
  whole document when they are nil."
  [^js document start-line end-line]
  (let [language-id (.-languageId document)
        options #js {:tabSize (editor-setting language-id "tabSize" 2)
                     :insertSpaces (editor-setting language-id "insertSpaces" true)}
        requested (if start-line
                    (let [last-line (min end-line (dec (.-lineCount document)))]
                      (execute-command "vscode.executeFormatRangeProvider" (.-uri document)
                                       (zero-based-range [start-line 0 last-line (.. document (lineAt last-line) -text -length)])
                                       options))
                    (execute-command "vscode.executeFormatDocumentProvider" (.-uri document) options))]
    (.then requested
           (fn [^js edits]
             (mapv (fn [^js edit]
                     (let [^js edit-range (.-range edit)]
                       {:range [(.. edit-range -start -line) (.. edit-range -start -character)
                                (.. edit-range -end -line) (.. edit-range -end -character)]
                        :text (.-newText edit)}))
                   (array-seq (or edits #js [])))))))

(defn extension-active? [extension-id]
  (boolean (some-> (.getExtension (.-extensions vscode) extension-id) .-isActive)))

(defn calva-installed? []
  (some? (.getExtension (.-extensions vscode) "betterthantomorrow.calva")))

(defn calva-api
  "Resolves to the first version of Calva's extension API, starting Calva
  when it waits, or to nil without Calva."
  []
  (if-let [^js extension (.getExtension (.-extensions vscode) "betterthantomorrow.calva")]
    (-> (.activate extension)
        (.then (fn [^js exports] (some-> exports .-v1)))
        (.catch (fn [_error] nil)))
    (js/Promise.resolve nil)))

(def ^:private severity-names
  "Problem severities in the order of VS Code's DiagnosticSeverity."
  [:error :warning :information :hint])

(defn- problem-code
  "A problem's code as text, which VS Code gives as a string, a number,
  or an object holding the value with a link to its explanation."
  [code]
  (cond
    (or (string? code) (number? code)) (str code)
    (some? code) (some-> (.-value ^js code) str)
    :else nil))

(defn diagnostics
  "Every problem VS Code knows of in files on disk, by path, each {:line
  :column :end-line :end-column :severity :message :source :code},
  zero-based."
  []
  (into {}
        (keep (fn [^js entry]
                (let [^js uri (aget entry 0)
                      ^js found (aget entry 1)]
                  (when (and (on-disk? uri) (pos? (.-length found)))
                    [(.-fsPath uri)
                     (mapv (fn [^js diagnostic]
                             (let [^js span (.-range diagnostic)]
                               {:line (.. span -start -line)
                                :column (.. span -start -character)
                                :end-line (.. span -end -line)
                                :end-column (.. span -end -character)
                                :severity (get severity-names (.-severity diagnostic) :information)
                                :message (.-message diagnostic)
                                :source (.-source diagnostic)
                                :code (problem-code (.-code diagnostic))}))
                           (array-seq found))]))))
        (array-seq (.getDiagnostics (.-languages vscode)))))

(defn on-diagnostics-change!
  "Calls `handler` whenever the problems VS Code knows of change."
  [handler]
  (.onDidChangeDiagnostics (.-languages vscode) (fn [_event] (handler))))

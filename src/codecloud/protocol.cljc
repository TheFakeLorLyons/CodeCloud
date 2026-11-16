(ns codecloud.protocol
  "Messages between the cloud view and its host. Each is an EDN map with a
  :type key, sent as a string so keywords and sets survive the trip."
  (:require #?(:clj [clojure.edn :as edn]
               :cljs [cljs.reader :as reader])))

(def message-types
  "Every message type, with its direction and what it carries."
  {:view/ready "view to host: the view loaded and can receive data"
   :view/select-snapshot "view to host: show :snapshot-id, a commit hash or :working-tree"
   :view/request-sources "view to host: send the source files of :namespaces, each {:namespace-id :source-key}, in :snapshot-id"
   :view/open-location "view to host: open :path at :line and :column in a text editor"
   :view/edit "view to host: in :path, replace :replaced at :range, [line column end-line end-column] from zero, with :text; :revision numbers the view's edits and :epoch the text they build on"
   :view/save "view to host: save :path"
   :view/save-preference "view to host: remember :key with :value for this viewer"
   :view/save-view "view to host: add a view called :name with :selection to the cloud file"
   :view/log "view to host: write :message to the extension's output channel"
   :view/repl-connect "view to host: connect REPL tab :tab to the nREPL server at :host and :port, or without a :port to the one a port file in the cloud's folder or a folder above names, or with :via :calva join Calva's own REPL"
   :view/repl-disconnect "view to host: close REPL tab :tab's connection"
   :view/repl-eval "view to host: evaluate :code in REPL tab :tab as request :id in namespace :ns, read from :path at :line and :column, counted from one; with a :path, a namespace the REPL lacks loads from that file's ns form first"
   :view/repl-load-file "view to host: load :path into REPL tab :tab as request :id, its text being :code"
   :view/repl-load-files "view to host: load the files at :paths into REPL tab :tab, in order, as request :id"
   :view/repl-interrupt "view to host: stop every evaluation REPL tab :tab still runs"
   :view/repl-close "view to host: tab :tab closed; close its connection, or let go of its terminal"
   :view/shell-open "view to host: open shell tab :tab in a new VS Code terminal, or in the open terminal :terminal names"
   :view/shell-run "view to host: run :command in shell tab :tab's terminal as request :id"
   :view/shell-input "view to host: type :text and Enter into shell tab :tab's terminal, as input for the command running there"
   :view/shell-interrupt "view to host: press Ctrl+C in shell tab :tab's terminal"
   :view/list-terminals "view to host: send the terminals VS Code has open"
   :view/focus "view to host: what the viewer is looking at: the :snapshot-id and :snapshot-label shown, the :lifted page or :lifted-folder, :editing with the edited page's :namespace-id and :path and the caret's :line, :column, :anchor-line, and :anchor-column counted from zero, the :in-view page ids largest first, the :view-filter, the :visible-count, and the :arrangement"
   :view/connect-agent "view to host: offer the ways to connect an agent to the agent hub"
   :view/agents-reporting "view to host: agents report at :level, one of codecloud.agents.tools/reporting-levels"
   :view/agents-wavescope-bands "view to host: agents get :bands of wavescope's view around a line, one of codecloud.agents.tools/wavescope-bands"
   :view/dismiss-pin "view to host: take away the pin an agent set, :pin-id"
   :view/assistant-ask "view to host: ask the assistant :text"
   :view/assistant-stop "view to host: stop the assistant's question"
   :view/assistant-new "view to host: begin a new conversation with the assistant"
   :view/assistant-model "view to host: offer the assistant's models and the ways to add or remove a provider"
   :view/format "view to host: format :text, the view's text of :path in :language at :revision and :epoch, with the editor's formatter: lines :start-line to :end-line, counted from zero, or the whole file without them"
   :view/new-file "view to host: ask for the name of a new file in :directory, relative to the cloud's folder, and make it"
   :view/window-open "view to host: load window :id, a :kind window, one of :web, :html, :markdown, :pdf, and :image, on :source, a web address or a path relative to the cloud's folder, and keep it current"
   :view/window-close "view to host: window :id closed, so stop keeping it current"
   :view/window-pick-file "view to host: ask for a file from the cloud's folder to show in a window"
   :view/window-reveal "view to host: open the :source of a :kind window in VS Code, or a web page in the browser"
   :view/run-page-command "view to host: run VS Code's :command with :args, or the command line :shell in a terminal, for the file :path, relative to the cloud's folder, its caret at :line and :column, counted from one; :title names it"
   :view/pick-page-command "view to host: offer every command extensions contribute, and run the one picked for the file :path at :line and :column, as :view/run-page-command runs one"
   :view/list-ignored "view to host: list what the cloud leaves out inside the folder :path, ending in a slash, or \"\" for the cloud's folder"
   :host/init "host to view: settings, theme, editor font, and remembered preferences"
   :host/views "host to view: the cloud file's named :views and its :color-layers"
   :host/snapshot "host to view: a snapshot to draw"
   :host/history "host to view: commits that can be viewed as :commits"
   :host/sources "host to view: :entries of {:source-key :namespace-id :files}, each file with :path, :language, and :text"
   :host/status "host to view: progress :message, or nil when idle"
   :host/theme "host to view: the editor theme changed to :theme-kind"
   :host/editor-colors "host to view: the editor theme's :code-colors for both skies, or nil when its files cannot be read"
   :host/arrive "host to view: the viewer zoomed out of :path at :line; start there"
   :host/shown "host to view: the cloud's panel came back into view"
   :host/edited "host to view: edit :revision of :path applied, leaving text with :content-key, and whether the file is :dirty?"
   :host/document "host to view: :path holds :text with :content-key, after an edit that could not apply or a change from elsewhere; later edits build on :epoch"
   :host/dirty "host to view: whether :path has unsaved changes"
   :host/notice "host to view: a :message worth showing, such as configuration problems"
   :host/repl-status "host to view: REPL tab :tab's connection :status, one of :connecting, :connected, :disconnected, and :failed, with its :port, a :message saying what happened, and :via :calva for a tab joined to Calva's REPL"
   :host/repl-output "host to view: part of the answer to request :id in REPL tab :tab, holding :out or :err text, a printed :value with its :ns, an :ex class, a :note on what the host did for it, or :status words, done among them in the last part; or the :input Calva evaluated from elsewhere, opening a request of its own"
   :host/shell-status "host to view: shell tab :tab's :status, one of :starting, :ready, :running, :closed, and :failed, with the shell's :label, its :cwd, and a :message"
   :host/shell-output "host to view: output :text of request :id in shell tab :tab, a :note about it, the :command when it was typed in VS Code's own terminal, and at its end :done? with the :exit-code and the new :cwd"
   :host/terminals "host to view: the :terminals VS Code has open with a shell behind them, each {:id :name}, and whether Calva is installed, as :calva?"
   :host/evaluate "host to view: evaluate code from a file open in a VS Code editor through the REPL tab that evaluates, as request :id: the :scope, the :code, the file's :path and :namespace-id, the :namespace-name, and the :line and :column where the code starts, counted from one"
   :host/agents "host to view: whether the agent hub is :serving?, its :url, the :agents present, each {:name :since}, how much agents are :reporting, the :wavescope-bands they get, and a :problem when it could not start"
   :host/presence "host to view: where agents work: the :agents, each {:id :name :initials :hue :place :doing :intent :reads :changes :present? :parent :since :last-at}, with its :place {:path :line}, its :reads {:path :start :end :at}, and its :changes {:path :start :end :added :removed :at :credited?}, and the :pins, each {:id :agent-id :path :line :note :at}; paths are relative to the cloud's folder, lines count from one, and a read or change of a whole file has no lines"
   :host/formatted "host to view: :text, the formatted text of :path at :revision and :epoch"
   :host/created "host to view: the new file at :path is made, the caret to go to :line and :column, counted from zero"
   :host/diagnostics "host to view: the problems VS Code knows of in the cloud's files, :files mapping each path to its problems, each {:line :column :end-line :end-column :severity :message :source :code}, lines and columns counted from zero and :severity one of :error, :warning, :information, and :hint"
   :host/window "host to view: what window :id shows at :version, a :url to load, or Markdown :text with the :base-url its images sit under, or :waiting saying what it waits for, or an :error saying why it shows nothing"
   :host/open-window "host to view: open a :kind window on :source, as :view/window-open names them"
   :host/pick-page-command "host to view: offer the commands to run for the page in hand, the one being edited, brought closer, or under the pointer"
   :host/ignored "host to view: the :entries, each {:path :folder? :reason}, that the cloud leaves out inside :path, the :reason :git, :cloud, or :type, with :more counting those not listed"
   :host/assistant "host to view: the conversation with the assistant: its :entries, each {:id :version :kind} with :kind :question, :answer, :tool, or :notice, a question, an answer, and a notice holding :text, an answer still arriving marked :streaming?, and a tool holding its :name, :title, :target, :status (:running, :done, or :failed), and :result; whether a question is :running?, the :model chosen with its provider, whether one is chosen as :ready?, and whether the agent hub gives the assistant its tools as :hub?"})

(defn encode [message]
  (let [protocol-version 1]
    (pr-str (assoc message :protocol/version protocol-version))))

(defn decode
  "String to message map, or nil when the text is not a message."
  [text]
  (try
    (let [message #?(:clj (edn/read-string text)
                     :cljs (reader/read-string text))]
      (when (and (map? message) (keyword? (:type message)))
        message))
    (catch #?(:clj Exception :cljs :default) _error
      nil)))

(defn message [message-type fields]
  {:pre [(contains? message-types message-type)]}
  (assoc fields :type message-type))

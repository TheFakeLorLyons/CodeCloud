(ns codecloud.presentation.page-commands
  "Commands a page runs for its file from its right-click menu. A VS Code
  command runs with the file shown in the editor at the caret, as when it
  runs from the file's own editor, so commands that read the editor, such
  as Calva's and Calva Power Tools', work on the page. A command line runs
  in a terminal in the cloud's folder. Either may show a window once it
  has run, such as the page a server it starts answers at. The
  codecloud.pageCommands setting lists the commands every page offers,
  and a page runs any other command picked from a list, offered from its
  menu or, for the page in hand, by Ctrl+Alt+R or C in the cloud and by
  the CodeCloud: Run a Command for the Page command."
  (:require [clojure.string :as string]
            [codecloud.presentation.handling :refer [changed send-effect with-notice]]
            [codecloud.presentation.page :as page]
            [codecloud.presentation.windows-state :as windows-state]))

(defn- trimmed-text [value]
  (when (some? value)
    (let [text (string/trim (str value))]
      (when (seq text) text))))

(defn from-setting
  "One entry of the codecloud.pageCommands setting, a map with string
  keys, as {:title :command :args :shell :window}, or nil without a title
  and a VS Code command or a command line. A window's address may be a
  port alone, for this computer's."
  [entry]
  (when (map? entry)
    (let [title (trimmed-text (get entry "title"))
          command (trimmed-text (get entry "command"))
          shell (trimmed-text (get entry "shell"))
          window (some-> (trimmed-text (get entry "window")) windows-state/web-address)]
      (when (and title (or command shell))
        (cond-> {:title title}
          command (assoc :command command)
          (and command (contains? entry "args")) (assoc :args (get entry "args"))
          (and shell (not command)) (assoc :shell shell)
          window (assoc :window window))))))

(defn command-line
  "Command line `shell` with ${file}, ${relativeFile}, and ${line} put in
  from `place`, {:file :relative-file :line}."
  [shell {:keys [file relative-file line]}]
  (-> shell
      (string/replace "${file}" (str file))
      (string/replace "${relativeFile}" (str relative-file))
      (string/replace "${line}" (str line))))

(defn- page-place
  "The file and caret, counted from one, a command for page
  `namespace-id` runs at: the caret while the page is being edited, else
  the start of the page's file."
  [{:keys [editing snapshot] :as state} namespace-id]
  (if (and (:path editing) (= namespace-id (:namespace-id editing)))
    (let [[line column] (get-in state [:buffers (:path editing) :caret] [0 0])]
      {:path (:path editing) :line (inc line) :column (inc column)})
    (when-let [file (page/primary-file (get-in snapshot [:namespaces namespace-id]))]
      {:path (:path file) :line 1 :column 1})))

(defn- run-page-command
  "Asks the host to run the page command at `index` for page
  `namespace-id`, and shows its window, if it names one."
  [state index namespace-id]
  (let [page-command (get-in state [:settings :page-commands index])
        place (page-place state namespace-id)
        closed (assoc state :menu nil)]
    (if (and page-command place)
      (let [run (send-effect :view/run-page-command (merge (dissoc page-command :window) place))]
        (if-let [address (:window page-command)]
          (update (windows-state/show-web-page closed address) :effects #(into [run] %))
          (changed closed run)))
      (changed closed))))

(defn- pick-page-command
  "Asks the host to offer every command extensions contribute, and to run
  the one picked for page `namespace-id`."
  [state namespace-id]
  (if-let [place (page-place state namespace-id)]
    (changed (assoc state :menu nil) (send-effect :view/pick-page-command place))
    (changed (assoc state :menu nil))))

(defn- page-in-hand
  "The page a command runs for when none was right-clicked: the page
  being edited, else the one brought closer, else the one under the
  pointer."
  [{:keys [editing focused-id hovered-id]}]
  (or (:namespace-id editing) focused-id hovered-id))

(defn- pick-command-for-page-in-hand
  "Offers the commands to run for the page in hand, or says to click a
  page first."
  [state]
  (if-let [namespace-id (page-in-hand state)]
    (pick-page-command state namespace-id)
    (changed (with-notice state "Click a page first, then run a command for it."))))

(def handlers
  {:run-page-command run-page-command
   :pick-page-command pick-page-command
   :pick-command-for-page-in-hand pick-command-for-page-in-hand})

(def message-handlers
  "Handlers of the host's page command messages, by type."
  {:host/pick-page-command (fn [state _message] (pick-command-for-page-in-hand state))})

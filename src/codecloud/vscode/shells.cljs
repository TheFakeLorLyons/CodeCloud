(ns codecloud.vscode.shells
  "Shell tabs of one cloud, each joined to one of VS Code's terminals,
  which the tab opened or found open. Commands typed into a tab run
  through VS Code's shell integration, which reports their output, exit
  code, and working directory, and commands typed in VS Code's own
  terminal panel show in the tab as well. Each function takes a context
  of {:shells :root-directory :post!}."
  (:require [clojure.string :as string]
            [codecloud.presentation.terminal :as terminal]
            [codecloud.protocol :as protocol]
            [codecloud.vscode.api :as api]))

(defn create
  "The shell tabs of a cloud that has opened none."
  []
  (atom {}))

;; Naming terminals

(defonce ^:private terminal-ids (js/WeakMap.))

(defonce ^:private terminals-named (atom 0))

(defn- terminal-id
  "A lasting id for `terminal`, so the view can name it."
  [terminal]
  (or (.get terminal-ids terminal)
      (let [id (str "terminal-" (swap! terminals-named inc))]
        (.set terminal-ids terminal id)
        id)))

(defn list-terminals
  "The terminals VS Code has open with a shell behind them, each {:id
  :name}."
  []
  (into []
        (comp (remove api/pseudo-terminal?)
              (map (fn [open-terminal] {:id (terminal-id open-terminal) :name (api/terminal-name open-terminal)})))
        (api/open-terminals)))

;; Telling the view

(defn- post-status! [{:keys [post!]} tab fields]
  (post! (protocol/message :host/shell-status (assoc fields :tab tab))))

(defn- post-output! [{:keys [post!]} fields]
  (post! (protocol/message :host/shell-output fields)))

(defn- tab-of [{:keys [shells]} open-terminal]
  (some (fn [[tab {joined :terminal}]] (when (identical? joined open-terminal) tab)) @shells))

(defn- working-directory
  "Where a tab's terminal works, as shell integration reports it, or else
  the folder the tab opened it in."
  [{joined :terminal :keys [start-directory]}]
  (or (api/terminal-cwd joined) start-directory))

(defn- ready!
  "Tells the view a tab's terminal takes commands, saying when its output
  cannot reach the tab."
  [{:keys [shells] :as context} tab]
  (when-let [{joined :terminal :keys [label] :as shell} (get @shells tab)]
    (let [integrated? (api/shell-integrated? joined)]
      (swap! shells assoc-in [tab :integrated?] integrated?)
      (post-status! context tab {:status :ready
                                 :label label
                                 :cwd (working-directory shell)
                                 :message (when-not integrated?
                                            (str "VS Code's shell integration is off in this terminal, so commands run "
                                                 "but their output shows only in VS Code's terminal panel."))}))))

(defn open!
  "Joins tab `tab` to the terminal `terminal` names, or a new one in the
  cloud's folder, telling the view once it takes commands."
  [{:keys [shells root-directory] :as context} {:keys [tab] terminal-wanted :terminal}]
  (let [integration-patience-milliseconds 4000
        existing (when terminal-wanted
                   (some #(when (= terminal-wanted (terminal-id %)) %) (api/open-terminals)))
        shell-name (or (api/default-shell-name) "Shell")
        joined (or existing (api/create-terminal (str "CodeCloud " shell-name) root-directory))
        label (if existing (api/terminal-name existing) shell-name)]
    (if (and terminal-wanted (nil? existing))
      (post-status! context tab {:status :failed :message "That terminal has closed."})
      (do (swap! shells assoc tab {:terminal joined :created? (nil? existing) :label label :integrated? false
                                   :start-directory (when (nil? existing) root-directory)})
          (post-status! context tab {:status :starting :label label
                                     :message (if existing (str "Joined the terminal " label ".") (str "Opened a terminal running " shell-name "."))})
          (if (or (api/shell-integrated? joined) (not (api/shell-integration-supported?)))
            (do (swap! shells assoc-in [tab :phase] :ready)
                (ready! context tab))
            (js/setTimeout #(when (and (contains? @shells tab) (= :starting (get-in @shells [tab :phase] :starting)))
                              (swap! shells assoc-in [tab :phase] :ready)
                              (ready! context tab))
                           integration-patience-milliseconds))))))

;; Running commands

(defn- mark-running!
  "Notes that request `id` runs in tab `tab`'s terminal, or with
  `running?` false that it ended, unless the tab closed meanwhile."
  [{:keys [shells]} tab id running?]
  (swap! shells (fn [tabs]
                  (if (contains? tabs tab)
                    (update-in tabs [tab :running] (if running? (fnil conj #{}) disj) id)
                    tabs))))

(defn- stream-output!
  "Passes an execution's output to the view as lines for request `id`,
  then reports its end. A line still waiting for its end shows after a
  short pause, as a prompt asking for input does."
  [context tab id execution]
  (let [pause-milliseconds 300
        exit-code-patience-milliseconds 1500
        state (atom {:reader terminal/empty-state :stream-done? false :ended? false :exit-code nil :timer nil :finished? false
                     :redraw-noted? false})
        note-redraws! (fn []
                        (when-not (:redraw-noted? @state)
                          (swap! state assoc :redraw-noted? true)
                          (post-output! context {:tab tab :id id
                                                 :note (str "This program draws over the terminal in place, which the tab shows "
                                                            "only line by line. It works as drawn in VS Code's terminal, shown now.")})
                          (some-> (get-in @(:shells context) [tab :terminal]) api/show-terminal!)))
        send-text! (fn [text] (when (seq text) (post-output! context {:tab tab :id id :text text})))
        finish! (fn []
                  (let [{:keys [stream-done? ended? exit-code finished?]} @state]
                    (when (and stream-done? ended? (not finished?))
                      (swap! state assoc :finished? true)
                      (mark-running! context tab id false)
                      (let [{:keys [lines]} (terminal/flush-line (:reader @state))]
                        (send-text! lines)
                        (post-output! context {:tab tab :id id :done? true :exit-code exit-code
                                               :cwd (some-> (get @(:shells context) tab) working-directory)})))))]
    (mark-running! context tab id true)
    (api/read-execution! execution
                         (fn [chunk]
                           (js/clearTimeout (:timer @state))
                           (let [{:keys [lines redraws?] reader :state} (terminal/feed (:reader @state) chunk)]
                             (swap! state assoc :reader reader)
                             (when redraws? (note-redraws!))
                             (send-text! lines))
                           (swap! state assoc :timer
                                  (js/setTimeout (fn []
                                                   (let [{:keys [lines] reader :state} (terminal/flush-line (:reader @state))]
                                                     (swap! state assoc :reader reader)
                                                     (send-text! lines)))
                                                 pause-milliseconds)))
                         (fn []
                           (js/clearTimeout (:timer @state))
                           (swap! state assoc :stream-done? true)
                           (finish!)
                           ;; An exit code that never comes still ends the request.
                           (js/setTimeout #(do (swap! state assoc :ended? true) (finish!))
                                          exit-code-patience-milliseconds)))
    {:ended! (fn [exit-code]
               (swap! state assoc :ended? true :exit-code exit-code)
               (finish!))}))

(defonce ^:private watched-executions (js/WeakMap.))

(defn run-command!
  "Runs `command` in tab `tab`'s terminal. Without shell integration, or
  while another command runs, it is typed into the terminal and the
  request ends at once."
  [{:keys [shells] :as context} {:keys [tab id command]}]
  (let [{joined :terminal :keys [running]} (get @shells tab)]
    (cond
      (nil? joined)
      (post-output! context {:tab tab :id id :text "This tab has no terminal.\n" :done? true})

      (and (api/shell-integrated? joined) (empty? running))
      (let [execution (api/execute-in-terminal! joined command)]
        (swap! shells update-in [tab :sent] (fnil conj []) command)
        (.set watched-executions execution (stream-output! context tab id execution)))

      :else
      (do (api/send-to-terminal! joined command true)
          (post-output! context {:tab tab :id id :done? true})))))

(defn send-input!
  "Types `text` and Enter into tab `tab`'s terminal, as input for the
  command running there."
  [{:keys [shells]} {:keys [tab text]}]
  (when-let [{joined :terminal} (get @shells tab)]
    (api/send-to-terminal! joined text true)))

(defonce ^:private panel-commands (atom 0))

(defn- execution-started!
  "Shows in its tab a command typed in VS Code's own terminal panel, in a
  terminal a tab is joined to; commands the tab sent itself are read
  already."
  [{:keys [shells] :as context} open-terminal execution]
  (when-let [tab (tab-of context open-terminal)]
    (let [command (string/trim (or (api/execution-command execution) ""))
          sent (get-in @shells [tab :sent])
          drop-sent! (fn []
                       (let [[before after] (split-with #(not= command (string/trim %)) sent)]
                         (swap! shells assoc-in [tab :sent] (vec (concat before (rest after))))))]
      (cond
        ;; The tab reads its own commands from the execution it started.
        (or (.has watched-executions execution) (some #(= command (string/trim %)) sent)) (drop-sent!)
        :else
        (let [id (str "panel-" (swap! panel-commands inc))]
          (post-output! context {:tab tab :id id :command (or command "")})
          (.set watched-executions execution (stream-output! context tab id execution)))))))

(defn- execution-ended! [execution exit-code]
  (when-let [{:keys [ended!]} (.get watched-executions execution)]
    (ended! exit-code)))

(defn interrupt!
  "Presses Ctrl+C in tab `tab`'s terminal."
  [{:keys [shells]} {:keys [tab]}]
  (when-let [{joined :terminal} (get @shells tab)]
    (api/send-to-terminal! joined "\u0003" false)))

;; Ending

(defn close!
  "Lets go of tab `tab`'s terminal, closing it when the tab opened it."
  [{:keys [shells]} tab]
  (when-let [{joined :terminal :keys [created?]} (get @shells tab)]
    (swap! shells dissoc tab)
    (when created?
      (api/dispose-terminal! joined))))

(defn release-all!
  "Lets go of every terminal as the cloud closes, leaving them open in
  VS Code's terminal panel."
  [{:keys [shells]}]
  (reset! shells {}))

(defn announce!
  "Tells the view about every shell tab again, as after it reloaded."
  [{:keys [shells] :as context}]
  (doseq [[tab {:keys [label] :as shell}] @shells]
    (post-status! context tab {:status :ready :label label :cwd (working-directory shell)})))

(defn watch!
  "Follows VS Code's terminals for this cloud's shell tabs, as shell
  integration starts, commands start and end, and terminals close.
  Returns the disposables to dispose with the cloud."
  [{:keys [shells] :as context}]
  (remove nil?
          [(api/on-shell-integration-change! (fn [open-terminal]
                                               (when-let [tab (tab-of context open-terminal)]
                                                 (when-not (get-in @shells [tab :integrated?])
                                                   (swap! shells assoc-in [tab :phase] :ready)
                                                   (ready! context tab)))))
           (api/on-shell-execution-start! #(execution-started! context %1 %2))
           (api/on-shell-execution-end! (fn [_terminal execution exit-code] (execution-ended! execution exit-code)))
           (api/on-terminal-close! (fn [open-terminal]
                                     (when-let [tab (tab-of context open-terminal)]
                                       (swap! shells dissoc tab)
                                       (post-status! context tab {:status :closed :message "The terminal closed."}))))]))

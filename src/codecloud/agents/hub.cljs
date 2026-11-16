(ns codecloud.agents.hub
  "CodeCloud's MCP hub, the HTTP endpoint agents connect to. It serves the
  cloud's own tools, answered from CodeCloud's analysis and the person's
  view, and wavescope's views of files and projects from a child process.
  It keeps the presence, recording what each agent read through the hub,
  what it announced, the pins it set, and the changes credited to it. The
  hub knows nothing of VS Code; codecloud.vscode.agents starts it and
  tells it of changes no person made."
  (:require ["fs" :as fs]
            ["path" :as path]
            [clojure.string :as string]
            [codecloud.agents.cloud-tools :as cloud-tools]
            [codecloud.agents.mcp :as mcp]
            [codecloud.agents.presence :as presence]
            [codecloud.agents.roster :as roster]
            [codecloud.agents.tools :as tools]
            [codecloud.io.mcp-http :as mcp-http]
            [codecloud.io.mcp-stdio :as mcp-stdio]
            [codecloud.io.task :as task]
            [goog.object :as gobj]))

(def endpoint "/mcp")

(defn- log! [{:keys [config]} text]
  (when-let [log-line! (:log! config)]
    (log-line! text)))

(defn- reporting
  "How much agents report, one of codecloud.agents.tools/reporting-levels."
  [{:keys [config]}]
  (if-let [level (:reporting config)] (level) :full))

;; Paths

(defn- existing-path [candidate]
  (-> (.access (.-promises fs) candidate)
      (task/then (fn [_] candidate))
      (task/recover (fn [_error] nil))))

(defn- absolute-path
  "Resolves to the absolute path `given` names, which is `given` itself
  when absolute, else its place within the first of `roots` holding it,
  or within the first root when none does."
  [roots given]
  (if (path/isAbsolute given)
    (task/resolved (path/normalize given))
    (task/then (task/all (map #(existing-path (path/resolve % given)) roots))
               (fn [found] (or (some identity found) (path/resolve (first roots) given))))))

(defn- real-path
  "Resolves to `candidate` with symbolic links followed, or unchanged
  when nothing exists there."
  [candidate]
  (task/recover (.realpath (.-promises fs) candidate) (fn [_error] candidate)))

(defn- within? [folder candidate]
  (let [relative (path/relative folder candidate)]
    (not (or (= ".." relative)
             (string/starts-with? relative (str ".." (.-sep path)))
             (path/isAbsolute relative)))))

(defn- inside-roots
  "Resolves to `absolute` when it lies within one of `roots` once links
  are followed, or rejects saying the hub reads nowhere else."
  [roots absolute]
  (task/then (task/all (cons (real-path absolute) (map real-path roots)))
             (fn [[real-candidate & real-roots]]
               (if (some #(within? % real-candidate) real-roots)
                 absolute
                 (task/rejected (js/Error. (str absolute " lies outside the workspace folders, "
                                                "which are the only places the hub reads")))))))

(defn resolved-arguments
  "Resolves to a tool call's arguments with every path made absolute and
  checked to lie within `roots`, the workspace's folders. A landmark
  search given neither a file nor a directory searches the first folder."
  [roots tool-name arguments]
  (let [completed (if (and (= "get_important_positions" tool-name)
                           (not (contains? arguments "file"))
                           (not (contains? arguments "directory")))
                    (assoc arguments "directory" (first roots))
                    arguments)
        path-keys (filter #(string? (get completed %)) tools/path-arguments)]
    (if (empty? roots)
      (task/rejected (js/Error. "No folder is open in VS Code, so the hub has nothing to read"))
      (task/then (task/all (map (fn [path-key]
                                  (-> (absolute-path roots (get completed path-key))
                                      (task/then #(inside-roots roots %))
                                      (task/then (fn [checked] [path-key checked]))))
                                path-keys))
                 #(into completed %)))))

(defn- described-target
  "What a call is about, for the log, naming the file or folder it reads,
  relative to the first root when it lies there, or the page, definition,
  or folder a call to the cloud's tools names."
  [roots arguments]
  (let [target (some #(let [value (get arguments %)] (when (string? value) value))
                     ["file" "directory" "page" "name" "from" "folder"])]
    (cond
      (not (string? target)) nil
      (not (path/isAbsolute target)) target
      (and (seq roots) (within? (first roots) target)) (let [relative (path/relative (first roots) target)]
                                                         (if (string/blank? relative) "the workspace" relative))
      :else target)))

;; Agents and presence

(defn present-agents
  "The agents present now, as codecloud.agents.roster/present-agents
  lists them."
  [{:keys [state]}]
  (if-let [http-server (:http @state)]
    (roster/present-agents (mcp-http/roster-of http-server) (js/Date.now))
    []))

(defn- present-sessions [hub]
  (into #{} (map :session) (present-agents hub)))

(defn- agent-of
  "The agent behind session `session-id` as the presence records it, with
  its id, its name told apart from a twin's by number, and its client."
  [{:keys [state] :as hub} session-id]
  (let [session (some-> (:http @state) mcp-http/roster-of (get session-id))
        listed (some #(when (= session-id (:session %)) %) (present-agents hub))]
    {:id session-id
     :name (or (:name listed) (:name session) "An agent")
     :client (:client session)}))

(defn presence-summary
  "Where agents work now, as codecloud.agents.presence/summary gives it,
  each agent saying as :present? whether it is still connected."
  [{:keys [state] :as hub}]
  (presence/summary (:presence @state) (js/Date.now) (present-sessions hub)))

(defn- revise-presence! [{:keys [state config] :as hub} revise]
  (swap! state update :presence revise)
  (when-let [on-presence (:on-presence config)]
    (on-presence (presence-summary hub))))

(defn- record-reads!
  "Records that session `session-id`'s agent read each of `readings`,
  {:path :start :end :line :doing} with :path absolute, as
  codecloud.agents.presence/read-by takes them."
  [hub session-id readings]
  (when (seq readings)
    (let [reader (agent-of hub session-id)
          now (js/Date.now)]
      (revise-presence! hub (fn [current]
                              (reduce (fn [so-far reading]
                                        (presence/read-by so-far reader (update reading :path #(some-> % path/normalize)) now))
                                      current
                                      readings))))))

(defn- show-effect!
  "Shows the work or pin that session `session-id`'s agent, or a subagent
  of it, reported, with :path absolute."
  [hub session-id {:keys [kind line intent note] effect-path :path named :name}]
  (let [caller (agent-of hub session-id)
        now (js/Date.now)
        place (some-> effect-path path/normalize)
        ;; Below :full, subagents leave reporting to their agent, so a
        ;; name one sends anyway joins its agent's badge.
        subagent-name (when (= :full (reporting hub)) named)]
    (revise-presence! hub (fn [current]
                            (case kind
                              :announce (presence/announced current
                                                            (if subagent-name
                                                              {:id (str session-id "/" subagent-name)
                                                               :name subagent-name
                                                               :client (:client caller)
                                                               :parent session-id}
                                                              caller)
                                                            {:path place :line line :intent intent}
                                                            now)
                              :pin (presence/pinned current caller {:path place :line line :note note} now)
                              current)))))

(defn note-change!
  "Records a change no person made to the file at `:path`, credited to the
  agent that touched it last. Changes count only while an agent is
  connected. True when it counted."
  [{:keys [state config] :as hub} {changed-path :path :keys [start end added removed] :as change}]
  (boolean
   (when (and changed-path (seq (present-agents hub)))
     (let [now (js/Date.now)
           credit (presence/credited (:presence @state) (path/normalize changed-path) now)
           credited-name (some-> (get-in @state [:presence :agents credit]) :name)
           roots ((:roots config))]
       (revise-presence! hub #(presence/changed % (assoc change :path (path/normalize changed-path)) now))
       (log! hub (str (or credited-name "An agent CodeCloud could not name") " changed "
                      (described-target roots {"file" changed-path})
                      (cond
                        (nil? start) ", its lines unknown"
                        (= start end) (str ", line " start ", +" added " \u2212" removed)
                        :else (str ", lines " start "-" end ", +" added " \u2212" removed))
                      (when credited-name ", by CodeCloud's guess"))))
     true)))

(defn dismiss-pin!
  "Takes away pin `pin-id`, which the person dismissed."
  [hub pin-id]
  (revise-presence! hub #(presence/unpinned % pin-id)))

(defn- expire-presence!
  "Lets go of reads and changes past their lifetimes and of agents that
  left with nothing more to show, handing the presence on when that
  changed it, or always when `announce?`, as when an agent came or went."
  [{:keys [state] :as hub} announce?]
  (let [before (:presence @state)
        after (presence/expired before (present-sessions hub) (js/Date.now))]
    (when (or announce? (not= before after))
      (revise-presence! hub (constantly after)))))

;; wavescope

(defn- wavescope!
  "Resolves to the running wavescope client, starting wavescope first
  when it is not running. Calls arriving while it starts share the one
  start; a wavescope that exits starts again on the next call."
  [{:keys [state config] :as hub}]
  (or (:wavescope @state)
      (let [{:keys [command arguments environment]} (:wavescope config)
            starting (-> (mcp-stdio/start! {:command command
                                            :arguments arguments
                                            :environment environment
                                            :working-directory (first ((:roots config)))
                                            :client-info {"name" "codecloud" "version" (:version config)}
                                            :on-log #(log! hub (str "wavescope: " %))
                                            :on-exit (fn [_client reason]
                                                       (swap! state dissoc :wavescope)
                                                       (when-not (:stopping? @state)
                                                         (log! hub (str "wavescope ended, since " reason
                                                                        "; it starts again on the next call."))))})
                         (task/recover (fn [error]
                                         (swap! state dissoc :wavescope)
                                         (task/rejected (js/Error. (str "wavescope did not start: " (task/error-text error)))))))]
        (swap! state assoc :wavescope starting)
        starting)))

(defn- read-texts
  "Resolves to a map from each of `paths` to its text, read with
  `read-text` a batch at a time, so a page used everywhere never opens
  too many files at once. A file that cannot be read is left out."
  [read-text paths]
  (let [batch-size 64]
    (reduce (fn [so-far batch]
              (task/then so-far
                         (fn [texts]
                           (task/then (task/all (map (fn [file-path]
                                                       (task/then (read-text file-path) (fn [text] [file-path text])))
                                                     batch))
                                      (fn [pairs] (into texts (filter second) pairs))))))
            (task/resolved {})
            (partition-all batch-size (distinct paths)))))

(defn- run-cloud-tool!
  "Resolves to a cloud tool's answer for session `session-id`, recording
  in the presence what the call read and what it asks to show."
  [{:keys [config] :as hub} session-id tool-name arguments]
  (if-let [cloud (:cloud config)]
    (task/then (cloud)
               (fn [context]
                 (if (nil? context)
                   (mcp/failed-result "No folder is open in VS Code, so there is no cloud to read.")
                   (task/then (read-texts (:read-text context) (cloud-tools/paths-to-read context tool-name arguments))
                              (fn [texts]
                                (let [context (assoc context :texts texts :now (js/Date.now)
                                                     :presence (presence-summary hub) :caller session-id)
                                      in-folder (fn [relative]
                                                  (when (and relative (:root context)) (path/resolve (:root context) relative)))
                                      {:keys [text error effect]} (cloud-tools/answer context tool-name arguments)]
                                  (cond
                                    error (mcp/failed-result error)
                                    effect (do (show-effect! hub session-id (update effect :path in-folder))
                                               (mcp/text-result text))
                                    :else (do (record-reads! hub session-id
                                                             (map #(update % :path in-folder)
                                                                  (cloud-tools/touched-places context tool-name arguments)))
                                              (mcp/text-result text)))))))))
    (task/resolved (mcp/failed-result "This hub serves no cloud."))))

(defn- shown-range
  "The zero-based first and last line a wavescope answer showed in full,
  from the range of its fine band, or nil when it names none."
  [result]
  (let [text (get-in result ["content" 0 "text"])]
    (when (and (string? text) (string/starts-with? text "{"))
      (let [parsed (try (js/JSON.parse text) (catch :default _error nil))
            fine-range (when (object? parsed) (gobj/getValueByKeys parsed "bands" "fine" "range"))]
        (when (array? fine-range) (vec fine-range))))))

(defn- record-wavescope-read!
  "Records what session `session-id`'s call of wavescope's tool read,
  given the call's resolved arguments, unless the call failed."
  [hub session-id tool-name resolved result]
  (when-not (get result "isError")
    (when-let [reading (tools/wavescope-read tool-name resolved (shown-range result))]
      (record-reads! hub session-id [reading]))))

(defn- run-wavescope-tool!
  "Resolves to wavescope's answer to session `session-id`'s call, its
  paths resolved first, recording what it read."
  [hub session-id roots tool-name arguments]
  (task/then (resolved-arguments roots tool-name arguments)
             (fn [resolved]
               (-> (wavescope! hub)
                   (task/then #(mcp-stdio/call-tool! % tool-name resolved))
                   (task/then (fn [result]
                                (record-wavescope-read! hub session-id tool-name resolved result)
                                result))))))

(defn- run-cursor-tool!
  "Resolves to wavescope's answer about the person's caret, telling
  wavescope first where the caret was last in the file the call names, or
  the caret's own file."
  [{:keys [config] :as hub} session-id roots tool-name arguments]
  (let [{:keys [latest by-file]} (when-let [carets (:carets config)] (carets))
        named (get arguments "file")]
    (if (and (nil? named) (nil? latest))
      (task/rejected (js/Error. (str "The person has not placed a caret in a file since CodeCloud started. "
                                     "Name a file and line with query_wavelet_context instead.")))
      (task/then (resolved-arguments roots tool-name (cond-> arguments (nil? named) (assoc "file" (:file latest))))
                 (fn [resolved]
                   (let [file (get resolved "file")]
                     (if-let [caret (get by-file (path/normalize file))]
                       (-> (wavescope! hub)
                           (task/then (fn [client]
                                        (task/then (mcp-stdio/call-tool! client "update_cursor_position"
                                                                         {"file" file "line" (:line caret) "column" (:column caret)})
                                                   (fn [_] (mcp-stdio/call-tool! client tool-name resolved)))))
                           (task/then (fn [result]
                                        (record-wavescope-read! hub session-id tool-name (assoc resolved "center" (:line caret)) result)
                                        result)))
                       (task/rejected (js/Error. (str "The person's caret has not been in " (described-target roots resolved)
                                                      " since CodeCloud started"
                                                      (when latest (str "; it is in " (described-target roots {"file" (:file latest)})))
                                                      "."))))))))))

(defn- run-tool!
  "Resolves to the result of the tool call one agent made, logging the
  call with the time it took."
  [{:keys [state config] :as hub} session-id {tool-name :name :keys [arguments]}]
  (let [started (js/Date.now)
        roots ((:roots config))
        agent-label (or (some-> (:http @state) mcp-http/roster-of (get session-id) :name) "An agent")
        logged! (fn [outcome]
                  (log! hub (str agent-label ": " (string/join " " (remove nil? [tool-name (described-target roots arguments)]))
                                 ", " outcome " in " (- (js/Date.now) started) " ms")))]
    (-> (task/resolved nil)
        (task/then (fn [_]
                     (cond
                       (cloud-tools/tool? tool-name) (run-cloud-tool! hub session-id tool-name arguments)
                       (contains? tools/cursor-tools tool-name) (run-cursor-tool! hub session-id roots tool-name arguments)
                       :else (run-wavescope-tool! hub session-id roots tool-name arguments))))
        (task/then (fn [result]
                     (logged! (if (get result "isError") "failed" "answered"))
                     result))
        (task/recover (fn [error]
                        (logged! (str "failed (" (task/error-text error) ")"))
                        (mcp/failed-result (task/error-text error)))))))

(defn- note-arrivals!
  "Logs each agent that arrives or leaves, with the client it uses, and
  hands the presence on when one did, since it says who is connected."
  [{:keys [state] :as hub} sessions]
  (let [now (js/Date.now)
        present (into {}
                      (keep (fn [[session-id session]]
                              (when (roster/present? session now) [session-id session])))
                      sessions)
        before (:present @state {})
        arrived (remove #(contains? before (key %)) present)
        left (remove #(contains? present (key %)) before)]
    (doseq [[_session-id {agent-label :name :keys [client client-version protocol-version]}] arrived]
      (log! hub (str agent-label " connected"
                     (when client (str " as " client (when client-version (str " " client-version))))
                     ", speaking MCP " protocol-version)))
    (doseq [[_session-id {agent-label :name}] left]
      (log! hub (str agent-label " left")))
    (swap! state assoc :present present)
    (when (or (seq arrived) (seq left))
      (expire-presence! hub true))))

;; Running

(defn url
  "The address local agents reach the hub at. A hub listening on every
  interface is still reached at 127.0.0.1 from this computer."
  [{:keys [address port]}]
  (let [url-host (cond
                   (contains? #{"0.0.0.0" "::" ""} address) "127.0.0.1"
                   (string/includes? address ":") (str "[" address "]")
                   :else address)]
    (str "http://" url-host ":" port endpoint)))

(defn start!
  "Starts the hub, resolving to it or rejecting with the listening error,
  EADDRINUSE when the port is taken. `:port` 0 takes any free port."
  [{:keys [address port token version on-agents] :as config}]
  (let [state (atom {:presence presence/empty-presence})
        hub {:state state :config config}
        ;; Reads fade within minutes, so a look every half minute lets go
        ;; of what no longer shows soon after it fades.
        expiry-interval-milliseconds 30000]
    (-> (mcp-http/start! {:address address
                          :port port
                          :token token
                          :endpoint endpoint
                          :describe (fn []
                                      (let [level (reporting hub)]
                                        {:info {"name" "codecloud" "title" "CodeCloud" "version" version}
                                         :instructions (tools/instructions level)
                                         :tools (into (cloud-tools/listed-tools level) tools/wavescope-tools)}))
                          :run-tool (fn [session-id call] (run-tool! hub session-id call))
                          :on-change (fn [sessions]
                                       (note-arrivals! hub sessions)
                                       (when on-agents
                                         (on-agents (present-agents hub))))})
        (task/then (fn [http-server]
                     (swap! state assoc
                            :http http-server
                            :expiry-timer (js/setInterval #(expire-presence! hub false) expiry-interval-milliseconds))
                     (assoc hub :address address :port (:port http-server)))))))

(defn reporting-changed!
  "Tells connected agents to list the tools again after the reporting
  level changed. New instructions reach agents that connect later."
  [{:keys [state]}]
  (when-let [http-server (:http @state)]
    (mcp-http/notify-all! http-server "notifications/tools/list_changed" nil)))

(defn set-token!
  "Accepts only `token` from now on; agents holding the old one are
  refused until connected again."
  [{:keys [state]} token]
  (mcp-http/set-token! (:http @state) token))

(defn stop!
  "Stops serving and ends wavescope, resolving once the server closed."
  [{:keys [state]}]
  (swap! state assoc :stopping? true)
  (some-> (:expiry-timer @state) js/clearInterval)
  (when-let [starting (:wavescope @state)]
    (-> starting
        (task/then mcp-stdio/stop!)
        (task/recover (fn [_error] nil))))
  (if-let [http-server (:http @state)]
    (mcp-http/stop! http-server)
    (task/resolved nil)))

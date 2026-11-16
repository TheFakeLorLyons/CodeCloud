(ns codecloud.agents.roster
  "The agents connected to the hub, one for each MCP session, with what
  each calls itself, when it arrived, when it last spoke, and whether it
  still listens on an event stream. Times are milliseconds, passed in so
  the bookkeeping stays pure.")

(def ^:private client-names
  "How people call the MCP clients that introduce themselves by these
  names in `initialize`."
  {"claude-code" "Claude Code"
   "opencode" "opencode"
   "codex-mcp-client" "Codex"
   "codex" "Codex"
   "Visual Studio Code" "VS Code"
   "Visual Studio Code - Insiders" "VS Code Insiders"
   "cursor-vscode" "Cursor"
   "gemini-cli-mcp-client" "Gemini CLI"
   "mcp-inspector" "MCP Inspector"})

(defn agent-name
  "What to call the agent whose `initialize` carried `client-info`, a map
  with string keys."
  [client-info]
  (let [client (when (map? client-info) client-info)]
    (or (get client-names (get client "name"))
        (not-empty (get client "title"))
        (not-empty (get client "name"))
        "An agent")))

(defn opened
  "The roster with session `session-id`, which an agent describing itself
  with `client-info` just opened in protocol revision `version`."
  [roster session-id client-info version now]
  (assoc roster session-id {:name (agent-name client-info)
                            :client (get client-info "name")
                            :client-version (get client-info "version")
                            :protocol-version version
                            :opened-at now
                            :seen-at now
                            :streams 0
                            :calls 0}))

(defn- revise [roster session-id change]
  (if (contains? roster session-id)
    (update roster session-id change)
    roster))

(defn seen [roster session-id now]
  (revise roster session-id #(assoc % :seen-at now)))

(defn called [roster session-id now]
  (revise roster session-id #(-> % (assoc :seen-at now) (update :calls inc))))

(defn listening
  "The roster after session `session-id` opened an event stream."
  [roster session-id now]
  (revise roster session-id #(-> % (assoc :seen-at now) (update :streams inc))))

(defn stopped-listening
  "The roster after one of session `session-id`'s event streams closed."
  [roster session-id now]
  (revise roster session-id #(-> % (assoc :seen-at now) (update :streams (fn [streams] (max 0 (dec streams)))))))

(defn closed [roster session-id]
  (dissoc roster session-id))

(defn present?
  "True while a session's agent listens on an event stream or has spoken
  within ten minutes. Agents that only send requests never say goodbye,
  so silence is the only sign they left."
  [{:keys [streams seen-at]} now]
  (let [quiet-limit-milliseconds (* 10 60 1000)]
    (or (pos? streams) (< (- now seen-at) quiet-limit-milliseconds))))

(defn forgotten
  "The roster without sessions that have listened on no stream and said
  nothing for a day; an agent coming back after that opens a new
  session."
  [roster now]
  (let [forget-after-milliseconds (* 24 60 60 1000)]
    (into {}
          (remove (fn [[_session-id {:keys [streams seen-at]}]]
                    (and (zero? streams) (> (- now seen-at) forget-after-milliseconds))))
          roster)))

(defn present-agents
  "The agents present at `now`, first arrived first, each {:session
  :name :since :calls}. Agents of the same name are told apart by number,
  as Claude Code and Claude Code 2."
  [roster now]
  (let [present (->> roster
                     (filter (fn [[_session-id session]] (present? session now)))
                     (sort-by (fn [[session-id session]] [(:opened-at session) session-id])))]
    (first
     (reduce (fn [[agents seen-names] [session-id {label :name :keys [opened-at calls]}]]
               (let [occurrence (inc (get seen-names label 0))]
                 [(conj agents {:session session-id
                                :name (if (= 1 occurrence) label (str label " " occurrence))
                                :since opened-at
                                :calls calls})
                  (assoc seen-names label occurrence)]))
             [[] {}]
             present))))

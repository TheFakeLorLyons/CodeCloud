(ns codecloud.vscode.agents
  "CodeCloud's agent hub inside VS Code, with its token in VS Code's
  secret storage, its port remembered for each workspace, the settings
  that start and place it, the status bar item counting agents, VS Code's
  own agents told of it, where agents work as the clouds show it, and the
  command that connects any other MCP client."
  (:require ["crypto" :as crypto]
            ["os" :as os]
            ["path" :as path]
            [clojure.string :as string]
            [codecloud.agents.hub :as hub]
            [codecloud.io.mcp-http :as mcp-http]
            [codecloud.io.task :as task]
            [codecloud.vscode.api :as api]
            [codecloud.vscode.settings :as settings]))

(declare connect-agent!)

(def ^:private token-key "codecloud.agentToken")

(def ^:private port-memory-key "codecloud.agentPort")

(def ^:private server-name
  "The name agents know the hub by, as in Claude Code's mcp__codecloud__ tools."
  "codecloud")

(def ^:private provider-id
  "The MCP server definition provider package.json contributes."
  "codecloud.hub")

(defn create
  "The agents' part of the host's state, before the hub starts."
  []
  (atom {:status :stopped :hub nil :token nil :present [] :problem nil :status-item nil :offer nil}))

;; What the cloud and the status bar show

(defn- summary
  "What a cloud's view needs to show the hub, whether it serves, where,
  the agents present, and what went wrong when it could not start."
  [{:keys [status hub present problem]}]
  {:serving? (= :serving status)
   :url (when (and hub (= :serving status)) (hub/url hub))
   :agents (mapv #(select-keys % [:name :since]) present)
   :reporting (settings/agents-reporting)
   :problem problem})

(defn- clock-time [milliseconds]
  (.toLocaleTimeString (js/Date. milliseconds) js/undefined #js {:hour "numeric" :minute "2-digit"}))

(defn- status-tooltip [{:keys [hub present]}]
  (string/join "\n" (concat [(str "CodeCloud's agent hub at " (hub/url hub))]
                            (if (seq present)
                              (map #(str (:name %) ", here since " (clock-time (:since %))) present)
                              ["No agent is connected."])
                            ["Click to connect an agent."])))

(defn- show-status!
  "Brings the status bar item and every cloud's view up to date."
  [{:keys [agents agent-summary]}]
  (let [{:keys [status present problem status-item] :as current} @agents]
    (when status-item
      (case status
        :serving (api/show-status! status-item {:text (if (seq present) (str "$(radio-tower) " (count present)) "$(radio-tower)")
                                                :tooltip (status-tooltip current)
                                                :command "codecloud.connectAgent"})
        :failed (api/show-status! status-item {:text "$(radio-tower) $(warning)"
                                               :tooltip (str "CodeCloud's agent hub did not start: " problem)
                                               :command "codecloud.connectAgent"})
        (api/hide-status! status-item)))
    (reset! agent-summary (summary current))))

(defn- agents-changed! [host present]
  (when (not= present (:present @(:agents host)))
    (swap! (:agents host) assoc :present present)
    (show-status! host)))

(def ^:private no-presence
  "Where agents work while no hub serves."
  {:agents [] :pins []})

(defn agents-present?
  "True while an agent is connected to the hub."
  [{:keys [agents]}]
  (boolean (seq (:present @agents))))

(defn note-change!
  "Tells the hub of a change no person made, as
  codecloud.agents.hub/note-change! takes it."
  [{:keys [agents]} change]
  (when-let [running (:hub @agents)]
    (hub/note-change! running change)))

(defn dismiss-pin!
  "Takes away pin `pin-id`, which the person dismissed in a cloud."
  [{:keys [agents]} pin-id]
  (when-let [running (:hub @agents)]
    (hub/dismiss-pin! running pin-id)))

;; The token and the port

(defn- fresh-token []
  (let [token-bytes 32]
    (.toString (.randomBytes crypto token-bytes) "hex")))

(defn- token!
  "Resolves to the hub's token, made and stored the first time. Without a
  reachable keychain the token lasts until VS Code closes."
  [{:keys [secrets log!]}]
  (-> (task/resolved nil)
      (task/then (fn [_] (api/secret secrets token-key)))
      (task/then (fn [stored]
                   (or stored
                       (let [made (fresh-token)]
                         (task/then (api/store-secret! secrets token-key made) (fn [_] made))))))
      (task/recover (fn [error]
                      (log! (str "VS Code could not keep the agent token in its secret storage ("
                                 (task/error-text error) "), so the token lasts until VS Code closes."))
                      (fresh-token)))))

(defn- wanted-port
  "The port to ask for, the one the setting names, else the one this
  workspace's hub had before, else 0 for any free port."
  [{:keys [workspace-state]}]
  (let [configured (settings/agents-port)]
    (if (pos? configured)
      configured
      (or (api/remembered workspace-state port-memory-key) 0))))

(defn- wavescope-command
  "How to run the bundled wavescope, with VS Code's own Node, which the
  Electron program running the extension host becomes when
  ELECTRON_RUN_AS_NODE is set."
  [{:keys [extension-uri]}]
  {:command (.-execPath js/process)
   :arguments [(path/join (api/uri-path extension-uri) "dist" "wavescope.mjs")]
   :environment {"ELECTRON_RUN_AS_NODE" "1"}})

(defn- hub-options [{:keys [cloud-context carets presence-summary] :as host} token port]
  {:address (settings/agents-address)
   :port port
   :token token
   :version (:version host)
   :roots api/workspace-folder-paths
   :wavescope (wavescope-command host)
   :cloud #(cloud-context host)
   :carets (fn [] @carets)
   :reporting settings/agents-reporting
   :on-agents #(agents-changed! host %)
   :on-presence #(reset! presence-summary %)
   :log! (:log! host)})

(defn- note-moved-port!
  "Tells the person the hub could not have the port this workspace's
  agents know and offers to keep the new one."
  [{:keys [workspace-state] :as host} wanted running]
  (let [keep-label (str "Keep port " (:port running))]
    (task/then (api/warning-message (str "Port " wanted ", where this workspace's agents reach CodeCloud, is in use, "
                                         "so the agent hub runs at port " (:port running) " for now.")
                                    keep-label "Connect an Agent")
               (fn [choice]
                 (cond
                   (= keep-label choice) (api/remember! workspace-state port-memory-key (:port running))
                   (= "Connect an Agent" choice) (connect-agent! host))))))

(defn- listen!
  "Starts the hub at the wanted port. When another program holds a port
  this workspace's hub had before, the hub takes any free port for now;
  a port the setting names is never swapped for another."
  [{:keys [workspace-state] :as host} token]
  (let [wanted (wanted-port host)
        configured? (pos? (settings/agents-port))]
    (-> (hub/start! (hub-options host token wanted))
        (task/then (fn [running]
                     (when (and (zero? wanted) (not configured?))
                       (api/remember! workspace-state port-memory-key (:port running)))
                     running))
        (task/recover (fn [error]
                        (if (and (= "EADDRINUSE" (.-code error)) (pos? wanted) (not configured?))
                          (task/then (hub/start! (hub-options host token 0))
                                     (fn [running]
                                       (note-moved-port! host wanted running)
                                       running))
                          (task/rejected error)))))))

;; The person's caret

(defn record-caret!
  "Remembers `caret`, {:file :line :column :anchor-line :anchor-column
  :source :at} with positions counted from zero, as where the person's
  caret is now and where it was last in its file, for agents asking."
  [{:keys [carets]} caret]
  (let [most-files 64
        placed (update caret :file path/normalize)
        file (:file placed)]
    (swap! carets (fn [{:keys [by-file]}]
                    (let [kept (if (and (>= (count by-file) most-files) (not (contains? by-file file)))
                                 (dissoc by-file (key (apply min-key (comp :at val) by-file)))
                                 by-file)]
                      {:latest placed :by-file (assoc kept file placed)})))))

(defn watch-carets!
  "Follows the caret in VS Code's editors, starting from the active
  editor's. Returns the disposable that stops following."
  [host]
  (let [record! (fn [{file-path :path :as moved}]
                  (record-caret! host (-> moved
                                          (select-keys [:line :column :anchor-line :anchor-column])
                                          (assoc :file file-path :source :editor :at (js/Date.now)))))]
    (when-let [current (api/active-editor-selection)]
      (record! current))
    (api/on-selection-change! record!)))

;; Starting and stopping

(defn- offer-description
  "The hub as VS Code's agents see it, or nil while it is not serving."
  [{:keys [agents version]}]
  (let [{:keys [status hub token]} @agents]
    (when (= :serving status)
      {:label "CodeCloud"
       :url (hub/url hub)
       :headers {"Authorization" (str "Bearer " token)}
       :version version})))

(defn- told-vscode! [{:keys [agents]}]
  (when-let [changed! (:changed! (:offer @agents))]
    (changed!)))

(defn- start-now!
  "Starts the hub when the settings allow it, reporting the outcome in
  the status bar and the log."
  [{:keys [agents log!] :as host}]
  (if-not (settings/agents-enabled?)
    (do (swap! agents assoc :status :stopped :problem nil :present [])
        (show-status! host)
        (task/resolved nil))
    (do (swap! agents assoc :status :starting :problem nil)
        (-> (token! host)
            (task/then (fn [token]
                         (swap! agents assoc :token token)
                         (listen! host token)))
            (task/then (fn [running]
                         (swap! agents assoc :status :serving :hub running :present [])
                         (log! (str "The agent hub serves MCP at " (hub/url running)
                                    (when-not (mcp-http/loopback-address? (:address running))
                                      (str ", listening on " (:address running) " for other computers"))
                                    "."))
                         (show-status! host)
                         (told-vscode! host)
                         running))
            (task/recover (fn [error]
                            (swap! agents assoc :status :failed :hub nil :problem (task/error-text error))
                            (log! (str "The agent hub did not start: " (task/error-text error)))
                            (show-status! host)
                            nil))))))

(defn- stop-now!
  "Stops the hub, resolving once it closed."
  [{:keys [agents presence-summary] :as host}]
  (let [running (:hub @agents)]
    (swap! agents assoc :status :stopped :hub nil :present [])
    (reset! presence-summary no-presence)
    (show-status! host)
    (told-vscode! host)
    (if running (hub/stop! running) (task/resolved nil))))

(defn- in-turn!
  "Runs `step` once every start and stop asked for earlier has finished,
  so settings changed in quick succession never leave two hubs running."
  [{:keys [agents]} step]
  (let [turn (-> (or (:turn @agents) (task/resolved nil))
                 (task/recover (fn [_error] nil))
                 (task/then (fn [_] (step))))]
    (swap! agents assoc :turn turn)
    turn))

(defn start!
  "Starts the hub, in turn with other starts and stops."
  [host]
  (in-turn! host #(start-now! host)))

(defn stop!
  "Stops the hub, in turn with other starts and stops, resolving once it
  closed."
  [host]
  (in-turn! host #(stop-now! host)))

(defn restart!
  "Stops the hub and starts it again with the current settings."
  [host]
  (in-turn! host #(task/then (stop-now! host) (fn [_] (start-now! host)))))

(defn activate!
  "Creates the status bar item, offers the hub to VS Code's agents, starts
  the hub, and restarts it whenever its settings change. Returns the
  disposables to keep with the extension."
  [{:keys [agents] :as host}]
  (let [status-priority 90 ; among items on the right, a higher priority sits further left
        status-item (api/status-bar-item "codecloud.agents" "CodeCloud agents" status-priority)
        offer (api/offer-mcp-server! provider-id #(offer-description host))]
    (swap! agents assoc :status-item status-item :offer offer)
    (start! host)
    (cond-> [status-item
             (api/on-settings-change! (fn [affects?]
                                        (cond
                                          (some affects? ["codecloud.agents.enabled" "codecloud.agents.address" "codecloud.agents.port"])
                                          (restart! host)

                                          ;; Agents keep their connections and list their tools again.
                                          (affects? "codecloud.agents.reporting")
                                          (do (some-> (:hub @agents) hub/reporting-changed!)
                                              (show-status! host)))))]
      offer (conj (:disposable offer)))))

;; Connecting agents

(defn- copy! [text what-was-copied]
  (task/then (api/write-clipboard! text)
             (fn [_] (api/info-message what-was-copied))))

(defn- claude-code-lines
  "Commands that give Claude Code the hub at its local scope, which keeps
  the token in Claude Code's own configuration outside the repository,
  after removing an earlier entry of the same name."
  [url token]
  [(str "claude mcp remove " server-name " --scope local")
   (str "claude mcp add --transport http --scope local " server-name " " url
        " --header \"Authorization: Bearer " token "\"")])

(defn- pretty-json [data]
  (js/JSON.stringify (clj->js data) nil 2))

(defn- opencode-entry [url token]
  (pretty-json {"$schema" "https://opencode.ai/config.json"
                "mcp" {server-name {"type" "remote"
                                    "url" url
                                    "enabled" true
                                    "headers" {"Authorization" (str "Bearer " token)}}}}))

(defn- codex-table [url token]
  (str "[mcp_servers." server-name "]\n"
       "url = \"" url "\"\n"
       "http_headers = { \"Authorization\" = \"Bearer " token "\" }\n"))

(defn- generic-configuration [url token]
  (pretty-json {"mcpServers" {server-name {"type" "http"
                                           "url" url
                                           "headers" {"Authorization" (str "Bearer " token)}}}}))

(defn- other-computer-urls
  "The hub's URL at each of this computer's IPv4 addresses on a network,
  for agents in virtual machines or on other computers, when the hub
  listens beyond this computer."
  [running]
  (when-not (mcp-http/loopback-address? (:address running))
    (for [[_interface-name addresses] (js->clj (.networkInterfaces os) :keywordize-keys true)
          {:keys [family address internal]} addresses
          :when (and (not internal) (or (= "IPv4" family) (= 4 family)))]
      (str "http://" address ":" (:port running) hub/endpoint))))

(defn- serving-choices [{:keys [hub token]}]
  (let [url (hub/url hub)]
    (concat
     [{:label "$(terminal) Claude Code" :description "Run claude mcp add in a terminal"
       :value [:terminal "Claude Code" (claude-code-lines url token)]}
      {:label "$(copy) Claude Code" :description "Copy the claude mcp add command"
       :value [:copy (last (claude-code-lines url token))
               "Copied the command that connects Claude Code. Run it in the workspace folder."]}
      {:label "$(copy) opencode" :description "Copy an entry for opencode.json"
       :value [:copy (opencode-entry url token)
               "Copied an MCP entry for opencode. Merge it into ~/.config/opencode/opencode.json, outside the repository, since it holds the token."]}
      {:label "$(copy) Codex" :description "Copy a table for ~/.codex/config.toml"
       :value [:copy (codex-table url token) "Copied a table for Codex. Add it to ~/.codex/config.toml."]}
      {:label "$(vscode) VS Code's agents"
       :description (if (api/mcp-servers-supported?) "Offered to them already" "Need VS Code 1.101 or later")
       :value [:vscode]}
      {:separator "Any MCP client"}
      {:label "$(copy) Copy the URL" :description url :value [:copy url "Copied the hub's URL."]}
      {:label "$(copy) Copy the token" :description "Sent as Authorization: Bearer"
       :value [:copy token "Copied the hub's token."]}
      {:label "$(copy) Copy a JSON configuration" :description "The mcpServers form many clients read"
       :value [:copy (generic-configuration url token) "Copied a JSON configuration for the hub."]}]
     (for [remote-url (other-computer-urls hub)]
       {:label "$(copy) Copy the URL for another computer" :description remote-url
        :value [:copy remote-url "Copied the hub's URL on this computer's network."]})
     [{:separator "Hub"}
      {:label "$(key) Replace the token" :description "Agents connected now must connect again" :value [:replace-token]}
      {:label "$(output) Show the hub's log" :value [:log]}
      {:label "$(settings-gear) Settings" :value [:settings]}])))

(defn- stopped-choices [{:keys [status problem]}]
  (concat (if (= :failed status)
            [{:label "$(refresh) Try again" :description problem :value [:restart]}
             {:label "$(output) Show the hub's log" :value [:log]}]
            [{:label "$(radio-tower) Turn the agent hub on" :value [:enable]}])
          [{:label "$(settings-gear) Settings" :value [:settings]}]))

(defn- connect-placeholder [{:keys [status present problem]}]
  (case status
    :serving (if (seq present)
               (str (count present) (if (= 1 (count present)) " agent" " agents") " connected: "
                    (string/join ", " (map :name present)))
               "No agent is connected yet")
    :failed (str "The agent hub did not start: " problem)
    :starting "The agent hub is starting"
    "The agent hub is off"))

(defn- replace-token! [{:keys [agents secrets log!] :as host}]
  (task/then (api/confirm "Replace the agent hub's token?"
                          "Every agent connected now is refused until connected again with the new token."
                          "Replace")
             (fn [confirmed?]
               (when confirmed?
                 (let [token (fresh-token)]
                   (-> (api/store-secret! secrets token-key token)
                       (task/recover (fn [error]
                                       (log! (str "VS Code could not store the new token (" (task/error-text error)
                                                  "), so it lasts until VS Code closes."))))
                       (task/then (fn [_]
                                    (swap! agents assoc :token token)
                                    (when-let [running (:hub @agents)]
                                      (hub/set-token! running token))
                                    (told-vscode! host)
                                    (task/then (api/info-message "Replaced the token. Connect agents again with the new one."
                                                                 "Connect an Agent")
                                               (fn [choice]
                                                 (when choice (connect-agent! host))))))))))))

(defn- act!
  "Carries out one choice from Connect an Agent."
  [{:keys [channel] :as host} [action & details]]
  (case action
    :terminal (let [[terminal-name command-lines] details]
                (api/run-in-terminal! terminal-name (first (api/workspace-folder-paths)) command-lines))
    :copy (let [[text what-was-copied] details]
            (copy! text what-was-copied))
    :vscode (api/info-message (if (api/mcp-servers-supported?)
                                (str "VS Code's agents list CodeCloud among their MCP servers. "
                                     "Start it from MCP: List Servers if it is not running.")
                                (str "This VS Code is older than 1.101, the first that lets extensions offer MCP servers. "
                                     "Add the hub by hand with Copy a JSON configuration.")))
    :replace-token (replace-token! host)
    :log (api/show-channel! channel)
    :settings (api/execute-command "workbench.action.openSettings" "codecloud.agents")
    :restart (restart! host)
    :enable (api/update-setting! "codecloud" "agents.enabled" true)
    nil))

(defn connect-agent!
  "Offers the ways to connect an agent to the hub and carries out the one
  chosen."
  [{:keys [agents] :as host}]
  (let [current @agents]
    (task/then (api/choose "Connect an agent to CodeCloud"
                           (connect-placeholder current)
                           (if (= :serving (:status current)) (serving-choices current) (stopped-choices current)))
               (fn [choice]
                 (when choice (act! host choice))))))

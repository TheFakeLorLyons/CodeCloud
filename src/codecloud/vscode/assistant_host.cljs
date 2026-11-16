(ns codecloud.vscode.assistant-host
  "CodeCloud's assistant inside VS Code. The providers the person added
  keep their keys in VS Code's secret storage, pickers set them and the
  model chosen, and each cloud's conversation runs through the agent
  hub's tools as one more agent, posted to the cloud's view as it
  streams."
  (:require [clojure.string :as string]
            [codecloud.agents.hub :as hub]
            [codecloud.agents.tools :as tools]
            [codecloud.assistant.conversation :as conversation]
            [codecloud.assistant.providers :as providers]
            [codecloud.assistant.runner :as runner]
            [codecloud.io.mcp-client :as mcp-client]
            [codecloud.io.task :as task]
            [codecloud.protocol :as protocol]
            [codecloud.vscode.api :as api]
            [codecloud.vscode.settings :as settings]))

(declare choose-model!)

(def ^:private providers-key "codecloud.assistant.providers")

(def ^:private choice-key "codecloud.assistant.model")

(defn- key-name
  "Where VS Code's secret storage keeps provider `provider-id`'s key."
  [provider-id]
  (str "codecloud.assistant.key:" provider-id))

(defn create
  "The assistant's part of the host's state, holding the providers added,
  each {:id :kind :label :base-url}, and the model chosen, {:provider-id
  :model}, as `global-state` remembers them for every workspace."
  [global-state]
  (atom {:providers (vec (or (api/remembered global-state providers-key) []))
         :choice (api/remembered global-state choice-key)}))

(defn- save! [{:keys [global-state assistant-settings]} change]
  (let [{:keys [providers choice]} (swap! assistant-settings change)]
    (task/all [(api/remember! global-state providers-key providers)
               (api/remember! global-state choice-key choice)])))

(defn- chosen [{:keys [assistant-settings]}]
  (let [{:keys [providers choice]} @assistant-settings
        provider (some #(when (= (:provider-id choice) (:id %)) %) providers)]
    (when (and provider (seq (:model choice)))
      {:provider provider :model (:model choice)})))

(defn model-label
  "The model chosen and its provider, as the assistant pane names them,
  or nil."
  [host]
  (when-let [{:keys [provider model]} (chosen host)]
    (str model " from " (:label provider))))

;; Choosing a model

(defn- choose! [host provider model-id]
  (task/then (save! host #(assoc % :choice {:provider-id (:id provider) :model model-id}))
             (fn [_] (api/status-message! (str "The assistant uses " model-id " from " (:label provider) ".") 4000))))

(defn- type-model! [host provider]
  (task/then (api/input-box {:title (str "A model from " (:label provider))
                             :prompt "The model's name, as the provider knows it"
                             :validate #(when (string/blank? %) "Type the model's name.")})
             (fn [model-id]
               (when (seq model-id) (choose! host provider (string/trim model-id))))))

(defn- address-problem [text]
  (when-not (re-find #"^https?://[^\s/]+" (string/trim (str text)))
    "The address starts with http:// or https://, as http://localhost:8080/v1."))

(defn- provider-label
  "What to call a provider added from `preset` at `base-url`, the preset's
  name, or for another server the host its address names, numbered when
  the same name is taken."
  [preset base-url taken]
  (let [named (if (= :custom (:preset preset))
                (or (second (re-find #"^https?://([^/]+)" base-url)) base-url)
                (:label preset))
        twins (count (filter #(= named (string/replace (:label %) #" \d+$" "")) taken))]
    (if (zero? twins) named (str named " " (inc twins)))))

(defn- key-for!
  "Resolves to the key typed for `preset`, \"\" for a provider that needs
  none or one left blank, or nil when the person gave up."
  [preset]
  (if (= :none (:key preset))
    (task/resolved "")
    (api/input-box {:title (str (:label preset) " API key")
                    :prompt "VS Code's secret storage keeps the key, out of the settings and the repository."
                    :password? true
                    :validate (when (= :required (:key preset)) #(when (string/blank? %) "This provider needs a key."))})))

(defn- add-provider!
  "Adds a provider picked from the presets, asking for an address where
  the preset has none and for a key where the provider takes one, then
  offers the models."
  [{:keys [secrets assistant-settings] :as host}]
  (task/then
   (api/choose "Add a provider for the assistant" "Choose a provider"
               (for [{key-need :key :keys [label base-url] :as preset} providers/presets]
                 {:label label
                  :description (or base-url "Its address next")
                  :detail (case key-need
                            :required "Needs an API key"
                            :optional "Takes an API key when the server asks for one"
                            "Needs no key")
                  :value preset}))
   (fn [preset]
     (when preset
       (task/then
        (if (:base-url preset)
          (task/resolved (:base-url preset))
          (api/input-box {:title (:label preset)
                          :prompt "The server's address, up to the API's version, as http://localhost:8080/v1"
                          :validate address-problem}))
        (fn [base-url]
          (when (seq base-url)
            (task/then
             (key-for! preset)
             (fn [api-key]
               (when (some? api-key)
                 (let [provider {:id (str (random-uuid))
                                 :kind (:kind preset)
                                 :label (provider-label preset (string/trim base-url) (:providers @assistant-settings))
                                 :base-url (string/trim base-url)}]
                   (-> (if (string/blank? api-key)
                         (task/resolved nil)
                         (api/store-secret! secrets (key-name (:id provider)) (string/trim api-key)))
                       (task/then (fn [_] (save! host #(update % :providers conj provider))))
                       (task/then (fn [_] (choose-model! host)))))))))))))))

(defn- remove-provider! [{:keys [secrets assistant-settings] :as host}]
  (task/then
   (api/choose "Remove a provider" "Choose the provider to remove"
               (for [{:keys [label base-url] :as provider} (:providers @assistant-settings)]
                 {:label label :description base-url :value provider}))
   (fn [provider]
     (when provider
       (-> (api/delete-secret! secrets (key-name (:id provider)))
           (task/recover (fn [_error] nil))
           (task/then (fn [_]
                        (save! host (fn [settings]
                                      (cond-> (update settings :providers (fn [kept] (filterv #(not= (:id provider) (:id %)) kept)))
                                        (= (:id provider) (get-in settings [:choice :provider-id])) (assoc :choice nil)))))))))))

(defn- model-items
  "Resolves to the picker's items, each provider's models under its name
  with the model chosen marked, a way to type a model's name where none
  could be listed, and the ways to add and remove providers."
  [{:keys [secrets assistant-settings]}]
  (let [{:keys [providers choice]} @assistant-settings]
    (task/then
     (task/all (for [provider providers]
                 (-> (api/secret secrets (key-name (:id provider)))
                     (task/then (fn [api-key] (runner/list-models! provider api-key)))
                     (task/then (fn [models] {:provider provider :models models}))
                     (task/recover (fn [error] {:provider provider :models [] :problem (task/error-text error)})))))
     (fn [listed]
       (concat
        (mapcat (fn [{:keys [provider models problem]}]
                  (concat [{:separator (:label provider)}]
                          (for [{:keys [id label]} models
                                :let [current? (and (= (:id provider) (:provider-id choice)) (= id (:model choice)))]]
                            {:label (str (when current? "$(check) ") id)
                             :description (when (not= label id) label)
                             :value [:model provider id]})
                          (when (or problem (empty? models))
                            [{:label "$(edit) Type a model's name"
                              :description (or problem "The provider listed no models")
                              :value [:type-model provider]}])))
                listed)
        [{:separator "Providers"}
         {:label "$(add) Add a provider" :description "Anthropic, OpenAI, OpenRouter, Ollama, LM Studio, or another server"
          :value [:add]}]
        (when (seq providers)
          [{:label "$(trash) Remove a provider" :value [:remove]}]))))))

(defn choose-model!
  "Offers the models of every provider added, with the ways to add and
  remove providers, and carries out the choice. With no provider yet,
  adding one comes first."
  [{:keys [assistant-settings] :as host}]
  (if (empty? (:providers @assistant-settings))
    (add-provider! host)
    (task/then (api/choose-from "The assistant's model" "Choose a model, or add a provider" (model-items host))
               (fn [[action provider model-id]]
                 (case action
                   :model (choose! host provider model-id)
                   :type-model (type-model! host provider)
                   :add (add-provider! host)
                   :remove (remove-provider! host)
                   nil)))))

;; A cloud's conversation

(defn create-chat
  "A cloud's conversation with the assistant before the first question,
  holding the conversation itself, replaced whole when the person starts
  over, and what runs it."
  []
  (atom {:conversation (atom conversation/empty-conversation)
         :run nil
         :controller nil
         :stopped? false
         :client nil
         :client-title nil
         :tools nil
         :last-post 0
         :post-timer nil}))

(defn- hub-serving? [{:keys [agents]}]
  (= :serving (:status @agents)))

(defn- state-message
  "The conversation as the cloud's view shows it."
  [host chat]
  (let [{:keys [conversation run]} @chat]
    (protocol/message :host/assistant {:entries (conversation/shown @conversation)
                                       :running? (some? run)
                                       :model (model-label host)
                                       :ready? (some? (chosen host))
                                       :hub? (hub-serving? host)})))

(defn announce!
  "Sends the cloud's view the conversation."
  [{:keys [host chat post!]}]
  (post! (state-message host chat)))

(defn- announce-soon!
  "Sends the conversation now, or once a moment passed since the last
  time, so a streaming reply reaches the view a few times a second."
  [{:keys [chat] :as context}]
  (let [spacing-milliseconds 80
        now (js/Date.now)
        since (- now (:last-post @chat))]
    (cond
      (>= since spacing-milliseconds) (do (swap! chat assoc :last-post now)
                                          (announce! context))
      (nil? (:post-timer @chat)) (swap! chat assoc :post-timer
                                        (js/setTimeout (fn []
                                                         (swap! chat assoc :post-timer nil :last-post (js/Date.now))
                                                         (announce! context))
                                                       (- spacing-milliseconds since))))))

(defn- hub-client!
  "Resolves to {:client :tools} for the agent hub, connected as `title`,
  the session held kept while it answers and has that title, or to nil
  while the hub does not serve."
  [{:keys [host chat]} title]
  (let [{:keys [status token] running :hub} @(:agents host)
        {:keys [client client-title]} @chat
        connect! (fn []
                   (-> (if client (mcp-client/close! client) (task/resolved nil))
                       (task/then (fn [_]
                                    (mcp-client/connect! {:url (hub/url running)
                                                          :token token
                                                          :client-info {"name" "codecloud-assistant" "title" title
                                                                        "version" (or (:version host) "")}})))
                       (task/then (fn [connected]
                                    (task/then (mcp-client/list-tools! connected)
                                               (fn [listed]
                                                 (swap! chat assoc :client connected :client-title title :tools listed)
                                                 {:client connected :tools listed}))))))]
    (cond
      (not= :serving status) (task/resolved nil)
      (and client (= title client-title)) (-> (mcp-client/list-tools! client)
                                              (task/then (fn [listed]
                                                           (swap! chat assoc :tools listed)
                                                           {:client client :tools listed}))
                                              (task/recover (fn [_error] (connect!))))
      :else (connect!))))

(defn ask!
  "Runs the person's question through the assistant and the hub's tools,
  posting the conversation as it changes. One question runs at a time,
  and with no model chosen the picker opens."
  [{:keys [host chat cloud-name] :as context} text]
  (let [question (string/trim (str text))
        choice (chosen host)]
    (cond
      (or (string/blank? question) (:run @chat)) nil
      (nil? choice) (do (swap! (:conversation @chat) conversation/noted "Choose a model for the assistant first.")
                        (announce! context)
                        (choose-model! host))
      :else
      (let [{:keys [provider model]} choice
            run-id (str (random-uuid))
            ^js controller (js/AbortController.)
            held (:conversation @chat)
            ended! (fn []
                     (when (= run-id (:run @chat))
                       (swap! chat assoc :run nil :controller nil))
                     (announce! context))]
        (swap! held conversation/asked question)
        (swap! chat assoc :run run-id :controller controller :stopped? false)
        (announce! context)
        (-> (task/all [(api/secret (:secrets host) (key-name (:id provider)))
                       (task/recover (hub-client! context (str "Assistant (" model ")"))
                                     (fn [error] {:problem (task/error-text error)}))])
            (task/then
             (fn [[api-key {:keys [client problem] listed :tools}]]
               (when problem
                 (swap! held conversation/noted
                        (str "The assistant could not reach the agent hub (" problem "), so it answers without CodeCloud's tools.")))
               (let [definitions (if client (conversation/tool-definitions listed) [])
                     system (conversation/system-prompt (when client (tools/instructions (settings/agents-reporting)))
                                                        cloud-name
                                                        (boolean (some #(= "point_out" (:name %)) definitions)))]
                 (runner/answer!
                  {:conversation held
                   :on-change (fn [_] (announce-soon! context))
                   :ask! (fn [turns on-progress]
                           (task/then (runner/stream-reply! provider
                                                            (providers/reply-request provider api-key model
                                                                                     {:system system :turns turns :tools definitions
                                                                                      :max-tokens conversation/reply-token-cap
                                                                                      :cache-lifetime (settings/assistant-cache-lifetime)})
                                                            (.-signal controller)
                                                            on-progress)
                                      (fn [reply]
                                        (when-let [usage (providers/usage-words (:usage reply))]
                                          ((:log! host) (str "Assistant, " model ": " usage ".")))
                                        reply)))
                   :run-tool! (fn [tool-name input]
                                (task/recover (mcp-client/call-tool! client tool-name input)
                                              (fn [error]
                                                (swap! chat assoc :client nil)
                                                (task/rejected error))))
                   :titles (into {} (map (juxt :name :title)) definitions)
                   :stopped? #(or (:stopped? @chat) (not= run-id (:run @chat)))}))))
            (task/recover (fn [error]
                            (swap! held conversation/noted (task/error-text error))))
            (task/then (fn [_] (ended!))))))))

(defn stop!
  "Stops the question running, keeping what it said so far."
  [{:keys [chat]}]
  (when-let [^js controller (:controller @chat)]
    (swap! chat assoc :stopped? true)
    (.abort controller)))

(defn start-over!
  "Stops any question running and begins a new conversation."
  [{:keys [chat] :as context}]
  (stop! context)
  (swap! chat assoc :conversation (atom conversation/empty-conversation) :run nil :controller nil)
  (announce! context))

(defn close!
  "Stops the conversation's question and ends its session with the hub,
  as its cloud closes."
  [{:keys [chat] :as context}]
  (stop! context)
  (some-> (:post-timer @chat) js/clearTimeout)
  (when-let [client (:client @chat)]
    (swap! chat assoc :client nil)
    (mcp-client/close! client)))

(ns codecloud.assistant.providers
  "The model providers CodeCloud's assistant talks to, kept pure. They are
  the Anthropic Messages API and any service speaking OpenAI's chat
  completions API, as OpenAI, OpenRouter, Ollama, and LM Studio do. Here
  are their presets, the requests that list models and ask for a reply,
  the turns of a conversation in each API's shape, and the folding of a
  streamed reply into its text and tool calls. JSON arrives parsed, as
  maps with string keys, and leaves the same way."
  (:require [clojure.string :as string]))

(def presets
  "The providers offered when one is added, in the order offered. :key
  says whether the provider needs an API key, takes one if given, or
  needs none, and a preset without :base-url asks for one."
  [{:preset :anthropic :kind :anthropic :label "Anthropic" :base-url "https://api.anthropic.com" :key :required}
   {:preset :openai :kind :openai-chat :label "OpenAI" :base-url "https://api.openai.com/v1" :key :required}
   {:preset :openrouter :kind :openai-chat :label "OpenRouter" :base-url "https://openrouter.ai/api/v1" :key :required}
   {:preset :ollama :kind :openai-chat :label "Ollama on this computer" :base-url "http://localhost:11434/v1" :key :none}
   {:preset :lm-studio :kind :openai-chat :label "LM Studio on this computer" :base-url "http://localhost:1234/v1" :key :none}
   {:preset :custom :kind :openai-chat :label "Another OpenAI-compatible server" :key :optional}])

(def anthropic-version
  "The version of the Messages API the requests ask for."
  "2023-06-01")

(defn- without-trailing-slashes [url]
  (string/replace (str url) #"/+$" ""))

(defn- headers-for
  "The headers of every request to a provider of `kind` holding
  `api-key`, which a local server may lack."
  [kind api-key]
  (case kind
    :anthropic (cond-> {"anthropic-version" anthropic-version "content-type" "application/json"}
                 (seq api-key) (assoc "x-api-key" api-key))
    :openai-chat (cond-> {"content-type" "application/json"}
                   (seq api-key) (assoc "authorization" (str "Bearer " api-key)))))

;; Models

(defn models-request
  "The request listing the models `provider` offers: {:method :url
  :headers}."
  [{:keys [kind base-url]} api-key]
  (let [most-listed 100]
    {:method "GET"
     :url (case kind
            :anthropic (str (without-trailing-slashes base-url) "/v1/models?limit=" most-listed)
            :openai-chat (str (without-trailing-slashes base-url) "/models"))
     :headers (headers-for kind api-key)}))

(def ^:private other-than-chat
  "Ids of the models an OpenAI-style list mixes in that cannot hold a
  conversation, such as those for embeddings, speech, transcription,
  images, and moderation."
  #"(?i)embed|whisper|tts|transcribe|dall-e|moderation|image")

(defn model-list
  "The models a models request for `provider` answered with, as [{:id
  :label}], Anthropic's newest first as it lists them and an OpenAI-style
  list by name, without its other kinds of model."
  [{:keys [kind]} answer]
  (let [listed (filter #(string? (get % "id")) (get answer "data"))]
    (case kind
      :anthropic (mapv (fn [model] {:id (get model "id") :label (or (not-empty (get model "display_name")) (get model "id"))})
                       listed)
      :openai-chat (->> listed
                        (remove #(re-find other-than-chat (get % "id")))
                        (map (fn [model] {:id (get model "id") :label (or (not-empty (get model "name")) (get model "id"))}))
                        (sort-by (comp string/lower-case :label))
                        vec))))

;; Turns

(defn- anthropic-messages
  "A turn as Anthropic messages, carrying the person's text, the
  assistant's text and tool uses, or tool results, which travel in a user
  message. An assistant turn with nothing in it is left out."
  [{:keys [role text tool-calls results]}]
  (case role
    :user [{"role" "user" "content" text}]
    :assistant (let [content (vec (concat (when (seq text) [{"type" "text" "text" text}])
                                          (for [{:keys [id name input]} tool-calls]
                                            {"type" "tool_use" "id" id "name" name "input" (or input {})})))]
                 (if (seq content) [{"role" "assistant" "content" content}] []))
    :tool [{"role" "user"
            "content" (vec (for [{:keys [id text error?]} results]
                             (cond-> {"type" "tool_result" "tool_use_id" id "content" text}
                               error? (assoc "is_error" true))))}]))

(defn- openai-messages
  "A turn as OpenAI chat messages, carrying the person's text, the
  assistant's text with its tool calls, their arguments sent back as the
  model wrote them, or one tool message for each result."
  [{:keys [role text tool-calls results]}]
  (case role
    :user [{"role" "user" "content" text}]
    :assistant (if (or (seq text) (seq tool-calls))
                 [(cond-> {"role" "assistant" "content" (not-empty text)}
                    (seq tool-calls) (assoc "tool_calls" (vec (for [{:keys [id name input-json]} tool-calls]
                                                                {"id" id
                                                                 "type" "function"
                                                                 "function" {"name" name "arguments" (or input-json "{}")}}))))]
                 [])
    :tool (vec (for [{:keys [id text]} results]
                 {"role" "tool" "tool_call_id" id "content" text}))))

(defn reply-request
  "The streamed request for `model`'s next reply to `turns`, as {:method
  :url :headers :body}, `tools` being {:name :description :schema}.
  Anthropic caches the conversation for `cache-lifetime`, 5m or 1h."
  [{:keys [kind base-url]} api-key model {:keys [system turns tools max-tokens cache-lifetime]}]
  (case kind
    :anthropic
    {:method "POST"
     :url (str (without-trailing-slashes base-url) "/v1/messages")
     :headers (headers-for kind api-key)
     :body (cond-> {"model" model
                    "max_tokens" max-tokens
                    "system" system
                    "messages" (into [] (mapcat anthropic-messages) turns)
                    "stream" true
                    "cache_control" (cond-> {"type" "ephemeral"} (= "1h" cache-lifetime) (assoc "ttl" "1h"))}
             (seq tools) (assoc "tools" (mapv (fn [{:keys [name description schema]}]
                                                {"name" name "description" description "input_schema" schema})
                                              tools)))}

    :openai-chat
    {:method "POST"
     :url (str (without-trailing-slashes base-url) "/chat/completions")
     :headers (headers-for kind api-key)
     :body (cond-> {"model" model
                    "messages" (into [{"role" "system" "content" system}] (mapcat openai-messages) turns)
                    "stream" true}
             (seq tools) (assoc "tools" (mapv (fn [{:keys [name description schema]}]
                                                {"type" "function"
                                                 "function" {"name" name "description" description "parameters" schema}})
                                              tools)))}))

;; Streamed replies

(defn- event-of
  "One server-sent event from its lines, as {:event :data}, or nil when
  it carries no data."
  [block]
  (let [{:keys [event data]} (reduce (fn [found line]
                                       (cond
                                         (string/starts-with? line "data:") (update found :data conj (string/replace-first (subs line 5) #"^ " ""))
                                         (string/starts-with? line "event:") (assoc found :event (string/trim (subs line 6)))
                                         :else found))
                                     {:event nil :data []}
                                     (string/split-lines block))]
    (when (seq data)
      {:event event :data (string/join "\n" data)})))

(defn sse-events
  "[events rest] from streamed `text`, each complete server-sent event as
  {:event :data}, and the unfinished text left to join the next chunk."
  [text]
  (let [parts (string/split (string/replace text "\r\n" "\n") #"\n\n" -1)]
    [(into [] (keep event-of) (butlast parts)) (last parts)]))

(def empty-reply
  "A reply before any of its stream arrives, with room for its text, the
  tool calls still arriving by their index, why it stopped, what went
  wrong, and the tokens it used, as the provider counts them."
  {:text "" :calls {} :stop nil :error nil :usage nil})

(defn- text-block-started
  "`text` as a new block of text begins with `opening`, a blank line set
  between the blocks when the text before ends mid-line."
  [text opening]
  (str (if (and (seq text) (not (re-find #"\s$" text))) (str text "\n\n") text) opening))

(defn anthropic-event
  "The reply after one event of an Anthropic stream, `data` its parsed
  JSON."
  [reply data]
  (let [index (get data "index")]
    (case (get data "type")
      "message_start" (assoc reply :usage (get-in data ["message" "usage"]))
      "content_block_start" (let [block (get data "content_block")]
                              (case (get block "type")
                                "tool_use" (assoc-in reply [:calls index] {:id (get block "id") :name (get block "name") :input-json ""})
                                "text" (update reply :text text-block-started (get block "text" ""))
                                reply))
      "content_block_delta" (let [delta (get data "delta")]
                              (case (get delta "type")
                                "text_delta" (update reply :text str (get delta "text"))
                                "input_json_delta" (update-in reply [:calls index :input-json] str (get delta "partial_json"))
                                reply))
      "message_delta" (cond-> reply
                        (get-in data ["delta" "stop_reason"]) (assoc :stop (get-in data ["delta" "stop_reason"]))
                        (map? (get data "usage")) (update :usage merge (get data "usage")))
      "error" (assoc reply :error (or (get-in data ["error" "message"]) "The provider reported an error."))
      reply)))

(defn openai-chunk
  "The reply after one chunk of an OpenAI-style stream, `data` its
  parsed JSON. Tool calls arrive in pieces by index, the first piece
  carrying the call's id and name."
  [reply data]
  (if-let [problem (get data "error")]
    (assoc reply :error (or (when (map? problem) (get problem "message")) (str problem)))
    (let [choice (first (get data "choices"))
          delta (get choice "delta")
          content (get delta "content")
          finish (get choice "finish_reason")]
      (cond-> (reduce (fn [so-far {call-index "index" call-id "id" {call-name "name" arguments "arguments"} "function"}]
                        (let [index (or call-index 0)]
                          (update-in so-far [:calls index]
                                     (fn [call]
                                       (cond-> (merge {:id nil :name nil :input-json ""} call)
                                         call-id (assoc :id call-id)
                                         call-name (assoc :name call-name)
                                         arguments (update :input-json str arguments))))))
                      reply
                      (get delta "tool_calls"))
        (string? content) (update :text str content)
        finish (assoc :stop finish)))))

(defn completed
  "A streamed reply once its stream ended, as {:text :tool-calls :stop
  :error :usage} with each tool call {:id :name :input-json} in the order
  the model gave them, a call without an id given one from its place."
  [{:keys [text calls stop error usage]}]
  {:text text
   :tool-calls (vec (for [[index {:keys [id name input-json]}] (sort-by key calls)
                          :when (seq name)]
                      {:id (or id (str "call-" index))
                       :name name
                       :input-json (if (string/blank? input-json) "{}" input-json)}))
   :stop stop
   :error error
   :usage usage})

(defn whole-reply
  "A reply that came back whole, from a server that does not stream, as
  `completed` makes one; an Anthropic tool use carries its :input
  already parsed."
  [kind body]
  (case kind
    :anthropic (let [content (get body "content")]
                 {:text (string/join "\n\n" (keep #(when (= "text" (get % "type")) (get % "text")) content))
                  :tool-calls (vec (for [block content
                                         :when (= "tool_use" (get block "type"))]
                                     {:id (get block "id") :name (get block "name") :input (or (get block "input") {})}))
                  :stop (get body "stop_reason")
                  :error nil
                  :usage (get body "usage")})
    :openai-chat (let [{message "message" finish "finish_reason"} (first (get body "choices"))]
                   {:text (or (get message "content") "")
                    :tool-calls (vec (map-indexed (fn [index {call-id "id" {call-name "name" arguments "arguments"} "function"}]
                                                    {:id (or call-id (str "call-" index))
                                                     :name call-name
                                                     :input-json (if (string/blank? arguments) "{}" arguments)})
                                                  (get message "tool_calls")))
                   :stop finish
                   :error (some-> (get body "error") (get "message"))})))

(defn usage-words
  "What a reply's Anthropic `usage` counted, in words for the log, or nil
  without a count."
  [usage]
  (when (map? usage)
    (let [tokens #(or (get usage %) 0)]
      (str (tokens "cache_read_input_tokens") " input tokens read from the cache, "
           (tokens "cache_creation_input_tokens") " written to it, "
           (tokens "input_tokens") " uncached, and "
           (tokens "output_tokens") " out"))))

(defn error-message
  "What a provider's error answer says, its status and the message in its
  parsed body, where there is one."
  [status body]
  (let [detail (cond
                 (string? (get-in body ["error" "message"])) (get-in body ["error" "message"])
                 (string? (get body "error")) (get body "error")
                 (string? (get body "message")) (get body "message"))]
    (str "The provider answered " status
         (case status
           401 ", refusing the key"
           403 ", refusing the request"
           404 ", finding nothing there"
           429 ", asking to slow down"
           nil)
         (if detail (str ": " detail) "."))))

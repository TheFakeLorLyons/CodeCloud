(ns codecloud.assistant.stand-ins
  "Stand-ins for an Anthropic server and an OpenAI-style server, for
  tests, speaking the parts of each API the assistant uses. Each lists
  two models for the key \"test-key\", and answers a conversation by
  calling cloud_overview first, then, once the tool answered, with a
  reply quoting the tool's first line, streamed in pieces."
  (:require ["http" :as http]
            [clojure.string :as string]))

(def test-key "test-key")

(defn- send-json! [^js response status data]
  (.writeHead response status #js {"content-type" "application/json"})
  (.end response (js/JSON.stringify (clj->js data))))

(defn- send-events!
  "Streams `events`, each [event-name data], as server-sent events, a
  piece at a time."
  [^js response events]
  (.writeHead response 200 #js {"content-type" "text/event-stream" "cache-control" "no-cache"})
  (doseq [[event-name data] events]
    (.write response (str (when event-name (str "event: " event-name "\n"))
                          "data: " (if (string? data) data (js/JSON.stringify (clj->js data))) "\n\n")))
  (.end response))

(defn- tool-answer
  "The first line of the tool's answer in the conversation, or nil while
  no tool has answered."
  [messages]
  (some (fn [message]
          (let [content (get message "content")]
            (cond
              (= "tool" (get message "role")) (first (string/split-lines (str content)))
              (sequential? content) (some #(when (= "tool_result" (get % "type")) (first (string/split-lines (str (get % "content")))))
                                          content))))
        (reverse messages)))

(defn- usage
  "What a request reports it used. A cached request writes its first turn
  to the cache and reads it back on the next."
  [cached? later-turn?]
  (let [prefix 600]
    {"input_tokens" (if cached? 40 (+ prefix 40))
     "cache_creation_input_tokens" (if (and cached? (not later-turn?)) prefix 0)
     "cache_read_input_tokens" (if (and cached? later-turn?) prefix 0)
     "output_tokens" 1}))

(defn- anthropic-events [messages cached?]
  (if-let [answer (tool-answer messages)]
    [["message_start" {"type" "message_start" "message" {"id" "msg_2" "usage" (usage cached? true)}}]
     ["content_block_start" {"type" "content_block_start" "index" 0 "content_block" {"type" "text" "text" ""}}]
     ["content_block_delta" {"type" "content_block_delta" "index" 0 "delta" {"type" "text_delta" "text" "The overview says: "}}]
     ["content_block_delta" {"type" "content_block_delta" "index" 0 "delta" {"type" "text_delta" "text" answer}}]
     ["content_block_stop" {"type" "content_block_stop" "index" 0}]
     ["message_delta" {"type" "message_delta" "delta" {"stop_reason" "end_turn"}}]
     ["message_stop" {"type" "message_stop"}]]
    [["message_start" {"type" "message_start" "message" {"id" "msg_1" "usage" (usage cached? false)}}]
     ["content_block_start" {"type" "content_block_start" "index" 0 "content_block" {"type" "text" "text" ""}}]
     ["content_block_delta" {"type" "content_block_delta" "index" 0 "delta" {"type" "text_delta" "text" "Let me "}}]
     ["ping" {"type" "ping"}]
     ["content_block_delta" {"type" "content_block_delta" "index" 0 "delta" {"type" "text_delta" "text" "look."}}]
     ["content_block_stop" {"type" "content_block_stop" "index" 0}]
     ["content_block_start" {"type" "content_block_start" "index" 1
                             "content_block" {"type" "tool_use" "id" "toolu_1" "name" "cloud_overview" "input" {}}}]
     ["content_block_delta" {"type" "content_block_delta" "index" 1 "delta" {"type" "input_json_delta" "partial_json" "{\"limit\""}}]
     ["content_block_delta" {"type" "content_block_delta" "index" 1 "delta" {"type" "input_json_delta" "partial_json" ": 3}"}}]
     ["content_block_stop" {"type" "content_block_stop" "index" 1}]
     ["message_delta" {"type" "message_delta" "delta" {"stop_reason" "tool_use"}}]
     ["message_stop" {"type" "message_stop"}]]))

(defn- openai-events [messages]
  (if-let [answer (tool-answer messages)]
    [[nil {"choices" [{"delta" {"role" "assistant" "content" "The overview says: "}}]}]
     [nil {"choices" [{"delta" {"content" answer}}]}]
     [nil {"choices" [{"delta" {} "finish_reason" "stop"}]}]
     [nil "[DONE]"]]
    [[nil {"choices" [{"delta" {"role" "assistant" "content" "Let me "}}]}]
     [nil {"choices" [{"delta" {"content" "look."}}]}]
     [nil {"choices" [{"delta" {"tool_calls" [{"index" 0 "id" "call_1" "type" "function"
                                                "function" {"name" "cloud_overview" "arguments" "{\"limit\""}}]}}]}]
     [nil {"choices" [{"delta" {"tool_calls" [{"index" 0 "function" {"arguments" ": 3}"}}]}}]}]
     [nil {"choices" [{"delta" {} "finish_reason" "tool_calls"}]}]
     [nil "[DONE]"]]))

(defn- read-body [^js request on-body]
  (let [chunks #js []]
    (.on request "data" #(.push chunks %))
    (.on request "end" #(on-body (let [text (.toString (js/Buffer.concat chunks) "utf8")]
                                   (when (seq text) (js->clj (js/JSON.parse text))))))))

(defn- handle! [requests ^js request ^js response]
  (let [url (.-url request)
        headers (js->clj (.-headers request))
        anthropic-key (get headers "x-api-key")
        bearer (some->> (get headers "authorization") (re-find #"^Bearer (.+)$") second)]
    (read-body request
               (fn [body]
                 (swap! requests conj {:url url :method (.-method request) :headers headers :body body})
                 (cond
                   (string/starts-with? url "/anthropic/")
                   (cond
                     (not= test-key anthropic-key) (send-json! response 401 {"type" "error" "error" {"type" "authentication_error" "message" "invalid x-api-key"}})
                     (not= "2023-06-01" (get headers "anthropic-version")) (send-json! response 400 {"type" "error" "error" {"message" "missing anthropic-version"}})
                     (string/starts-with? url "/anthropic/v1/models") (send-json! response 200 {"data" [{"id" "claude-test-large" "display_name" "Claude Test Large"}
                                                                                                    {"id" "claude-test-small" "display_name" "Claude Test Small"}]
                                                                                           "has_more" false})
                     (= url "/anthropic/v1/messages") (send-events! response (anthropic-events (get body "messages")
                                                                                               (map? (get body "cache_control"))))
                     :else (send-json! response 404 {"type" "error" "error" {"message" "no such route"}}))

                   (string/starts-with? url "/openai/")
                   (cond
                     (not= test-key bearer) (send-json! response 401 {"error" {"message" "Incorrect API key provided"}})
                     (= url "/openai/models") (send-json! response 200 {"object" "list" "data" [{"id" "test-chat"} {"id" "text-embedding-test"} {"id" "test-mini"}]})
                     (= url "/openai/chat/completions") (send-events! response (openai-events (get body "messages")))
                     :else (send-json! response 404 {"error" {"message" "no such route"}}))

                   :else (send-json! response 404 {"error" "no such route"}))))))

(defn start!
  "Resolves to {:port :requests :server} for both stand-ins on one port,
  the Anthropic one under /anthropic and the OpenAI-style one under
  /openai, with `requests` an atom of every request received."
  []
  (js/Promise.
   (fn [resolve _reject]
     (let [requests (atom [])
           ^js server (.createServer http #(handle! requests %1 %2))]
       (.listen server 0 "127.0.0.1" (fn [] (resolve {:port (.-port (.address server)) :requests requests :server server})))))))

(defn stop! [{:keys [^js server]}]
  (js/Promise. (fn [resolve _reject] (.close server resolve))))

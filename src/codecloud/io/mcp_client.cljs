(ns codecloud.io.mcp-client
  "A client of an MCP server over Streamable HTTP, as CodeCloud's
  assistant reaches the agent hub. It opens a session, lists the tools,
  calls them, and ends the session. Messages are maps with string keys,
  as JSON parses them."
  (:require [clojure.string :as string]
            [codecloud.agents.mcp :as mcp]
            [codecloud.assistant.providers :as providers]
            [codecloud.io.task :as task]))

(def ^:private protocol-version
  "The protocol revision the client asks for."
  (first mcp/supported-versions))

(defn- headers-for [{:keys [token session]}]
  (clj->js (cond-> {"content-type" "application/json"
                    "accept" "application/json, text/event-stream"
                    "authorization" (str "Bearer " token)}
             session (assoc "mcp-session-id" session "mcp-protocol-version" protocol-version))))

(defn- answer-of
  "The JSON-RPC message in an answer's text, which is the body itself or
  the first event of an event stream."
  [^js response text]
  (let [streamed? (string/includes? (or (.get (.-headers response) "content-type") "") "event-stream")
        payload (if streamed? (some-> (first (first (providers/sse-events (str text "\n\n")))) :data) text)]
    (when (seq payload)
      (js->clj (js/JSON.parse payload)))))

(defn- post!
  "Resolves to [response answer] for `message` posted in the client's
  session, rejecting when the server refuses it."
  [{:keys [url] :as client} message]
  (-> (js/fetch url #js {:method "POST" :headers (headers-for client) :body (js/JSON.stringify (clj->js message))})
      (task/then (fn [^js response]
                   (task/then (.text response)
                              (fn [text]
                                (if (.-ok response)
                                  [response (answer-of response text)]
                                  (task/rejected (js/Error. (str "The hub answered " (.-status response)
                                                                 (when (seq text) (str ": " text))))))))))))

(defn- result-of [answer]
  (if-let [problem (get answer "error")]
    (task/rejected (js/Error. (str "The hub refused the request: " (get problem "message"))))
    (task/resolved (get answer "result"))))

(defn connect!
  "Resolves to a client in a new session with the server at `url`,
  holding `token`, introduced as `client-info`, {\"name\" \"title\"
  \"version\"}."
  [{:keys [url token client-info]}]
  (let [opening {:url url :token token :session nil :next-id (atom 1)}]
    (-> (post! opening (mcp/request 0 "initialize" {"protocolVersion" protocol-version
                                                    "capabilities" {}
                                                    "clientInfo" client-info}))
        (task/then (fn [[^js response answer]]
                     (task/then (result-of answer)
                                (fn [_]
                                  (let [client (assoc opening :session (.get (.-headers response) "mcp-session-id"))]
                                    (task/then (post! client (mcp/notification "notifications/initialized" nil))
                                               (fn [_] client))))))))))

(defn- ask! [{:keys [next-id] :as client} method params]
  (let [id (swap! next-id inc)]
    (task/then (post! client (mcp/request id method params))
               (fn [[_response answer]] (result-of answer)))))

(defn list-tools!
  "Resolves to the tools the server lists, each a map as MCP gives it."
  [client]
  (task/then (ask! client "tools/list" {}) #(vec (get % "tools"))))

(defn call-tool!
  "Resolves to the result of calling tool `tool-name` with `arguments`,
  a map as MCP gives it, \"isError\" true for a failed call."
  [client tool-name arguments]
  (ask! client "tools/call" {"name" tool-name "arguments" (or arguments {})}))

(defn close!
  "Ends the client's session, resolving whatever the server says."
  [{:keys [url session] :as client}]
  (if session
    (-> (js/fetch url #js {:method "DELETE" :headers (headers-for client)})
        (task/recover (fn [_error] nil)))
    (task/resolved nil)))

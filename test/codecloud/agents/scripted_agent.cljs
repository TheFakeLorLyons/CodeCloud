(ns codecloud.agents.scripted-agent
  "A scripted MCP client for tests, speaking Streamable HTTP to the hub
  the way an agent does."
  (:require ["http" :as http]
            [clojure.string :as string]
            [codecloud.agents.mcp :as mcp]
            [goog.object :as gobj]))

(defn exchange
  "Resolves to {:status :headers :body} for one request to the server on
  `port`, the body parsed when it is JSON."
  [port {:keys [method path headers body] :or {method "POST" path "/mcp"}}]
  (js/Promise.
   (fn [resolve reject]
     (let [^js request (.request http #js {:host "127.0.0.1" :port port :method method :path path
                                           :headers (clj->js headers)}
                                 (fn [^js response]
                                   (let [chunks #js []]
                                     (.on response "data" #(.push chunks %))
                                     (.on response "end"
                                          (fn []
                                            (let [text (.toString (js/Buffer.concat chunks) "utf8")
                                                  json? (string/includes? (or (gobj/get (.-headers response) "content-type") "") "json")]
                                              (resolve {:status (.-statusCode response)
                                                        :headers (js->clj (.-headers response))
                                                        :body (if (and json? (seq text)) (js->clj (js/JSON.parse text)) text)})))))))]
       (.on request "error" reject)
       (when body (.write request body))
       (.end request)))))

(defn headers-for
  "The headers an agent holding `token` sends, naming `session-id` once
  it has one."
  [token session-id]
  (cond-> {"Content-Type" "application/json"
           "Accept" "application/json, text/event-stream"
           "Authorization" (str "Bearer " token)}
    session-id (assoc "Mcp-Session-Id" session-id)))

(defn post-message
  "Resolves to the answer to `message`, a map or a batch, sent in
  `session-id`."
  [port token session-id message]
  (exchange port {:headers (headers-for token session-id) :body (js/JSON.stringify (clj->js message))}))

(defn connect!
  "Resolves to an agent {:port :token :session} once the hub on `port`
  accepted `initialize` from a client calling itself `client-info` and
  heard that it is initialized."
  [port token client-info]
  (-> (post-message port token nil (mcp/request 1 "initialize" {"protocolVersion" "2025-06-18"
                                                                "capabilities" {}
                                                                "clientInfo" client-info}))
      (.then (fn [{:keys [headers]}]
               (let [agent {:port port :token token :session (get headers "mcp-session-id")}]
                 (.then (post-message port token (:session agent) (mcp/notification "notifications/initialized" nil))
                        (fn [_] agent)))))))

(defn ask!
  "Resolves to the JSON-RPC response to `method` with `params`."
  [{:keys [port token session]} method params]
  (.then (post-message port token session (mcp/request (str (random-uuid)) method params))
         (fn [answer] (:body answer))))

(defn call-tool!
  "Resolves to the result of calling tool `tool-name` with `arguments`."
  [agent tool-name arguments]
  (.then (ask! agent "tools/call" {"name" tool-name "arguments" arguments})
         #(get % "result")))

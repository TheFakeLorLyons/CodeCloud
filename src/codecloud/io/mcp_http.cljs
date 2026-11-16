(ns codecloud.io.mcp-http
  "An MCP server over Streamable HTTP on Node's http module, with one
  endpoint, a bearer token on every request, sessions named by the
  Mcp-Session-Id header, and event streams held open while an agent
  listens. codecloud.agents.mcp decides what each message means; this
  namespace checks who is asking and moves the bytes."
  (:require ["crypto" :as crypto]
            ["http" :as http]
            [clojure.string :as string]
            [codecloud.agents.mcp :as mcp]
            [codecloud.agents.roster :as roster]
            [codecloud.io.task :as task]
            [goog.object :as gobj]))

(defn- now [] (js/Date.now))

(defn- header
  "The request's header `header-name`, which Node keeps in lower case."
  [^js request header-name]
  (gobj/get (.-headers request) header-name))

;; Answers

(defn- send-json! [^js response status body headers]
  (.writeHead response status (clj->js (merge {"Content-Type" "application/json"} headers)))
  (.end response (js/JSON.stringify (clj->js body))))

(defn- refuse!
  "Answers with HTTP `status` and a JSON-RPC error saying why, as MCP
  clients expect of a refusal."
  ([response status text] (refuse! response status text {}))
  ([response status text headers]
   (send-json! response status (mcp/error-response nil :refused text) headers)))

;; Who may ask

(def ^:private loopback-hosts #{"localhost" "127.0.0.1" "::1" "[::1]"})

(defn- loopback-host? [host-name]
  (boolean (and host-name (or (contains? loopback-hosts host-name)
                              (string/starts-with? host-name "127.")))))

(defn loopback-address?
  "True when listening at `address` reaches only this computer."
  [address]
  (loopback-host? address))

(defn- host-name [host-header]
  (when host-header
    (if (string/starts-with? host-header "[")
      (subs host-header 0 (inc (or (string/index-of host-header "]") (dec (count host-header)))))
      (first (string/split host-header #":")))))

(defn- trusted-origin?
  "True for requests no web page sent. Browsers add an Origin header to
  every request a page makes, so this keeps websites from reaching the
  hub through the person's browser."
  [origin]
  (or (nil? origin)
      (string/starts-with? origin "vscode-file://")
      (string/starts-with? origin "vscode-webview://")
      (let [[_ origin-host] (re-matches #"https?://(\[[^\]]+\]|[^:/]+)(?::\d+)?/?" origin)]
        (loopback-host? origin-host))))

(defn- authorized?
  "True when the request carries `token` as its bearer token, compared in
  constant time so the comparison's duration reveals nothing."
  [request token]
  (let [[_ offered] (re-matches #"(?i)Bearer\s+(\S+)\s*" (or (header request "authorization") ""))]
    (boolean
     (when (and offered token)
       (let [expected-bytes (js/Buffer.from token "utf8")
             offered-bytes (js/Buffer.from offered "utf8")]
         (and (= (.-length expected-bytes) (.-length offered-bytes))
              (.timingSafeEqual crypto expected-bytes offered-bytes)))))))

;; Sessions

(defn- changed! [{:keys [state on-change]}]
  (when on-change
    (on-change (:roster @state))))

(defn- known-session? [{:keys [state]} session-id]
  (contains? (:roster @state) session-id))

(defn- end-streams! [{:keys [state]} session-id]
  (doseq [^js stream (get-in @state [:streams session-id])]
    (.end stream))
  (swap! state update :streams dissoc session-id))

(defn notify-all!
  "Sends the notification `method` with `params` on every open event
  stream, such as notifications/tools/list_changed after the tools change."
  [{:keys [state]} method params]
  (let [event (str "event: message\ndata: " (js/JSON.stringify (clj->js (mcp/notification method params))) "\n\n")]
    (doseq [[_session-id streams] (:streams @state)
            ^js stream streams]
      (.write stream event))))

;; Requests

(defn- read-body
  "Resolves to the request's body as text, or rejects with the code
  BODY_TOO_LARGE once it passes `limit` bytes."
  [^js request limit]
  (js/Promise.
   (fn [resolve reject]
     (let [chunks #js []
           received (atom 0)]
       (.on request "data" (fn [^js chunk]
                             (when (<= @received limit)
                               (swap! received + (.-length chunk))
                               (if (> @received limit)
                                 (reject (js/Object.assign (js/Error. "Body too large") #js {:code "BODY_TOO_LARGE"}))
                                 (.push chunks chunk)))))
       (.on request "end" #(resolve (.toString (js/Buffer.concat chunks) "utf8")))
       (.on request "error" reject)))))

(defn- parse-json [text]
  (try
    (js->clj (js/JSON.parse text))
    (catch :default _error ::unreadable)))

(defn- answer-call
  "Promise of the response to a tool call, its failure reported as a
  failed tool result the agent can read."
  [{:keys [run-tool]} session-id {:keys [id] :as call}]
  (-> (task/resolved nil)
      (task/then (fn [_] (run-tool session-id call)))
      (task/then (fn [result] (mcp/result-response id result)))
      (task/recover (fn [error] (mcp/result-response id (mcp/failed-result (task/error-text error)))))))

(defn- apply-outcome!
  "Records what one routed message did to its session and returns a
  promise of its response, or nil when it needs none."
  [{:keys [state] :as server} session-id {:keys [reply call opened]}]
  (let [moment (now)]
    (cond
      opened (swap! state update :roster roster/opened session-id (:client opened) (:version opened) moment)
      call (swap! state update :roster roster/called session-id moment)
      :else (swap! state update :roster roster/seen session-id moment))
    (cond
      call (answer-call server session-id call)
      reply (task/resolved reply)
      :else nil)))

(defn- answer-messages!
  "Routes the messages of one POST and answers them with 202 when none was
  a request, and otherwise with their responses, in an array when they
  came as one."
  [{:keys [describe] :as server} ^js response messages batch? session-id opening?]
  (let [outcomes (mapv #(mcp/route (describe) %) messages)
        pending (doall (keep #(apply-outcome! server session-id %) outcomes))]
    (when opening? (changed! server))
    (if (empty? pending)
      (do (.writeHead response 202) (.end response))
      (task/then (task/all pending)
                 (fn [responses]
                   (send-json! response 200 (if batch? responses (first responses))
                               (if opening? {"Mcp-Session-Id" session-id} {})))))))

(defn- version-refusal [request]
  (let [version (header request "mcp-protocol-version")]
    (when (and version (not (mcp/supported-version? version)))
      (str "The hub does not speak protocol revision " version))))

(defn- handle-post!
  "Answers a POST carrying one message or a batch of them. `initialize`
  opens a session, and every other message must name one."
  [server ^js request ^js response session-id]
  (let [body-limit (* 4 1024 1024)] ; four megabytes, far beyond any tool call's arguments
    (if-not (string/includes? (or (header request "content-type") "") "application/json")
      (refuse! response 415 "Requests must carry JSON")
      (-> (read-body request body-limit)
          (task/then
           (fn [text]
             (let [parsed (parse-json text)
                   batch? (vector? parsed)
                   messages (if batch? parsed [parsed])
                   opening? (some #(and (map? %) (= "initialize" (get % "method"))) messages)
                   version-problem (version-refusal request)]
               (cond
                 (= ::unreadable parsed) (send-json! response 400 (mcp/error-response nil :parse-error "The request body is not JSON") {})
                 (empty? messages) (send-json! response 400 (mcp/error-response nil :invalid-request "An empty batch asks for nothing") {})
                 (and opening? (> (count messages) 1)) (refuse! response 400 "initialize must arrive alone")
                 opening? (answer-messages! server response messages batch? (.randomUUID crypto) true)
                 (nil? session-id) (refuse! response 400 "Requests after initialize must carry the Mcp-Session-Id header")
                 (not (known-session? server session-id)) (refuse! response 404 "No session has that id; initialize again")
                 version-problem (refuse! response 400 version-problem)
                 :else (answer-messages! server response messages batch? session-id false)))))
          (task/recover
           (fn [error]
             (when-not (.-headersSent response)
               (if (= "BODY_TOO_LARGE" (.-code error))
                 (refuse! response 413 "The request body is too large" {"Connection" "close"})
                 (refuse! response 500 (task/error-text error))))))))))

(defn- open-stream!
  "Holds a GET open as an event stream for server notifications. An open
  stream also tells the hub its agent is still there; a comment every
  25 seconds keeps idle connections from being cut."
  [{:keys [state] :as server} ^js request ^js response session-id]
  (let [keep-alive-milliseconds 25000]
    (if-not (string/includes? (or (header request "accept") "") "text/event-stream")
      (refuse! response 406 "An event stream must accept text/event-stream")
      (let [keep-alive (js/setInterval #(.write response ":\n\n") keep-alive-milliseconds)]
        (.writeHead response 200 #js {"Content-Type" "text/event-stream"
                                      "Cache-Control" "no-cache, no-transform"
                                      "Connection" "keep-alive"
                                      "Mcp-Session-Id" session-id})
        (.write response ": CodeCloud\n\n")
        (swap! state (fn [current]
                       (-> current
                           (update-in [:streams session-id] (fnil conj #{}) response)
                           (update :roster roster/listening session-id (now)))))
        (changed! server)
        (.on response "close"
             (fn []
               (js/clearInterval keep-alive)
               (swap! state (fn [current]
                              (-> current
                                  (update-in [:streams session-id] disj response)
                                  (update :roster roster/stopped-listening session-id (now)))))
               (changed! server)))))))

(defn- end-session! [{:keys [state] :as server} ^js response session-id]
  (end-streams! server session-id)
  (swap! state update :roster roster/closed session-id)
  (changed! server)
  (.writeHead response 200)
  (.end response))

(defn- handle-request!
  "Checks who is asking, then passes the request on by its method."
  [{:keys [state endpoint loopback?] :as server} ^js request ^js response]
  (let [url-path (first (string/split (or (.-url request) "") #"\?"))
        session-id (header request "mcp-session-id")
        method (.-method request)
        version-problem (version-refusal request)]
    (cond
      (not= endpoint url-path) (refuse! response 404 (str "CodeCloud's hub answers only at " endpoint))
      (and loopback? (not (loopback-host? (host-name (header request "host"))))) (refuse! response 403 "The hub answers only requests addressed to this computer")
      (not (trusted-origin? (header request "origin"))) (refuse! response 403 "The hub refuses requests from web pages")
      (not (authorized? request (:token @state))) (refuse! response 401 "The hub needs the token that CodeCloud: Connect an Agent gives"
                                                           {"WWW-Authenticate" "Bearer realm=\"CodeCloud\""})
      (= "POST" method) (handle-post! server request response session-id)
      (not (contains? #{"GET" "DELETE"} method)) (refuse! response 405 "The hub answers GET, POST, and DELETE" {"Allow" "GET, POST, DELETE"})
      (nil? session-id) (refuse! response 400 "This request must carry the Mcp-Session-Id header")
      (not (known-session? server session-id)) (refuse! response 404 "No session has that id; initialize again")
      version-problem (refuse! response 400 version-problem)
      (= "GET" method) (open-stream! server request response session-id)
      :else (end-session! server response session-id))))

;; Running

(defn start!
  "Serves MCP at `endpoint`, resolving once it listens or rejecting with
  Node's error. `on-change` hears the roster as it changes and once a
  minute."
  [{:keys [address port token endpoint describe run-tool on-change]}]
  (js/Promise.
   (fn [resolve reject]
     (let [state (atom {:token token :roster {} :streams {}})
           server {:state state
                   :endpoint endpoint
                   :loopback? (loopback-address? address)
                   :describe describe
                   :run-tool run-tool
                   :on-change on-change}
           ^js http-server (.createServer http
                                          (fn [request ^js response]
                                            (try
                                              (handle-request! server request response)
                                              (catch :default error
                                                (when-not (.-headersSent response)
                                                  (refuse! response 500 (task/error-text error)))))))
           roster-review-milliseconds 60000]
       (.once http-server "error" reject)
       (.listen http-server port address
                (fn []
                  (let [review (js/setInterval (fn []
                                                 (swap! state update :roster roster/forgotten (now))
                                                 (changed! server))
                                               roster-review-milliseconds)]
                    (resolve (assoc server
                                    :http http-server
                                    :review review
                                    :address address
                                    :port (.-port (.address http-server)))))))))))

(defn roster-of
  "The running server's roster of agent sessions."
  [{:keys [state]}]
  (:roster @state))

(defn set-token!
  "Makes `token` the only one the running server accepts from now on."
  [{:keys [state]} token]
  (swap! state assoc :token token))

(defn stop!
  "Ends every event stream and closes the server, resolving once it is
  closed."
  [{:keys [state ^js http review] :as server}]
  (js/clearInterval review)
  (doseq [session-id (keys (:streams @state))]
    (end-streams! server session-id))
  (js/Promise.
   (fn [resolve _reject]
     (.close http (fn [_error] (resolve nil)))
     (when (.-closeAllConnections http)
       (.closeAllConnections http)))))

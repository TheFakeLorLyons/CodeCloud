(ns codecloud.agents.mcp
  "The Model Context Protocol as CodeCloud's hub speaks it, over JSON-RPC
  2.0. Messages are maps with string keys, as JSON parses them, so tool
  arguments and results pass through unchanged, `_meta` keys with
  slashes included. `route` decides what the hub does with each message
  an agent sends; the transports carry the bytes.")

(def supported-versions
  "Protocol revisions the hub speaks, newest first. A client asking for
  any other revision is offered the newest and decides for itself
  whether to go on."
  ["2025-11-25" "2025-06-18" "2025-03-26" "2024-11-05"])

(def ^:private error-codes
  "JSON-RPC's codes for the errors the hub reports; -32000 is the first of
  the codes JSON-RPC leaves to servers, used for refusals by the
  transport."
  {:parse-error -32700
   :invalid-request -32600
   :method-not-found -32601
   :invalid-params -32602
   :internal-error -32603
   :refused -32000})

(defn supported-version? [version]
  (boolean (some #{version} supported-versions)))

(defn negotiated-version
  "The revision that answers an `initialize` asking for `requested`."
  [requested]
  (if (supported-version? requested) requested (first supported-versions)))

;; Messages

(defn- valid-id? [id]
  (or (string? id) (number? id)))

(defn message-kind
  "The kind of a JSON-RPC message, one of :request, :notification,
  :response, and :invalid."
  [message]
  (let [has-id? (and (map? message) (contains? message "id"))]
    (cond
      (not (and (map? message) (= "2.0" (get message "jsonrpc")))) :invalid
      (string? (get message "method")) (cond
                                         (not has-id?) :notification
                                         (valid-id? (get message "id")) :request
                                         :else :invalid)
      (and has-id? (or (contains? message "result") (contains? message "error"))) :response
      :else :invalid)))

(defn request [id method params]
  (cond-> {"jsonrpc" "2.0" "id" id "method" method}
    (some? params) (assoc "params" params)))

(defn notification [method params]
  (cond-> {"jsonrpc" "2.0" "method" method}
    (some? params) (assoc "params" params)))

(defn result-response [id result]
  {"jsonrpc" "2.0" "id" id "result" result})

(defn error-response
  "An error answering request `id`, nil when the request had none,
  `error-key` naming one of JSON-RPC's codes."
  [id error-key text]
  {"jsonrpc" "2.0" "id" id "error" {"code" (get error-codes error-key) "message" text}})

;; Tool results

(defn text-result
  "A tool's answer holding `text`."
  [text]
  {"content" [{"type" "text" "text" text}]})

(defn failed-result
  "A tool's answer saying it could not do what was asked, which an agent
  reads and can act on, as it cannot a protocol error."
  [text]
  {"content" [{"type" "text" "text" text}] "isError" true})

;; Routing

(defn initialize-result
  "The answer to `initialize`, naming the revision both sides speak from
  then on, the hub's capabilities, who it is, and how agents should use
  it."
  [requested-version server-info instructions]
  (cond-> {"protocolVersion" (negotiated-version requested-version)
           "capabilities" {"tools" {"listChanged" true}}
           "serverInfo" server-info}
    instructions (assoc "instructions" instructions)))

(defn- listed? [tools tool-name]
  (boolean (some #(= tool-name (get % "name")) tools)))

(defn- route-request [{:keys [info instructions tools]} {id "id" method "method" params "params"}]
  (cond
    (and (some? params) (not (map? params)))
    {:reply (error-response id :invalid-params "Parameters must be an object")}

    (= "initialize" method)
    {:reply (result-response id (initialize-result (get params "protocolVersion") info instructions))
     :opened {:client (get params "clientInfo")
              :version (negotiated-version (get params "protocolVersion"))}}

    (= "ping" method)
    {:reply (result-response id {})}

    (= "tools/list" method)
    {:reply (result-response id {"tools" tools})}

    (= "tools/call" method)
    (let [tool-name (get params "name")
          arguments (or (get params "arguments") {})]
      (cond
        (not (listed? tools tool-name)) {:reply (error-response id :invalid-params (str "No tool is called " tool-name))}
        (not (map? arguments)) {:reply (error-response id :invalid-params "Tool arguments must be an object")}
        :else {:call {:id id :name tool-name :arguments arguments}}))

    :else
    {:reply (error-response id :method-not-found (str "The hub does not handle " method))}))

(defn route
  "What the hub does with one message from an agent, a :reply to send, a
  tool :call to run, an :opened session for initialize, the :notified
  method of a notification, or nothing for answers to requests it never
  sent."
  [server message]
  (case (message-kind message)
    :request (route-request server message)
    :notification {:notified (get message "method")}
    :response {}
    :invalid {:reply (error-response (let [id (when (map? message) (get message "id"))]
                                       (when (valid-id? id) id))
                                     :invalid-request
                                     "Not a JSON-RPC 2.0 message")}))

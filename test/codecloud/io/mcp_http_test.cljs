(ns codecloud.io.mcp-http-test
  "Checks the hub's HTTP transport with real requests, for whom it
  refuses, how sessions open and end, event streams, and tool calls."
  (:require ["http" :as http]
            [cljs.test :refer [async deftest is]]
            [clojure.string :as string]
            [codecloud.agents.mcp :as mcp]
            [codecloud.agents.roster :as roster]
            [codecloud.agents.scripted-agent :as agent]
            [codecloud.io.mcp-http :as mcp-http]))

(def ^:private token "a-token-for-tests")

(def ^:private described
  {:info {"name" "codecloud" "version" "test"}
   :instructions "Outline before reading."
   :tools [{"name" "echo" "inputSchema" {"type" "object"}}
           {"name" "break" "inputSchema" {"type" "object"}}]})

(defn- run-tool [session-id {tool-name :name :keys [arguments]}]
  (if (= "break" tool-name)
    (js/Promise.reject (js/Error. "broke on purpose"))
    (js/Promise.resolve (mcp/text-result (str session-id " said " (get arguments "words"))))))

(defn- start-server! [changes]
  (mcp-http/start! {:address "127.0.0.1"
                    :port 0
                    :token token
                    :endpoint "/mcp"
                    :describe (constantly described)
                    :run-tool run-tool
                    :on-change #(swap! changes conj %)}))

(defn- exchange [port options]
  (agent/exchange port options))

(defn- headers-for
  ([] (agent/headers-for token nil))
  ([session-id] (agent/headers-for token session-id)))

(defn- post-message [port session-id message]
  (agent/post-message port token session-id message))

(def ^:private initialize-message
  (mcp/request 1 "initialize" {"protocolVersion" "2025-06-18"
                               "capabilities" {}
                               "clientInfo" {"name" "claude-code" "version" "2.1.0"}}))

(deftest strangers-are-refused
  (async done
    (let [server-holder (atom nil)
          initialize-body (js/JSON.stringify (clj->js initialize-message))]
      (-> (start-server! (atom []))
          (.then (fn [{:keys [port] :as server}]
                   (reset! server-holder server)
                   (js/Promise.all
                    #js [(exchange port {:path "/other" :headers (headers-for) :body initialize-body})
                         (exchange port {:headers (dissoc (headers-for) "Authorization") :body initialize-body})
                         (exchange port {:headers (assoc (headers-for) "Authorization" "Bearer wrong") :body initialize-body})
                         (exchange port {:headers (assoc (headers-for) "Origin" "https://evil.example") :body initialize-body})
                         (exchange port {:headers (assoc (headers-for) "Host" "evil.example:80") :body initialize-body})
                         (exchange port {:headers (assoc (headers-for) "Content-Type" "text/plain") :body initialize-body})
                         (exchange port {:headers (headers-for) :body "{not json"})
                         (exchange port {:headers (headers-for) :body "[]"})
                         (exchange port {:headers (assoc (headers-for) "Origin" "http://localhost:6274") :body initialize-body})
                         (exchange port {:method "PUT" :headers (headers-for) :body initialize-body})])))
          (.then (fn [answers]
                   (let [[wrong-path no-token wrong-token web-page wrong-host plain-text unreadable empty-batch
                          local-page put] (vec answers)]
                     (is (= 404 (:status wrong-path)))
                     (is (= 401 (:status no-token)))
                     (is (= "Bearer realm=\"CodeCloud\"" (get-in no-token [:headers "www-authenticate"])))
                     (is (= -32000 (get-in no-token [:body "error" "code"])) "a refusal is a JSON-RPC error")
                     (is (= 401 (:status wrong-token)))
                     (is (= 403 (:status web-page)) "a web page's request is refused")
                     (is (= 403 (:status wrong-host)) "a request addressed to another host is refused")
                     (is (= 415 (:status plain-text)))
                     (is (= 400 (:status unreadable)))
                     (is (= -32700 (get-in unreadable [:body "error" "code"])))
                     (is (= 400 (:status empty-batch)))
                     (is (= 200 (:status local-page)) "a page served on this computer may connect")
                     (is (= 405 (:status put))))))
          (.catch (fn [error] (is (nil? error) (str "requests failed: " (.-message error)))))
          (.finally (fn []
                      (some-> @server-holder mcp-http/stop!)
                      (done)))))))

(deftest a-session-from-initialize-to-delete
  (async done
    (let [server-holder (atom nil)
          session (atom nil)
          changes (atom [])
          port (fn [] (:port @server-holder))]
      (-> (start-server! changes)
          (.then (fn [server]
                   (reset! server-holder server)
                   (post-message (port) nil initialize-message)))
          (.then (fn [{:keys [status headers body]}]
                   (is (= 200 status))
                   (is (= "2025-06-18" (get-in body ["result" "protocolVersion"])))
                   (is (= "Outline before reading." (get-in body ["result" "instructions"])))
                   (reset! session (get headers "mcp-session-id"))
                   (is (string? @session) "initialize names the session")
                   (is (= ["Claude Code"] (map :name (roster/present-agents (last @changes) (js/Date.now))))
                       "the hub hears of the agent")
                   (js/Promise.all
                    #js [(post-message (port) nil (mcp/request 2 "tools/list" {}))
                         (post-message (port) "no-such-session" (mcp/request 3 "tools/list" {}))
                         (post-message (port) @session (mcp/notification "notifications/initialized" nil))
                         (post-message (port) @session (mcp/request 4 "tools/list" {}))
                         (post-message (port) @session (mcp/request 5 "tools/call" {"name" "echo" "arguments" {"words" "hello"}}))
                         (post-message (port) @session (mcp/request 6 "tools/call" {"name" "break"}))
                         (post-message (port) @session [(mcp/request 7 "ping" nil) (mcp/notification "notifications/progress" {})
                                                        (mcp/request 8 "ping" nil)])
                         (exchange (port) {:headers (assoc (headers-for @session) "MCP-Protocol-Version" "1999-01-01")
                                           :body (js/JSON.stringify (clj->js (mcp/request 9 "ping" nil)))})
                         (exchange (port) {:headers (assoc (headers-for @session) "MCP-Protocol-Version" "2025-06-18")
                                           :body (js/JSON.stringify (clj->js (mcp/request 10 "ping" nil)))})])))
          (.then (fn [answers]
                   (let [[no-session unknown-session initialized listed called broken batch old-version agreed-version]
                         (vec answers)]
                     (is (= 400 (:status no-session)) "requests after initialize name their session")
                     (is (= 404 (:status unknown-session)) "an unknown session asks the agent to initialize again")
                     (is (= 202 (:status initialized)) "a notification is accepted without an answer")
                     (is (= ["echo" "break"] (map #(get % "name") (get-in listed [:body "result" "tools"]))))
                     (is (= {"content" [{"type" "text" "text" (str @session " said hello")}]}
                            (get-in called [:body "result"]))
                         "a tool call reaches the tool with its session")
                     (is (= {"content" [{"type" "text" "text" "broke on purpose"}] "isError" true}
                            (get-in broken [:body "result"]))
                         "a tool's failure is a result the agent can read")
                     (is (= [7 8] (map #(get % "id") (:body batch))) "a batch is answered in an array")
                     (is (= 400 (:status old-version)))
                     (is (= 200 (:status agreed-version)))
                     (is (= 2 (:calls (get (mcp-http/roster-of @server-holder) @session)))))
                   (exchange (port) {:method "DELETE" :headers (headers-for @session)})))
          (.then (fn [{:keys [status]}]
                   (is (= 200 status))
                   (is (empty? (mcp-http/roster-of @server-holder)) "the agent left")
                   (post-message (port) @session (mcp/request 20 "ping" nil))))
          (.then (fn [{:keys [status]}]
                   (is (= 404 status) "an ended session is unknown")))
          (.catch (fn [error] (is (nil? error) (str "the session failed: " (.-message error)))))
          (.finally (fn []
                      (some-> @server-holder mcp-http/stop!)
                      (done)))))))

(defn- open-stream
  "Resolves to {:status :request :events} for a GET event stream, once its
  first bytes arrive; :events collects what arrives later."
  [port headers]
  (js/Promise.
   (fn [resolve reject]
     (let [events (atom "")
           ^js request (.request http #js {:host "127.0.0.1" :port port :method "GET" :path "/mcp"
                                           :headers (clj->js headers)})]
       (.on request "response"
            (fn [^js response]
              (.setEncoding response "utf8")
              (.once response "data" (fn [_first] (resolve {:status (.-statusCode response) :request request :events events})))
              (.on response "data" #(swap! events str %))
              (.on response "error" (fn [_error] nil))))
       (.on request "error" (fn [error] (when-not (= "ECONNRESET" (.-code error)) (reject error))))
       (.end request)))))

(defn- once
  "Resolves once `ready?` returns true, checking every few milliseconds."
  [ready?]
  (js/Promise.
   (fn [resolve _reject]
     (letfn [(check [] (if (ready?) (resolve true) (js/setTimeout check 5)))]
       (check)))))

(deftest an-event-stream-keeps-an-agent-present
  (async done
    (let [server-holder (atom nil)
          session (atom nil)
          stream (atom nil)
          port (fn [] (:port @server-holder))
          streams-open (fn [] (:streams (get (mcp-http/roster-of @server-holder) @session)))]
      (-> (start-server! (atom []))
          (.then (fn [server]
                   (reset! server-holder server)
                   (post-message (port) nil initialize-message)))
          (.then (fn [{:keys [headers]}]
                   (reset! session (get headers "mcp-session-id"))
                   (exchange (port) {:method "GET" :headers (assoc (headers-for @session) "Accept" "application/json")})))
          (.then (fn [{:keys [status]}]
                   (is (= 406 status) "a stream must accept events")
                   (open-stream (port) (headers-for @session))))
          (.then (fn [opened]
                   (reset! stream opened)
                   (is (= 200 (:status opened)))
                   (once #(= 1 (streams-open)))))
          (.then (fn [_]
                   (mcp-http/notify-all! @server-holder "notifications/tools/list_changed" nil)
                   (once #(string/includes? @(:events @stream) "list_changed"))))
          (.then (fn [_]
                   (is (string/includes? @(:events @stream) "event: message\ndata: {\"jsonrpc\":\"2.0\",\"method\":\"notifications/tools/list_changed\"}"))
                   (.destroy ^js (:request @stream))
                   (once #(zero? (streams-open)))))
          (.then (fn [_]
                   (is (zero? (streams-open)) "a closed stream stops counting")
                   (mcp-http/set-token! @server-holder "a-new-token")
                   (post-message (port) @session (mcp/request 2 "ping" nil))))
          (.then (fn [{:keys [status]}]
                   (is (= 401 status) "the old token stops working once replaced")))
          (.catch (fn [error] (is (nil? error) (str "the stream failed: " (.-message error)))))
          (.finally (fn []
                      (some-> @server-holder mcp-http/stop!)
                      (done)))))))

(deftest an-oversized-body-is-refused
  (async done
    (let [server-holder (atom nil)
          five-megabytes (* 5 1024 1024)]
      (-> (start-server! (atom []))
          (.then (fn [{:keys [port] :as server}]
                   (reset! server-holder server)
                   (exchange port {:headers (headers-for) :body (.repeat "x" five-megabytes)})))
          (.then (fn [{:keys [status]}] (is (= 413 status)))
                 (fn [error] (is (= "ECONNRESET" (.-code error)) "or the connection closes while sending")))
          (.finally (fn []
                      (some-> @server-holder mcp-http/stop!)
                      (done)))))))

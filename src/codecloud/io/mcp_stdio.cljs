(ns codecloud.io.mcp-stdio
  "An MCP client for a server run as a child process, speaking
  newline-delimited JSON-RPC over the child's standard input and output,
  as wavescope-mcp does."
  (:require ["child_process" :as child-process]
            [clojure.string :as string]
            [codecloud.agents.mcp :as mcp]
            [codecloud.io.task :as task]))

(defn- send! [{:keys [^js child]} message]
  (let [^js input (.-stdin child)]
    (when (.-writable input)
      (.write input (str (js/JSON.stringify (clj->js message)) "\n")))))

(defn request!
  "Sends `method` with `params` and resolves to the server's result, or
  rejects with the server's error, when no answer comes within
  `milliseconds`, or when the server exits first."
  [{:keys [state] :as client} method params milliseconds]
  (js/Promise.
   (fn [resolve reject]
     (if (:exited? @state)
       (reject (task/coded-error (str "The server is not running, since " (:exit-reason @state)) "MCP_EXITED"))
       (let [id (:sent (swap! state update :sent inc))
             give-up! (fn []
                        (when (get-in @state [:awaiting id])
                          (swap! state update :awaiting dissoc id)
                          (send! client (mcp/notification "notifications/cancelled" {"requestId" id "reason" "No answer in time"}))
                          (reject (task/coded-error (str method " had no answer within " (/ milliseconds 1000) " seconds")
                                                    "MCP_SILENT"))))]
         (swap! state assoc-in [:awaiting id] {:resolve resolve
                                               :reject reject
                                               :timer (js/setTimeout give-up! milliseconds)})
         (send! client (mcp/request id method params)))))))

(defn- settle!
  "Hands a response to the request awaiting it."
  [{:keys [state]} {id "id" result "result" error "error"}]
  (when-let [{:keys [resolve reject timer]} (get-in @state [:awaiting id])]
    (js/clearTimeout timer)
    (swap! state update :awaiting dissoc id)
    (if error
      (reject (task/coded-error (str (get error "message")) (str "MCP_" (get error "code"))))
      (resolve result))))

(defn- answer-server-request!
  "Answers what the server asks of the client, a ping or the roots, of
  which the client offers none. Anything else is a method the client
  lacks."
  [client {id "id" method "method"}]
  (send! client (case method
                  "ping" (mcp/result-response id {})
                  "roots/list" (mcp/result-response id {"roots" []})
                  (mcp/error-response id :method-not-found (str "The client does not handle " method)))))

(defn- take-line! [client line]
  (let [message (try (js->clj (js/JSON.parse line)) (catch :default _error nil))]
    (case (mcp/message-kind message)
      :response (settle! client message)
      :request (answer-server-request! client message)
      nil)))

(defn- take-output!
  "Handles every whole line of output so far, keeping a line still
  arriving for the next chunk."
  [{:keys [state] :as client} chunk]
  (let [lines (string/split (str (:partial @state) chunk) #"\n" -1)]
    (swap! state assoc :partial (peek lines))
    (doseq [line (pop lines)
            :when (not (string/blank? line))]
      (take-line! client line))))

(defn- take-error-output!
  "Passes each line the server writes to standard error to `on-log`,
  keeping the last of them to explain a failure."
  [{:keys [state]} chunk on-log]
  (let [kept-characters 2000
        tail (str (:error-tail @state) chunk)]
    (swap! state assoc :error-tail (subs tail (max 0 (- (count tail) kept-characters))))
    (when on-log
      (doseq [line (string/split-lines chunk)
              :when (not (string/blank? line))]
        (on-log line)))))

(defn- exited!
  "Fails every request still awaiting an answer, once the server is gone."
  [{:keys [state] :as client} how on-exit]
  (when-not (:exited? @state)
    (let [error-tail (string/trim (:error-tail @state))
          reason (cond-> (str "the server exited " how)
                   (seq error-tail) (str ": " (last (string/split-lines error-tail))))
          awaiting (:awaiting @state)]
      (swap! state assoc :exited? true :exit-reason reason :awaiting {})
      (doseq [[_id {:keys [reject timer]}] awaiting]
        (js/clearTimeout timer)
        (reject (task/coded-error (str "The server stopped, since " reason) "MCP_EXITED")))
      (when on-exit
        (on-exit client reason)))))

(defn start!
  "Runs `command` and opens an MCP session with it, resolving to the
  client once it answers initialize, or rejecting when it cannot start,
  exits first, or stays silent past `answer-milliseconds`."
  [{:keys [command arguments environment working-directory client-info on-exit on-log answer-milliseconds]
    :or {answer-milliseconds 15000}}]
  (let [^js child (.spawn child-process command (into-array arguments)
                          #js {:cwd working-directory
                               :env (js/Object.assign #js {} (.-env js/process) (clj->js environment))
                               :windowsHide true
                               :stdio #js ["pipe" "pipe" "pipe"]})
        state (atom {:sent 0 :awaiting {} :partial "" :error-tail "" :exited? false})
        client {:child child :state state :answer-milliseconds answer-milliseconds}]
    (.setEncoding (.-stdout child) "utf8")
    (.setEncoding (.-stderr child) "utf8")
    (.on (.-stdout child) "data" #(take-output! client %))
    (.on (.-stderr child) "data" #(take-error-output! client % on-log))
    (.on (.-stdin child) "error" (fn [_error] nil))
    (.on child "error" (fn [^js error] (exited! client (str "before it started (" (.-message error) ")") on-exit)))
    (.on child "exit" (fn [code signal]
                        (exited! client (if signal (str "on " signal) (str "with code " code)) on-exit)))
    (-> (request! client "initialize" {"protocolVersion" (first mcp/supported-versions)
                                       "capabilities" {}
                                       "clientInfo" client-info}
                  answer-milliseconds)
        (task/then (fn [result]
                     (send! client (mcp/notification "notifications/initialized" nil))
                     (assoc client :server-info (get result "serverInfo"))))
        (task/recover (fn [error]
                        (.kill child)
                        (task/rejected error))))))

(defn list-tools! [{:keys [answer-milliseconds] :as client}]
  (task/then (request! client "tools/list" {} answer-milliseconds)
             #(get % "tools")))

(defn call-tool!
  "Resolves to the server's result for tool `tool-name` with `arguments`."
  [client tool-name arguments]
  (let [tool-call-milliseconds 120000] ; a project-wide scan of a large codebase takes a while
    (request! client "tools/call" {"name" tool-name "arguments" arguments} tool-call-milliseconds)))

(defn running? [{:keys [state]}]
  (not (:exited? @state)))

(defn stop!
  "Closes the server's input, which ends a well-behaved server, and kills
  it if it lingers two seconds after."
  [{:keys [^js child state]}]
  (let [linger-milliseconds 2000]
    (when-not (:exited? @state)
      (.end (.-stdin child))
      (js/setTimeout #(when-not (:exited? @state) (.kill child)) linger-milliseconds))))

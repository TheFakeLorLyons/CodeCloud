(ns codecloud.io.nrepl-test
  "Checks bencode against hand-written bytes, and the nREPL client against
  a stand-in server answering the way nREPL does."
  (:require ["net" :as net]
            [cljs.test :refer [async deftest is testing]]
            [clojure.string :as string]
            [codecloud.io.nrepl :as nrepl]))

(defn- text-of [^js buffer]
  (.toString buffer "utf8"))

(deftest bencode-matches-the-format
  (is (= "d4:code7:(+ 1 2)2:id1:74:linei3e2:op4:evale"
         (text-of (nrepl/encode {:op "eval" :code "(+ 1 2)" :id "7" :line 3})))
      "dictionary keys go out sorted")
  (is (= "2:λ" (text-of (nrepl/encode "λ"))) "a length counts bytes")
  (is (= "l5:helloi-12ed1:a0:ee" (text-of (nrepl/encode ["hello" -12 {:a ""}]))))
  (is (= "d28:nrepl.middleware.print/quotai9ee" (text-of (nrepl/encode {:nrepl.middleware.print/quota 9})))
      "a keyword key keeps its namespace")
  (is (thrown? js/Error (nrepl/encode {:op 1.5}))))

(deftest replies-arrive-in-any-pieces
  (let [first-reply {:id "1" :value "λ 3" :ns "user" :versions {:nrepl {:major 1}}}
        second-reply {:id "1" :status ["done"]}
        bytes (js/Buffer.concat #js [(nrepl/encode first-reply) (nrepl/encode second-reply)])]
    (doseq [split (range (inc (.-length bytes)))]
      (let [{early :values remainder :remainder} (nrepl/decode-all (.subarray bytes 0 split))
            {late :values left :remainder} (nrepl/decode-all (js/Buffer.concat #js [remainder (.subarray bytes split)]))]
        (is (= [first-reply second-reply] (into early late)) (str "split at byte " split))
        (is (zero? (.-length left)))))))

;; A stand-in server

(defn- reply!
  "Writes `message` in two pieces, as a network may deliver it."
  [^js socket message]
  (let [bytes (nrepl/encode message)
        middle (quot (.-length bytes) 2)]
    (.write socket (.subarray bytes 0 middle))
    (.write socket (.subarray bytes middle))))

(defn- answer!
  "Answers one request the way nREPL would, for the few codes the tests
  send. An endless loop runs until interrupted."
  [^js socket {:keys [op id code interrupt-id] :as request} running]
  (case op
    "clone" (reply! socket {:id id :new-session "session-1" :status ["done"]})
    "eval" (case code
             "(+ 1 2)" (do (reply! socket {:id id :value "3" :ns (:ns request "user")})
                           (reply! socket {:id id :status ["done"]}))
             "(/ 1 0)" (do (reply! socket {:id id :err "Execution error (ArithmeticException) at user/eval1 (REPL:1).\nDivide by zero\n"})
                           (reply! socket {:id id :ex "class java.lang.ArithmeticException" :status ["eval-error"]})
                           (reply! socket {:id id :status ["done"]}))
             "(loop [] (recur))" (reset! running id)
             "(System/exit 0)" (.destroy socket)
             (reply! socket {:id id :status ["done" "error"]}))
    "load-file" (do (reply! socket {:id id :value "#'app.core/start" :ns "user"})
                    (reply! socket {:id id :status ["done"]}))
    "interrupt" (if (= interrupt-id @running)
                  (do (reset! running nil)
                      (reply! socket {:id interrupt-id :status ["interrupted"]})
                      (reply! socket {:id interrupt-id :status ["done"]})
                      (reply! socket {:id id :status ["done"]}))
                  (reply! socket {:id id :status ["session-idle" "done"]}))
    "close" (reply! socket {:id id :status ["session-closed" "done"]})
    (reply! socket {:id id :status ["unknown-op" "done" "error"]})))

(defn- start-server!
  "Resolves to {:server :port :requests} for a stand-in server on a free
  port, which answers when `answer?`, recording every request."
  [answer?]
  (js/Promise.
   (fn [resolve _reject]
     (let [requests (atom [])
           ^js server (.createServer net
                                     (fn [^js socket]
                                       (let [pending (atom (js/Buffer.alloc 0))
                                             running (atom nil)]
                                         (.on socket "error" (fn [_error] nil))
                                         (.on socket "data"
                                              (fn [chunk]
                                                (let [{:keys [values remainder]} (nrepl/decode-all (js/Buffer.concat #js [@pending chunk]))]
                                                  (reset! pending remainder)
                                                  (doseq [request values]
                                                    (swap! requests conj request)
                                                    (when answer? (answer! socket request running)))))))))]
       (.listen server 0 "127.0.0.1"
                (fn [] (resolve {:server server :port (.-port (.address server)) :requests requests})))))))

(defn- replies-to
  "Resolves to every reply to the request `send!` makes with a reply
  handler, once one says it is done."
  [send!]
  (js/Promise.
   (fn [resolve _reject]
     (let [replies (atom [])]
       (send! (fn [reply]
                (swap! replies conj reply)
                (when (nrepl/done? reply) (resolve @replies))))))))

(defn- once
  "Resolves once `ready?` returns true, checking every few milliseconds."
  [ready?]
  (js/Promise.
   (fn [resolve _reject]
     (letfn [(check [] (if (ready?) (resolve true) (js/setTimeout check 5)))]
       (check)))))

(deftest a-session-evaluates-loads-and-stops
  (async done
    (let [closed (atom nil)
          server-holder (atom nil)
          connection-holder (atom nil)
          sent-op (fn [requests op] (last (filter #(= op (:op %)) @requests)))]
      (-> (start-server! true)
          (.then (fn [{:keys [port] :as started}]
                   (reset! server-holder started)
                   (nrepl/connect {:host "127.0.0.1" :port port :on-close #(reset! closed %)})))
          (.then (fn [connection]
                   (reset! connection-holder connection)
                   (replies-to #(nrepl/evaluate! connection {:id "sum" :code "(+ 1 2)" :ns "app.core"
                                                             :file "/work/src/app/core.clj" :line 4 :column 3}
                                                 %))))
          (.then (fn [replies]
                   (is (= ["3"] (keep :value replies)))
                   (is (= ["app.core"] (keep :ns replies)))
                   (is (= {:op "eval" :id "sum" :session "session-1" :ns "app.core" :file "/work/src/app/core.clj"
                           :line 4 :column 3 :nrepl.middleware.print/quota 32768}
                          (dissoc (sent-op (:requests @server-holder) "eval") :code))
                       "the request carries the session, where the code came from, and the print quota")
                   (replies-to #(nrepl/evaluate! @connection-holder {:code "(/ 1 0)"} %))))
          (.then (fn [replies]
                   (is (string/includes? (apply str (keep :err replies)) "Divide by zero"))
                   (is (= ["class java.lang.ArithmeticException"] (keep :ex replies)))
                   (is (= #{"eval-error" "done"} (set (mapcat :status replies))))
                   (let [running (replies-to #(nrepl/evaluate! @connection-holder {:id "spin" :code "(loop [] (recur))"} %))]
                     (nrepl/interrupt! @connection-holder "spin" (fn [_reply] nil))
                     running)))
          (.then (fn [replies]
                   (is (= #{"interrupted" "done"} (set (mapcat :status replies))))
                   (replies-to #(nrepl/load-file! @connection-holder {:code "(ns app.core)" :file "/work/src/app/core.clj"} %))))
          (.then (fn [replies]
                   (is (= ["#'app.core/start"] (keep :value replies)))
                   (is (= {:op "load-file" :file "(ns app.core)" :file-path "/work/src/app/core.clj" :file-name "core.clj"}
                          (select-keys (sent-op (:requests @server-holder) "load-file") [:op :file :file-path :file-name])))
                   (nrepl/close! @connection-holder)
                   (once #(some? @closed))))
          (.then (fn [_]
                   (is (identical? @connection-holder @closed) "closing reports the connection that closed")
                   (is (some? (sent-op (:requests @server-holder) "close")) "the session is closed on the server")))
          (.catch (fn [error] (is (nil? error) (str "the session failed: " (.-message error)))))
          (.finally (fn []
                      (some-> ^js (:server @server-holder) .close)
                      (done)))))))

(deftest a-lost-connection-ends-what-awaits-it
  (async done
    (let [closed (atom nil)
          server-holder (atom nil)]
      (-> (start-server! true)
          (.then (fn [{:keys [port] :as started}]
                   (reset! server-holder started)
                   (nrepl/connect {:host "127.0.0.1" :port port :on-close #(reset! closed %)})))
          (.then (fn [connection]
                   (replies-to #(nrepl/evaluate! connection {:code "(System/exit 0)"} %))))
          (.then (fn [replies]
                   (is (= [["done" "connection-closed"]] (mapv :status replies)))
                   (once #(some? @closed))))
          (.catch (fn [error] (is (nil? error) (str "the session failed: " (.-message error)))))
          (.finally (fn []
                      (some-> ^js (:server @server-holder) .close)
                      (done)))))))

(deftest a-server-that-is-not-nrepl-is-refused
  (async done
    (let [server-holder (atom nil)]
      (-> (start-server! false)
          (.then (fn [{:keys [port] :as started}]
                   (reset! server-holder started)
                   (nrepl/connect {:host "127.0.0.1" :port port :answer-milliseconds 60})))
          (.then (fn [_connection] (is false "a silent server should not give a connection"))
                 (fn [error] (is (= "NREPL_SILENT" (.-code error)))))
          (.then (fn [_]
                   (let [port (:port @server-holder)]
                     (.close ^js (:server @server-holder))
                     (reset! server-holder nil)
                     (nrepl/connect {:host "127.0.0.1" :port port}))))
          (.then (fn [_connection] (is false "a closed port should not give a connection"))
                 (fn [error] (is (= "ECONNREFUSED" (.-code error)))))
          (.finally (fn []
                      (some-> ^js (:server @server-holder) .close)
                      (done)))))))

(ns codecloud.vscode.stand-ins
  "What the host's tests share, a stand-in nREPL server answering as each
  test asks and a wait for a condition."
  (:require ["net" :as net]
            [codecloud.io.nrepl :as nrepl]))

(defn nrepl-server!
  "Resolves to {:server :port :requests} for a stand-in nREPL server on a
  free port. Each request but a clone goes to `answer!` with a function
  that sends a reply to it and the socket, and every request is
  recorded."
  [answer!]
  (js/Promise.
   (fn [resolve _reject]
     (let [requests (atom [])
           ^js server (.createServer net
                                     (fn [^js socket]
                                       (let [decoder (atom nrepl/empty-decoder)]
                                         (.on socket "error" (fn [_error] nil))
                                         (.on socket "data"
                                              (fn [chunk]
                                                (let [[values next-decoder] (nrepl/decode-chunk @decoder chunk)]
                                                  (reset! decoder next-decoder)
                                                  (doseq [{:keys [op id] :as request} values
                                                          :let [reply! #(.write socket (nrepl/encode (assoc % :id id)))]]
                                                    (swap! requests conj request)
                                                    (if (= "clone" op)
                                                      (reply! {:new-session "session-1" :status ["done"]})
                                                      (answer! request reply! socket)))))))))]
       (.listen server 0 "127.0.0.1"
                (fn [] (resolve {:server server :port (.-port (.address server)) :requests requests})))))))

(defn once
  "Resolves to what `found` returns once it returns something, checking
  every few milliseconds, or to nil after `patience` milliseconds when
  given."
  ([found] (once found nil))
  ([found patience]
   (let [started (js/Date.now)]
     (js/Promise.
      (fn [resolve _reject]
        (letfn [(check []
                  (if-let [result (found)]
                    (resolve result)
                    (if (and patience (> (- (js/Date.now) started) patience))
                      (resolve nil)
                      (js/setTimeout check 5))))]
          (check)))))))

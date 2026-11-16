(ns codecloud.io.ports-test
  (:require ["net" :as net]
            [cljs.test :refer [async deftest is]]
            [codecloud.io.ports :as ports]))

(deftest addresses-on-this-computer-have-a-port
  (is (= ["localhost" 1971] (ports/local-port "http://localhost:1971/notebook")))
  (is (= ["127.0.0.1" 80] (ports/local-port "http://127.0.0.1/")))
  (is (= ["::1" 443] (ports/local-port "https://[::1]/")))
  (is (nil? (ports/local-port "https://clojure.org/guides")))
  (is (nil? (ports/local-port "not an address"))))

(deftest a-server-answers-once-it-listens
  (async done
    (let [^js server (.createServer net)]
      (.listen server 0 "127.0.0.1"
               (fn []
                 (let [port (.-port (.address server))]
                   (-> (ports/answers? "localhost" port)
                       (.then (fn [answered?]
                                (is (true? answered?) "localhost reaches a server listening on 127.0.0.1")
                                (js/Promise. (fn [resolve] (.close server resolve)))))
                       (.then (fn [_] (ports/answers? "127.0.0.1" port)))
                       (.then (fn [answered?]
                                (is (false? answered?) "and nothing answers once it closes")
                                (done))))))))))

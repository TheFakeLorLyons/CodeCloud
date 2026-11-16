(ns codecloud.io.task-test
  (:require [cljs.test :refer [async deftest is]]
            [codecloud.io.task :as task]))

(deftest coded-errors-read-like-node-errors
  (let [error (task/coded-error "No nREPL answer on port 7888" "NREPL_SILENT")]
    (is (instance? js/Error error))
    (is (= "NREPL_SILENT" (.-code error)))
    (is (= "No nREPL answer on port 7888" (task/error-text error))))
  (is (= "plain words" (task/error-text "plain words"))))

(deftest map-limited-bounds-the-work-in-flight
  (async done
    (let [in-flight (atom 0)
          most-in-flight (atom 0)
          start-task (fn [item]
                       (swap! in-flight inc)
                       (swap! most-in-flight max @in-flight)
                       ;; Later items finish sooner, so results arrive out of order.
                       (-> (task/pause (- 40 (* 5 item)))
                           (task/then (fn [_] (swap! in-flight dec) (* item item)))))]
      (-> (task/map-limited 3 start-task (range 8))
          (task/then (fn [results]
                       (is (= [0 1 4 9 16 25 36 49] results))
                       (is (= 3 @most-in-flight))))
          (task/recover (fn [error] (is (nil? error))))
          (task/then (fn [_] (done)))))))

(deftest a-failed-step-reaches-recover
  (async done
    (-> (task/resolved 1)
        (task/then (fn [_] (task/rejected (task/coded-error "gone" "MCP_EXITED"))))
        (task/then (fn [_] (is false "a rejected step skips the steps after it")))
        (task/recover (fn [error] (.-code error)))
        (task/then (fn [code]
                     (is (= "MCP_EXITED" code))
                     (done))))))

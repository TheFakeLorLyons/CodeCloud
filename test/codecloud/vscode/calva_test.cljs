(ns codecloud.vscode.calva-test
  "Checks a REPL tab joined to Calva's REPL against a stand-in for
  Calva's extension API, answering as Calva's first API version does."
  (:require [cljs.test :refer [async deftest is testing]]
            [codecloud.vscode.calva :as calva]
            [codecloud.vscode.stand-ins :as stand-ins :refer [once]]))

(def ^:private both-sessions
  [{:replSessionKey "clj" :replType "clj" :currentRoutedTarget true}
   {:replSessionKey "cljs" :replType "cljs" :currentRoutedTarget false}])

(defn- stand-in-api
  "A stand-in for Calva's API whose sessions are those `sessions`, an
  atom, holds, answering each evaluation with `answer`, a function of the
  code and the options' map, and recording every evaluation and listener
  in `seen`."
  [sessions answer seen]
  #js {:repl #js {:listSessions (fn [] (clj->js @sessions))
                  :evaluate (fn [code ^js options]
                              (swap! seen update :evaluations (fnil conj [])
                                     {:code code :ns (.-ns options) :who (.-who options) :session (.-sessionKey options)
                                      :file (some-> options .-nReplOptions .-file)
                                      :line (some-> options .-nReplOptions .-line)})
                              (swap! seen update :message-ids (fnil conj []) (some-> options .-nReplOptions .-id))
                              (when-let [stdout (some-> options .-output .-stdout)] (stdout "printed\n"))
                              (answer code options))
                  :onOutputLogged (fn [listener]
                                    (swap! seen assoc :output-listener listener)
                                    #js {:dispose #(swap! seen update :disposed (fnil conj #{}) :output)})
                  :onSessionsChanged (fn [listener]
                                       (swap! seen assoc :sessions-listener listener)
                                       #js {:dispose #(swap! seen update :disposed (fnil conj #{}) :sessions)})}})

(defn- tab
  "A Calva tab's context, its replies to the view gathered in `posted`
  and its files' texts in `texts`."
  [api posted texts]
  {:calva (calva/create)
   :api api
   :root-directory "/work"
   :post! #(swap! posted conj (dissoc % :protocol/version :type))
   :read-text #(js/Promise.resolve (get texts %))
   :interrupt-all! #(swap! posted conj {:interrupted? true})})

(defn- ended [posted id]
  (once #(some (fn [{reply-id :id :keys [status]}] (and (= id reply-id) (some #{"done"} status))) @posted)))

(defn- status? [message]
  (keyword? (:status message)))

(defn- statuses
  "The connection statuses posted, each [status message]."
  [posted]
  (mapv (juxt :status :message) (filter status? @posted)))

(defn- replies
  "What was posted besides connection statuses."
  [posted]
  (vec (remove status? @posted)))

(deftest joining-says-what-calva-holds
  (let [joined (fn [api]
                 (let [posted (atom [])]
                   (calva/connect! (tab api posted {}))
                   (mapv (juxt :status :message :via) @posted)))]
    (is (= [[:failed "Calva is not installed, or did not start." :calva]] (joined nil)))
    (is (= [[:failed "Calva has no REPL connected yet. Jack in or connect with Calva, and the tab joins it." :calva]]
           (joined (stand-in-api (atom []) (constantly nil) (atom {})))))
    (is (= [[:connected "Calva's REPL, session clj" :calva]]
           (joined (stand-in-api (atom (take 1 both-sessions)) (constantly nil) (atom {})))))
    (is (= [[:connected "Calva's REPL, sessions clj and cljs" :calva]]
           (joined (stand-in-api (atom both-sessions) (constantly nil) (atom {})))))))

(deftest code-runs-in-calvas-session-for-its-dialect
  (async done
    (let [seen (atom {})
          posted (atom [])
          api (stand-in-api (atom both-sessions)
                            (fn [code _options]
                              (js/Promise.resolve (if (= "(/ 1 0)" code)
                                                    #js {:result "nil" :ns "app.core" :error "Divide by zero"}
                                                    #js {:result "3" :ns "app.core"})))
                            seen)
          context (tab api posted {})]
      (calva/evaluate! context {:id "e1" :code "(+ 1 2)" :ns "app.core" :path "src/app/core.cljs" :line 4 :column 1})
      (-> (ended posted "e1")
          (.then (fn [_]
                   (is (= [[:connected "Calva's REPL, sessions clj and cljs"]] (statuses posted))
                       "the tab joins Calva's REPL for its first evaluation")
                   (is (= {:code "(+ 1 2)" :ns "app.core" :who "CodeCloud" :session "cljs" :file "/work/src/app/core.cljs" :line 4}
                          (first (:evaluations @seen)))
                       "a .cljs file's code goes to the ClojureScript session, marked with its place, under CodeCloud's name")
                   (is (= [{:id "e1" :out "printed\n"} {:id "e1" :value "3" :ns "app.core" :status ["done"]}] (rest @posted)))
                   (calva/evaluate! context {:id "e2" :code "(/ 1 0)" :ns "app.core" :path "src/app/core.clj"})
                   (ended posted "e2")))
          (.then (fn [_]
                   (is (= "clj" (:session (second (:evaluations @seen)))))
                   (is (= {:id "e2" :ex "Divide by zero" :status ["done" "eval-error"]} (last @posted)))
                   (calva/evaluate! context {:id "e3" :code "(str 1)" :ns "" :path "src/app/core.cljc"})
                   (ended posted "e3")))
          (.then (fn [_]
                   (is (= [nil "user"] ((juxt :session :ns) (last (:evaluations @seen))))
                       "Calva routes other files itself, and code naming no namespace runs in the default one")))
          (.finally done)))))

(deftest a-namespace-the-repl-lacks-gets-its-ns-form-first
  (async done
    (let [seen (atom {})
          posted (atom [])
          loaded (atom #{})
          api (stand-in-api (atom both-sessions)
                            (fn [code ^js options]
                              (cond
                                (re-find #"^\(ns " code) (do (swap! loaded conj "app.core")
                                                             (js/Promise.resolve #js {:result "nil" :ns "app.core"}))
                                ;; nREPL sends no value for code in a namespace it lacks, which Calva
                                ;; reports as an empty result.
                                (not (contains? @loaded (.-ns options))) (js/Promise.resolve #js {:result "" :ns (.-ns options)})
                                :else (js/Promise.resolve #js {:result "\"hi\"" :ns "app.core"})))
                            seen)
          texts {"src/app/core.clj" "(ns app.core\n  (:require [clojure.string]))\n\n(defn greet [] \"hi\")\n"}
          context (tab api posted texts)]
      (calva/evaluate! context {:id "e1" :code "(greet)" :ns "app.core" :path "src/app/core.clj" :line 4 :column 1})
      (-> (ended posted "e1")
          (.then (fn [_]
                   (is (= ["(greet)" "(ns app.core\n  (:require [clojure.string]))" "(greet)"] (mapv :code (:evaluations @seen))))
                   (is (= [1 "user"] ((juxt :line :ns) (second (:evaluations @seen)))) "the ns form goes from its own line")
                   (is (= ["The REPL had not loaded app.core, so its ns form in core.clj goes first."] (keep :note @posted)))
                   (is (= {:id "e1" :value "\"hi\"" :ns "app.core" :status ["done"]} (last @posted)))
                   (calva/evaluate! context {:id "e2" :code "(greet)" :ns "app.other" :path "src/app/core.clj" :line 4 :column 1})
                   (ended posted "e2")))
          (.then (fn [_]
                   (is (= {:id "e2" :status ["done" "namespace-not-found"]} (last @posted))
                       "a file whose ns form names another namespace cannot help")
                   (calva/evaluate! context {:id "e3" :code ";; a note" :ns "app.other"})
                   (ended posted "e3")))
          (.then (fn [_]
                   (is (= {:id "e3" :status ["done"]} (last @posted)) "code without forms has no value to give")))
          (.finally done)))))

(deftest calva-going-away-ends-the-request
  (async done
    (let [posted (atom [])
          context (tab (stand-in-api (atom both-sessions) (fn [_ _] (js/Promise.reject (js/Error. "The REPL is not connected."))) (atom {}))
                       posted {})]
      (calva/connect! context)
      (reset! posted [])
      (calva/evaluate! context {:id "e1" :code "(+ 1 2)" :ns "user"})
      (-> (ended posted "e1")
          (.then (fn [_]
                   (is (= [[:disconnected "The REPL is not connected."]] (statuses posted)) "the tab hears that it left Calva's REPL")
                   (is (= ["done" "not-connected"] (:status (last @posted))))))
          (.finally done)))))

(deftest files-load-in-order-through-calva
  (async done
    (let [seen (atom {})
          posted (atom [])
          api (stand-in-api (atom both-sessions)
                            (fn [code _options]
                              (js/Promise.resolve (if (re-find #"broken" code)
                                                    #js {:result "nil" :error "Unable to resolve symbol: oops"}
                                                    #js {:result "nil" :ns "user"})))
                            seen)
          context (tab api posted {"src/a.clj" "(ns a)" "src/broken.clj" "(ns broken) (oops)" "src/c.clj" "(def c 1)"})]
      (calva/load-files! context {:id "l1" :paths ["src/a.clj" "src/broken.clj" "src/c.clj"]})
      (-> (ended posted "l1")
          (.then (fn [_]
                   (is (= ["Loaded src/a.clj"] (keep :note @posted)))
                   (is (= ["done" "eval-error"] (:status (last @posted))) "the load stops at the file that fails")
                   (is (= ["(ns a)" "(ns broken) (oops)"] (map :code (:evaluations @seen))))
                   (is (= [1 1] (map :line (:evaluations @seen))) "each file goes as itself, from its first line")))
          (.finally done)))))

(deftest stopping-an-evaluation-through-calva
  (async done
    (let [posted (atom [])
          seen (atom {})
          running (atom nil)
          started (atom nil)
          message-id #(first (:message-ids @seen))]
      (-> (stand-ins/nrepl-server!
           (fn [{:keys [op session interrupt-id]} reply! _socket]
             (case op
               "ls-sessions" (reply! {:sessions ["calva-main" "session-1"] :status ["done"]})
               "interrupt" (reply! {:status (if (and (= "calva-main" session) (= (message-id) interrupt-id))
                                              ["done"]
                                              ["error" "interrupt-id-mismatch" "done"])})
               (reply! {:status ["done"]}))))
          (.then (fn [{:keys [port requests] :as server}]
                   (reset! started server)
                   (let [^js api (stand-in-api (atom both-sessions)
                                               (fn [_ _] (js/Promise. (fn [resolve _reject] (reset! running resolve))))
                                               seen)
                         context (assoc (tab api posted {})
                                        :interrupt-all! (fn []
                                                          (swap! posted conj {:interrupted? true})
                                                          ;; Calva ends an evaluation it stopped with this error.
                                                          (@running #js {:result "nil" :ns "user" :error "Evaluation was interrupted"})))]
                     (set! (.. api -repl -listConnections) (fn [] #js [#js {:clientKey "c" :host "127.0.0.1" :port port}]))
                     (calva/interrupt! context)
                     (is (empty? @requests) "a tab with nothing running leaves Calva and its server alone")
                     (calva/evaluate! context {:id "e1" :code "(Thread/sleep 60000)" :ns "user"})
                     (is (calva/running? (:calva context)))
                     (calva/interrupt! context)
                     (-> (ended posted "e1")
                         (.then (fn [_]
                                  (let [interrupts (filter #(= "interrupt" (:op %)) @requests)]
                                    (is (= #{"calva-main" "session-1"} (set (map :session interrupts)))
                                        "each of the server's sessions hears the request")
                                    (is (and (some? (message-id)) (every? #(= (message-id) (:interrupt-id %)) interrupts))
                                        "naming the evaluation by the message id CodeCloud gave it"))
                                  (is (some :interrupted? @posted) "then Calva's command ends what Calva waits for")
                                  (is (= ["done" "interrupted"] (:status (last @posted))))
                                  (is (not (calva/running? (:calva context))))))))))
          (.catch (fn [error] (is (nil? error) (.-message error))))
          (.finally (fn [] (some-> ^js (:server @started) .close) (done)))))))

(deftest stopping-through-calva-alone
  (async done
    (let [posted (atom [])
          context (tab (stand-in-api (atom both-sessions) (fn [_ _] (js/Promise. (fn [_resolve _reject] nil))) (atom {}))
                       posted {})]
      (calva/evaluate! context {:id "e1" :code "(Thread/sleep 60000)" :ns "user"})
      (calva/interrupt! context)
      (-> (once #(some :interrupted? @posted))
          (.then (fn [asked?] (is asked? "without the servers' addresses, Calva's command is asked at once")))
          (.finally done)))))

(deftest the-tab-follows-calvas-connections
  (let [sessions (atom [])
        seen (atom {})
        posted (atom [])
        context (tab (stand-in-api sessions (fn [_ _] (js/Promise.resolve #js {:result "2" :ns "user"})) seen) posted {})
        sessions-changed! #((:sessions-listener @seen) #js {:type "session-added"})]
    (calva/connect! context)
    (testing "a tab waiting for Calva joins when Calva connects a REPL"
      (reset! sessions both-sessions)
      (sessions-changed!)
      (is (= [:failed :connected] (mapv first (statuses posted)))))
    (testing "and leaves when Calva's last REPL goes"
      (reset! sessions [])
      (sessions-changed!)
      (is (= [:disconnected "Calva's REPL disconnected. The tab joins it again when Calva connects."] (last (statuses posted)))))
    (testing "code typed into a tab that left joins it again"
      (reset! sessions both-sessions)
      (calva/close! context)
      (is (= #{:output :sessions} (:disposed @seen)) "leaving stops following Calva")
      (reset! posted [])
      (calva/evaluate! context {:id "e1" :code "(+ 1 1)" :ns "user"})
      (is (= [[:connected "Calva's REPL, sessions clj and cljs"]] (statuses posted))))))

(deftest what-calva-evaluates-elsewhere-shows-in-the-tab
  (let [seen (atom {})
        posted (atom [])
        context (tab (stand-in-api (atom both-sessions) (constantly nil) seen) posted {})]
    (calva/connect! context)
    (let [listener (:output-listener @seen)]
      ;; Calva starts code and values on a fresh line.
      (listener #js {:category "evaluatedCode" :text "\n(+ 1 1)" :who "ui" :ns "user"})
      (listener #js {:category "evaluationOutput" :text "hi\n" :who "ui"})
      (listener #js {:category "evaluationResults" :text "\n2" :who "ui" :ns "user"})
      (listener #js {:category "evaluatedCode" :text "(own)" :who "CodeCloud" :ns "user"})
      (listener #js {:category "otherOutput" :text "; connected" :who "ui"}))
    (is (= [{:id "calva-1" :input "(+ 1 1)" :ns "user"} {:id "calva-1" :out "hi\n"} {:id "calva-1" :value "2" :ns "user"}]
           (replies posted))
        "Calva's own evaluations show, while CodeCloud's and Calva's other notes do not")
    (calva/close! context)
    (is (= [:disconnected "Left Calva's REPL."] (last (statuses posted))))))

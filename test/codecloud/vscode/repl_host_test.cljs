(ns codecloud.vscode.repl-host-test
  "Checks a REPL tab's host side against stand-in nREPL servers answering
  as Clojure's nREPL and nbb's do."
  (:require ["fs" :as fs]
            ["os" :as os]
            ["path" :as path]
            [cljs.test :refer [async deftest is]]
            [clojure.string :as string]
            [codecloud.vscode.repl-host :as repl-host]
            [codecloud.vscode.stand-ins :as stand-ins :refer [once]]))

;; Stand-in servers

(defn- clojure-server
  "Answers as Clojure's nREPL does, each value of `values`, a map from
  code to what it prints, coming from the namespace the request asks for,
  and code outside the map answering nil."
  [values]
  (fn [{:keys [op code ns]} reply! _socket]
    (case op
      "describe" (reply! {:versions {:clojure {:version-string "1.12.0"}} :status ["done"]})
      "eval" (let [value (get values code "nil")]
               (doseq [part (if (sequential? value) value [value])]
                 (if (map? part) (reply! part) (reply! {:value part :ns (or ns "user")})))
               (reply! {:status ["done"]}))
      "interrupt" (reply! {:status ["unknown-op" "done" "error"]})
      (reply! {:status ["done"]}))))

(defn- nbb-server
  "Answers as nbb does, running code in the namespace the last ns form
  named, whatever a request asks for, while in-ns moves nowhere."
  []
  (let [current (atom "user")]
    (fn [{:keys [op code]} reply! _socket]
      (case op
        "describe" (reply! {:versions {:nbb {:version-string "1.3.0"}} :status ["done"]})
        "eval" (do (when-let [[_ named] (re-matches #"\(ns (\S+)\)" code)]
                     (reset! current named))
                   (reply! {:value "nil" :ns @current})
                   (reply! {:status ["done"] :ns @current}))
        (reply! {:status ["done"]})))))

;; A REPL tab

(defn- tab
  "A REPL tab's context, its replies to the view gathered in `posted`, its
  files read from `texts`, a map from path to text."
  [posted texts]
  {:repl (repl-host/create)
   :root-directory "/work"
   :post! #(swap! posted conj (dissoc % :protocol/version :type))
   :read-text #(js/Promise.resolve (get texts %))})

(defn- ended
  "Resolves once the view has the end of request `id`."
  [posted id]
  (once #(some (fn [{reply-id :id :keys [status]}] (and (= id reply-id) (some #{"done"} status))) @posted)))

(defn- connected
  "Resolves once `context`, posting to `posted`, connects to the
  stand-in at `port`."
  [context posted port]
  (repl-host/connect! context {:host "127.0.0.1" :port port})
  (once #(some (fn [{:keys [status]}] (= :connected status)) @posted)))

(defn- replies-to [posted id]
  (filterv #(= id (:id %)) @posted))

(defn- sent [requests op]
  (filterv #(= op (:op %)) @requests))

(defn- checked
  "Runs `steps`, a function of the stand-in started with `answer!`, then
  closes `context`'s connection, stops the stand-in, and ends the test."
  [done context answer! steps]
  (let [server-holder (atom nil)]
    (-> (stand-ins/nrepl-server! answer!)
        (.then (fn [started] (reset! server-holder started) (steps started)))
        (.catch (fn [error] (is (nil? error) (str "the exchange failed: " (.-message error)))))
        (.finally (fn []
                    (repl-host/close! context)
                    (some-> ^js (:server @server-holder) .close)
                    (done))))))

;; The checks

(deftest a-namespace-the-repl-lacks-gets-its-ns-form-first
  (async done
    (let [posted (atom [])
          context (tab posted {"src/app/core.clj" ";; The app.\n\n(ns app.core\n  (:require [clojure.string]))\n(defn f [] 1)"
                               "src/app/other.clj" "(ns app.other)"})]
      (checked done context
               (clojure-server {"(clojure.core/some? (clojure.core/find-ns 'app.core))" "false"
                                "(clojure.core/some? (clojure.core/find-ns 'app.lost))" "false"
                                "(f)" "1"})
               (fn [{:keys [port requests]}]
                 (-> (connected context posted port)
                     (.then (fn [_]
                              (repl-host/evaluate! context {:id "e1" :code "(f)" :ns "app.core" :path "src/app/core.clj" :line 5 :column 1})
                              (ended posted "e1")))
                     (.then (fn [_]
                              (let [[ns-request code-request] (remove #(string/includes? (:code %) "find-ns") (sent requests "eval"))]
                                (is (= "(ns app.core\n  (:require [clojure.string]))" (:code ns-request)))
                                (is (= [3 1 "/work/src/app/core.clj"] ((juxt :line :column :file) ns-request))
                                    "the ns form is marked where it stands in its file")
                                (is (= ["(f)" "app.core"] ((juxt :code :ns) code-request))))
                              (is (= ["The REPL had not loaded app.core, so its ns form in core.clj goes first." "1"]
                                     (keep #(or (:note %) (:value %)) (remove #(= "nil" (:value %)) (replies-to posted "e1")))))
                              (repl-host/evaluate! context {:id "e2" :code "(g)" :ns "app.lost" :path "src/app/other.clj"})
                              (ended posted "e2")))
                     (.then (fn [_]
                              (is (= [["done" "namespace-not-found"]] (keep :status (replies-to posted "e2")))
                                  "a file whose ns form names another namespace loads nothing")
                              (is (not-any? #(string/includes? (:code %) "app.other") (sent requests "eval")))))))))))

(deftest code-moving-namespaces-itself-gets-no-note
  (async done
    (let [posted (atom [])
          context (tab posted {})
          two-forms "(def x 1)\n(in-ns 'app.core)"]
      (checked done context
               (clojure-server {two-forms ["#'user/x" {:value "#namespace[app.core]" :ns "app.core"}]
                                "*ns*" ["#namespace[app.util]"]})
               (fn [{:keys [port requests]}]
                 (-> (connected context posted port)
                     (.then (fn [_]
                              (repl-host/evaluate! context {:id "e1" :code two-forms :ns "user"})
                              (ended posted "e1")))
                     (.then (fn [_]
                              (is (not-any? :note (replies-to posted "e1")))
                              (repl-host/evaluate! context {:id "e2" :code "*ns*" :ns "app.util"})
                              (ended posted "e2")))
                     (.then (fn [_]
                              (is (= [two-forms "*ns*"] (map :code (sent requests "eval")))
                                  "the next evaluation goes as it is, with no move first")
                              (is (not (false? (:honors-ns? @(:repl context)))))))))))))

(deftest a-server-moving-only-by-ns-forms
  (async done
    (let [posted (atom [])
          context (tab posted {})]
      (checked done context
               (nbb-server)
               (fn [{:keys [port requests]}]
                 (-> (connected context posted port)
                     (.then (fn [_]
                              (repl-host/evaluate! context {:id "e1" :code "(in-ns 'app.util)" :ns "app.core"})
                              (ended posted "e1")))
                     (.then (fn [_]
                              (is (= ["(ns app.util)"] (map :code (sent requests "eval")))
                                  "a lone in-ns form goes as the ns form that moves there")
                              (is (= "app.util" (some :ns (replies-to posted "e1"))))
                              (repl-host/evaluate! context {:id "e2" :code "(+ 1 2)" :ns "app.core"})
                              (ended posted "e2")))
                     (.then (fn [_]
                              (is (= ["(ns app.core)" "(+ 1 2)"] (map :code (drop 1 (sent requests "eval"))))
                                  "code for another namespace moves there first")
                              (is (not-any? :note (replies-to posted "e2")))))))))))

(deftest a-load-cut-off-by-a-closed-connection-stops
  (async done
    (let [posted (atom [])
          context (tab posted {"src/a.clj" "(ns a)" "src/b.clj" "(ns b)"})]
      (checked done context
               (fn [{:keys [op]} reply! ^js socket]
                 (case op
                   "describe" (reply! {:versions {:clojure {:version-string "1.12.0"}} :status ["done"]})
                   "load-file" (.destroy socket)
                   (reply! {:status ["done"]})))
               (fn [{:keys [port requests]}]
                 (-> (connected context posted port)
                     (.then (fn [_]
                              (repl-host/load-files! context {:id "l1" :paths ["src/a.clj" "src/b.clj"]})
                              (ended posted "l1")))
                     (.then (fn [_]
                              (is (= [["done" "connection-closed"]] (keep :status (replies-to posted "l1"))))
                              (is (not-any? :note (replies-to posted "l1")) "the file cut off does not count as loaded")
                              (is (= 1 (count (sent requests "load-file"))))))))))))

(deftest a-value-cut-short-says-so
  (async done
    (let [posted (atom [])
          context (tab posted {})]
      (checked done context
               (clojure-server {"(range)" [{:value "(0 1 2" :ns "user" :status ["nrepl.middleware.print/truncated"]}]})
               (fn [{:keys [port]}]
                 (-> (connected context posted port)
                     (.then (fn [_]
                              (repl-host/evaluate! context {:id "e1" :code "(range)" :ns "user"})
                              (ended posted "e1")))
                     (.then (fn [_]
                              (is (= ["(0 1 2 …"] (keep :value (replies-to posted "e1"))))))))))))

(deftest a-server-that-cannot-interrupt-says-so-beside-the-evaluation
  (async done
    (let [posted (atom [])
          context (tab posted {})]
      (checked done context
               (fn [{:keys [op code]} reply! _socket]
                 (case op
                   "describe" (reply! {:versions {:clojure {:version-string "1.12.0"}} :status ["done"]})
                   "eval" (when-not (= "(loop [] (recur))" code) (reply! {:status ["done"]}))
                   (reply! {:status ["unknown-op" "done" "error"]})))
               (fn [{:keys [port]}]
                 (-> (connected context posted port)
                     (.then (fn [_]
                              (repl-host/evaluate! context {:id "e1" :code "(loop [] (recur))" :ns "user"})
                              (once #(contains? (:pending @(:repl context)) "e1"))))
                     (.then (fn [_]
                              (repl-host/interrupt! context)
                              (once #(some :err (replies-to posted "e1")))))
                     (.then (fn [_]
                              (is (= ["This REPL cannot interrupt an evaluation.\n"] (keep :err (replies-to posted "e1"))))
                              (is (not-any? :status (replies-to posted "e1")) "the evaluation still runs")))))))))

(deftest a-connect-superseded-by-closing-lets-later-connects-try-again
  (async done
    (let [posted (atom [])
          folder (.mkdtempSync fs (path/join (.tmpdir os) "codecloud-repl-"))
          context (assoc (tab posted {}) :root-directory folder)
          servers (atom [])]
      (-> (js/Promise.all #js [(stand-ins/nrepl-server! (clojure-server {})) (stand-ins/nrepl-server! (clojure-server {}))])
          (.then (fn [started]
                   (let [[port-file-server other-server] (reset! servers (vec started))]
                     (.writeFileSync fs (path/join folder ".nrepl-port") (str (:port port-file-server)))
                     (-> (connected context posted (:port port-file-server))
                         (.then (fn [_]
                                  (repl-host/connect! context {:host "127.0.0.1" :port (:port other-server)})
                                  (repl-host/close! context)
                                  ;; The superseded connection closes itself once it opens.
                                  (once #(some (fn [{:keys [op]}] (= "close" op)) @(:requests other-server)))))
                         (.then (fn [_]
                                  (reset! posted [])
                                  (repl-host/connect! context {})
                                  (once #(some (fn [{:keys [status]}] (= :connected status)) @posted) 3000)))
                         (.then (fn [reconnected?]
                                  (is reconnected? "a connect after the superseded one reads the port file and connects")))))))
          (.catch (fn [error] (is (nil? error) (.-message error))))
          (.finally (fn []
                      (repl-host/close! context)
                      (doseq [{:keys [^js server]} @servers] (.close server))
                      (.rmSync fs folder #js {:recursive true :force true})
                      (done)))))))

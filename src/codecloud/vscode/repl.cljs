(ns codecloud.vscode.repl
  "The REPL of one cloud. It finds the project's nREPL server through the
  port file the server writes, keeps one connection to it, and passes
  evaluations from the view to the server and every reply back. Each
  function takes a context of {:repl :root-directory :post!}, :post!
  sending a message to the view."
  (:require ["path" :as path]
            [clojure.string :as string]
            [codecloud.io.files :as files]
            [codecloud.io.nrepl :as nrepl]
            [codecloud.io.task :as task]
            [codecloud.protocol :as protocol]))

(def ^:private port-files
  "Files nREPL servers write their port into, relative to the folder they
  serve: nREPL itself, Leiningen, and nbb, then shadow-cljs."
  [".nrepl-port" ".shadow-cljs/nrepl.port"])

(def ^:private local-hosts
  "Where a server named by a port file listens: the IPv4 loopback, then
  the IPv6 one, since servers bind to either."
  ["127.0.0.1" "::1"])

(defn create
  "The REPL state of a cloud that has not connected yet."
  []
  (atom {:connection nil :opening nil :attempt 0 :pending #{} :closing? false}))

;; Finding the server

(defn- read-port
  "Resolves to the port written in the file at `file-path`, or nil."
  [file-path]
  (task/then (files/read-text file-path)
             (fn [text]
               (let [highest-port 65535
                     port (some->> text string/trim (re-matches #"\d{1,5}") js/parseInt)]
                 (when (and port (<= 1 port highest-port)) port)))))

(defn- folders-upward
  "`directory` and every folder above it, nearest first."
  [directory]
  (let [parent (path/dirname directory)]
    (cons directory (when (not= parent directory) (lazy-seq (folders-upward parent))))))

(defn find-port
  "Resolves to {:port :port-file} for the nearest port file in
  `directory` or a folder above it, or nil when there is none."
  [directory]
  (letfn [(search [candidates]
            (if-let [candidate (first candidates)]
              (task/then (read-port candidate)
                         (fn [port] (if port {:port port :port-file candidate} (search (rest candidates)))))
              (task/resolved nil)))]
    (search (for [folder (folders-upward directory)
                  port-file port-files]
              (path/join folder port-file)))))

;; Telling the view

(defn- post-status! [{:keys [post!]} status fields]
  (post! (protocol/message :host/repl-status (assoc fields :status status))))

(defn- post-output!
  "Passes a reply to the view, with an exception's description cut to its
  first line, since some servers print a whole stack into it."
  [{:keys [post!]} reply]
  (let [longest-description 200
        description (some-> (:ex reply) string/split-lines first)]
    (post! (protocol/message :host/repl-output
                             (cond-> (select-keys reply [:id :out :err :value :ns :status])
                               description (assoc :ex (subs description 0 (min longest-description (count description)))))))))

(defn- runtime-label
  "What the server runs, read from its describe reply's versions, such as
  Clojure 1.12.0, or nil."
  [versions]
  (some (fn [[version-key runtime]]
          (when-let [{:keys [version-string]} (get versions version-key)]
            (if (re-find #"^\d" (str version-string)) (str runtime " " version-string) runtime)))
        [[:clojure "Clojure"] [:babashka "Babashka"] [:nbb "nbb"] [:nbb-nrepl "nbb"] [:clojurescript "ClojureScript"]]))

(defn- missing-text [root-directory]
  (str "No nREPL port file in " (path/basename root-directory) " or a folder above it. "
       "Start an nREPL server there, or type its port and press Enter."))

(defn- failure-text
  "Words for why the connection to `found` did not open."
  [^js error {:keys [port port-file]}]
  (let [named-by (when port-file (str ", which " (path/basename port-file) " names"))]
    (case (.-code error)
      "ECONNREFUSED" (str "Nothing answers on port " port named-by ". Start the nREPL server, or type another port.")
      "NREPL_SILENT" (str "What answers on port " port " does not speak nREPL.")
      (str "Could not reach port " port named-by ": " (.-message error)))))

(defn- announce-connected!
  "Tells the view the connection opened, naming what the server runs once
  it says, or after a moment without an answer."
  [{:keys [repl] :as context} connection port]
  (let [patience-milliseconds 1500
        announced? (atom false)
        announce! (fn [label]
                    (when (and (not @announced?) (identical? connection (:connection @repl)))
                      (reset! announced? true)
                      (post-status! context :connected {:port port :message (str (or label "nREPL") " on port " port)})))]
    (js/setTimeout #(announce! nil) patience-milliseconds)
    (nrepl/request! connection {:op "describe"}
                    (fn [{:keys [versions] :as reply}]
                      (when (or versions (nrepl/done? reply))
                        (announce! (runtime-label versions)))))))

;; Connecting

(defn- closed!
  "Forgets a connection that ended and tells the view."
  [{:keys [repl] :as context} connection]
  (when (identical? connection (:connection @repl))
    (let [deliberate? (:closing? @repl)]
      (swap! repl assoc :connection nil :pending #{} :closing? false)
      (post-status! context :disconnected {:port (:port connection)
                                           :message (if deliberate? "Disconnected." "The REPL server closed the connection.")}))))

(defn- connect-any
  "Resolves to a connection to the first of `hosts` that takes one at
  `port`. When every host refuses, or a later one cannot be tried, such
  as IPv6 on a machine without it, rejects with the first refusal."
  [hosts port callbacks]
  (task/recover (nrepl/connect (assoc callbacks :host (first hosts) :port port))
                (fn [^js error]
                  (if (and (next hosts) (= "ECONNREFUSED" (.-code error)))
                    (task/recover (connect-any (next hosts) port callbacks)
                                  (fn [^js later-error]
                                    (task/rejected (if (= "ECONNREFUSED" (.-code later-error)) later-error error))))
                    (task/rejected error)))))

(defn- open!
  "Resolves to the REPL's connection, opening one first when there is
  none, or when `target`, a {:host :port} the viewer typed, names a
  server; without a target, the server a port file names. Rejects when
  no connection opens, having told the view why."
  [{:keys [repl root-directory] :as context} target]
  (let [{:keys [connection opening]} @repl]
    (cond
      (and connection (nil? target)) (task/resolved connection)
      (and opening (nil? target)) opening
      :else
      (let [attempt (:attempt (swap! repl update :attempt inc))
            current? #(= attempt (:attempt @repl))
            take-connection (fn [opened-connection port]
                              (if (current?)
                                (let [previous (:connection @repl)]
                                  (swap! repl assoc :connection opened-connection :opening nil)
                                  (when previous (nrepl/close! previous))
                                  (announce-connected! context opened-connection port)
                                  opened-connection)
                                (do (nrepl/close! opened-connection)
                                    (task/rejected (js/Error. "A newer connection took this one's place.")))))
            opened (-> (if target (task/resolved target) (find-port root-directory))
                       (task/then
                        (fn [{:keys [host port] :as found}]
                          (if-not found
                            (task/rejected (js/Error. (missing-text root-directory)))
                            (do (when (current?)
                                  (post-status! context :connecting {:port port :message (str "Connecting to port " port "…")}))
                                (-> (connect-any (if host [host] local-hosts) port
                                                 {:on-close #(closed! context %)
                                                  :on-unclaimed #(post-output! context %)})
                                    (task/recover (fn [error] (task/rejected (js/Error. (failure-text error found)))))
                                    (task/then #(take-connection % port)))))))
                       (task/recover (fn [^js error]
                                       (when (current?)
                                         (swap! repl assoc :opening nil)
                                         (post-status! context :failed {:message (.-message error)}))
                                       (task/rejected error))))]
        (swap! repl assoc :opening opened)
        opened))))

(defn announce!
  "Tells the view about the open connection again, as after it reloaded."
  [{:keys [repl] :as context}]
  (when-let [connection (:connection @repl)]
    (announce-connected! context connection (:port connection))))

(defn connect!
  "Connects to the server at the :host and :port the view sent, or
  without a port to the one a port file names, telling the view how it
  went."
  [{:keys [repl] :as context} {:keys [host port]}]
  (if (and (nil? port) (:connection @repl))
    (announce! context)
    (task/recover (open! context (when port {:host host :port port})) (fn [_error] nil))))

(defn close!
  "Closes the REPL's connection, or gives up the one opening."
  [{:keys [repl] :as context}]
  (let [{:keys [connection opening]} (swap! repl update :attempt inc)]
    (cond
      connection (do (swap! repl assoc :closing? true)
                     (nrepl/close! connection))
      opening (do (swap! repl assoc :opening nil)
                  (post-status! context :disconnected {:message "Disconnected."})))))

;; Evaluating

(defn- relay!
  "Passes a reply to the view, forgetting its request once it is done."
  [{:keys [repl] :as context} reply]
  (when (nrepl/done? reply)
    (swap! repl update :pending disj (:id reply)))
  (post-output! context reply))

(defn- send-request!
  "Starts request `id` with `start!`, given the connection and a reply
  handler, connecting first when needed, or ends it as not connected."
  [{:keys [repl] :as context} id start!]
  (-> (open! context nil)
      (task/then (fn [connection]
                   (swap! repl update :pending conj id)
                   (start! connection #(relay! context %))))
      (task/recover (fn [_error]
                      (post-output! context {:id id :status ["done" "not-connected"]})))))

(defn evaluate!
  "Evaluates code the view sent, read from a file of the cloud when it
  names one."
  [{:keys [root-directory] :as context} {:keys [id code ns path line column]}]
  (send-request! context id
                 (fn [connection on-reply]
                   (nrepl/evaluate! connection
                                    (cond-> {:id id :code code :ns ns}
                                      path (assoc :file (files/absolute-path root-directory path) :line line :column column))
                                    on-reply))))

(defn load-file!
  "Loads a file of the cloud with the text the view holds for it."
  [{:keys [root-directory] :as context} {:keys [id code path]}]
  (send-request! context id
                 (fn [connection on-reply]
                   (nrepl/load-file! connection {:id id :code code :file (files/absolute-path root-directory path)}
                                     on-reply))))

(defn interrupt!
  "Asks the server to stop every evaluation of this cloud still running,
  saying once when it cannot."
  [{:keys [repl] :as context}]
  (when-let [connection (:connection @repl)]
    (let [told? (atom false)]
      (doseq [id (:pending @repl)]
        (nrepl/interrupt! connection id
                          (fn [{:keys [status] :as reply}]
                            (when (and (some #{"unknown-op"} status) (not @told?))
                              (reset! told? true)
                              (post-output! context {:id (:id reply)
                                                     :err "This REPL cannot interrupt an evaluation.\n"
                                                     :status ["done"]}))))))))

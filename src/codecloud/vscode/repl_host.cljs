(ns codecloud.vscode.repl-host
  "One REPL tab of a cloud. It finds the project's nREPL server through
  the port file the server writes, keeps one connection to it, and passes
  evaluations from the view to the server and every reply back. Each
  function takes a context of {:repl :root-directory :post! :read-text},
  :post! sending a message to the view and :read-text resolving to the
  text of a file of the cloud, as VS Code holds it."
  (:require ["path" :as path]
            [clojure.string :as string]
            [codecloud.io.files :as files]
            [codecloud.io.nrepl :as nrepl]
            [codecloud.io.task :as task]
            [codecloud.presentation.editing-state :as editing-state]
            [codecloud.presentation.forms-state :as forms-state]
            [codecloud.presentation.repl-state :as repl-state]
            [codecloud.protocol :as protocol]))

(def ^:private port-files
  "Files nREPL servers write their port into, relative to the folder they
  serve, that of nREPL itself, Leiningen, and nbb, then that of
  shadow-cljs."
  [".nrepl-port" ".shadow-cljs/nrepl.port"])

(def ^:private local-hosts
  "Where a server named by a port file listens, on the IPv4 loopback or
  else the IPv6 one, since servers bind to either."
  ["127.0.0.1" "::1"])

(defn create
  "The state of a REPL tab that has not connected yet."
  []
  (atom {:connection nil :opening nil :attempt 0 :pending #{} :closing? false}))

;; Finding the server

(defn- read-port [file-path]
  (task/then (files/read-text file-path)
             (fn [text]
               (let [highest-port 65535
                     port (some->> text string/trim (re-matches #"\d{1,5}") js/parseInt)]
                 (when (and port (<= 1 port highest-port)) port)))))

(defn- folders-upward [directory]
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
  first line, since some servers print a whole stack into it, and a value
  the server cut short marked so."
  [{:keys [post!]} {:keys [value status] :as reply}]
  (let [longest-description 200
        description (some-> (:ex reply) string/split-lines first)]
    (post! (protocol/message :host/repl-output
                             (cond-> (select-keys reply [:id :out :err :value :ns :status :note])
                               description (assoc :ex (subs description 0 (min longest-description (count description))))
                               (and value (some #{"nrepl.middleware.print/truncated"} status)) (assoc :value (str value " …")))))))

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

(defn- failure-text [^js error {:keys [port port-file]}]
  (let [named-by (when port-file (str ", which " (path/basename port-file) " names"))]
    (case (.-code error)
      "ECONNREFUSED" (str "Nothing answers on port " port named-by ". Start the nREPL server, or type another port.")
      "NREPL_SILENT" (str "What answers on port " port " does not speak nREPL.")
      (str "Could not reach port " port named-by ": " (.-message error)))))

(defn- namespace-habits
  "How a server treats namespaces, read from its versions. nbb runs code
  in the namespace its last ns form named, so code for another namespace
  needs an ns form first."
  [versions]
  (if (some #(contains? versions %) [:nbb :nbb-nrepl])
    {:honors-ns? false :switches-by-ns-form? true}
    {}))

(defn- announce-connected!
  "Tells the view the connection opened, naming what the server runs once
  it says, or after a moment without an answer, and notes how the server
  treats namespaces. Resolves to the connection once announced."
  [{:keys [repl] :as context} connection port]
  (js/Promise.
   (fn [resolve _reject]
     (let [patience-milliseconds 1500
           announced? (atom false)
           announce! (fn [label]
                       (when-not @announced?
                         (reset! announced? true)
                         (when (identical? connection (:connection @repl))
                           (post-status! context :connected {:port port :message (str (or label "nREPL") " on port " port)}))
                         (resolve connection)))]
       (js/setTimeout #(announce! nil) patience-milliseconds)
       (nrepl/request! connection {:op "describe"}
                       (fn [{:keys [versions] :as reply}]
                         (when versions
                           (swap! repl merge (namespace-habits versions)))
                         (when (or versions (nrepl/done? reply))
                           (announce! (runtime-label versions)))))))))

;; Connecting

(defn- closed! [{:keys [repl] :as context} connection]
  (when (identical? connection (:connection @repl))
    (let [deliberate? (:closing? @repl)]
      (swap! repl assoc :connection nil :pending #{} :closing? false :current-ns nil)
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
  "Resolves to the REPL's connection, opening one first when there is none
  or when `target` names a server, else the server a port file names.
  Rejects after telling the view why."
  [{:keys [repl root-directory] :as context} target]
  (let [{:keys [connection opening]} @repl]
    (cond
      (and opening (nil? target)) opening
      (and connection (nil? target)) (task/resolved connection)
      :else
      (let [attempt (:attempt (swap! repl update :attempt inc))
            current? #(= attempt (:attempt @repl))
            take-connection (fn [opened-connection port]
                              (if (current?)
                                (let [previous (:connection @repl)]
                                  (swap! repl assoc
                                         :connection opened-connection
                                         :known-namespaces #{}
                                         :current-ns nil
                                         :honors-ns? nil
                                         :switches-by-ns-form? false)
                                  (when previous (nrepl/close! previous))
                                  (announce-connected! context opened-connection port))
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
                                         (post-status! context :failed {:message (.-message error)}))
                                       (task/rejected error))))
            ;; Whether it opened, failed, or was superseded, this attempt is
            ;; over, so later connects try again unless a newer one opens.
            settled! #(swap! repl (fn [current] (if (identical? opened (:opening current)) (assoc current :opening nil) current)))]
        (swap! repl assoc :opening opened)
        (-> opened
            (task/then (fn [opened-connection] (settled!) opened-connection))
            (task/recover (fn [error] (settled!) (task/rejected error))))))))

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

(defn- note-namespace! [{:keys [repl]} {reported :ns}]
  (when-not (string/blank? reported)
    (swap! repl assoc :current-ns reported)))

(defn- relay! [{:keys [repl] :as context} reply]
  (note-namespace! context reply)
  (when (nrepl/done? reply)
    (swap! repl update :pending disj (:id reply)))
  (post-output! context reply))

(defn- send-request!
  "Starts request `id` by calling `start!` with the connection, connecting
  first when needed, or ends the request as not connected. `reply-id`,
  the id the view knows, is `id` unless given."
  ([context id start!] (send-request! context id id start!))
  ([{:keys [repl] :as context} id reply-id start!]
   (-> (open! context nil)
       (task/then (fn [connection]
                    (swap! repl update :pending conj id)
                    (start! connection)))
       (task/recover (fn [_error]
                       (post-output! context {:id reply-id :status ["done" "not-connected"]}))))))

(defn- trouble
  "What went wrong, as a reply reports it, one of :interrupted, :closed
  for a connection that ended, and :failed, or nil."
  [{:keys [ex status]}]
  (cond
    (some #{"interrupted"} status) :interrupted
    (some #{"connection-closed"} status) :closed
    (or ex (some #{"eval-error"} status)) :failed))

(defn- trouble-status [outcome]
  (case outcome
    :interrupted ["done" "interrupted"]
    :closed ["done" "connection-closed"]
    ["done" "eval-error"]))

(defn- passed-output!
  "Passes a reply's printed output and errors to the view under
  `reply-id`, leaving out values and the end."
  [context reply-id reply]
  (note-namespace! context reply)
  (let [output (select-keys reply [:out :err :ex])]
    (when (seq output)
      (post-output! context (assoc output :id reply-id)))))

(defn- probe-able?
  "True for a namespace name that reads as a symbol, so a probe can name
  it in code."
  [namespace-name]
  (boolean (re-matches #"[^\s()\[\]{}\"';@^`~\\]+" (str namespace-name))))

(defn- namespace-known
  "Resolves to whether the REPL has namespace `namespace-name`, asking it
  with find-ns once per connection. A REPL that cannot answer counts as
  having it, so the code goes as it is."
  [{:keys [repl] :as context} namespace-name]
  (if (contains? (:known-namespaces @repl) namespace-name)
    (task/resolved true)
    (js/Promise.
     (fn [resolve _reject]
       (let [probe-id (str "probe-" (:probes (swap! repl update :probes (fnil inc 0))))
             answer (atom nil)]
         (-> (open! context nil)
             (task/then (fn [connection]
                          (nrepl/evaluate! connection {:id probe-id
                                                       :code (str "(clojure.core/some? (clojure.core/find-ns '" namespace-name "))")}
                                           (fn [{:keys [value] :as reply}]
                                             (when value (reset! answer value))
                                             (when (nrepl/done? reply)
                                               (let [known? (not= "false" @answer)]
                                                 (when known?
                                                   (swap! repl update :known-namespaces (fnil conj #{}) namespace-name))
                                                 (resolve known?)))))))
             (task/recover (fn [_error] (resolve true)))))))))

(defn- load-ns-form-then!
  "Evaluates the ns form of the file at `path`, then calls `continue!`,
  ending request `id` when the file names no such namespace or the form
  fails."
  [{:keys [repl read-text root-directory] :as context} {:keys [id ns path]} continue!]
  (let [end-unloaded! #(post-output! context {:id id :status ["done" "namespace-not-found"]})]
    (-> (read-text path)
        (task/then
         (fn [text]
           (if-let [{ns-code :code :keys [line column]} (some-> text (repl-state/ns-form ns))]
             (let [ns-id (str id "-ns")
                   outcome (atom nil)]
               (post-output! context {:id id :note (repl-state/ns-form-note ns path)})
               (send-request! context ns-id id
                              (fn [connection]
                                (nrepl/evaluate! connection {:id ns-id :code ns-code
                                                             :file (files/absolute-path root-directory path)
                                                             :line line :column column}
                                                 (fn [reply]
                                                   (passed-output! context id reply)
                                                   (when-let [found (trouble reply)] (reset! outcome found))
                                                   (when (nrepl/done? reply)
                                                     (swap! repl update :pending disj ns-id)
                                                     (if @outcome
                                                       (post-output! context {:id id :status (trouble-status @outcome)})
                                                       (do (swap! repl update :known-namespaces (fnil conj #{}) ns)
                                                           (continue!)))))))))
             (end-unloaded!))))
        (task/recover (fn [_error] (end-unloaded!))))))

(defn- read-code [code]
  (forms-state/read-forms (editing-state/text-lines code)))

(defn- names-namespace? [code]
  (some? (forms-state/namespace-at (read-code code) [0 0])))

(defn- starts-in-namespace? [code by-ns-form-only?]
  (boolean (re-find (if by-ns-form-only? #"^\s*\(ns\s" #"^\s*\((?:ns|in-ns)\s") code)))

(defn- as-ns-form
  "`code` for a server that switches namespaces only by an ns form, a lone
  in-ns form becoming the ns form that switches there."
  [code]
  (let [forms (read-code code)
        [{:keys [kind children]}] forms
        target (forms-state/namespace-at forms [0 0])]
    (if (and (= 1 (count forms)) (= :list kind) (= "in-ns" (:text (first children))) target)
      (str "(ns " target ")")
      code)))

(defn- run-in-namespace!
  "Evaluates `request` on `connection`, moving a server that ignores the
  namespace asked for into it first by an ns form, where in-ns does
  nothing."
  [{:keys [repl] :as context} connection {:keys [id ns] :as request} on-reply]
  (let [{:keys [honors-ns? current-ns switches-by-ns-form?]} @repl
        {:keys [code] :as request} (cond-> request switches-by-ns-form? (update :code as-ns-form))]
    (if (and ns (false? honors-ns?) (not= ns current-ns) (not (starts-in-namespace? code switches-by-ns-form?)))
      (nrepl/evaluate! connection {:id (str id "-switch")
                                   :code (if switches-by-ns-form? (str "(ns " ns ")") (str "(in-ns '" ns ")"))}
                       (fn [reply]
                         (note-namespace! context reply)
                         (when (nrepl/done? reply)
                           (nrepl/evaluate! connection request on-reply))))
      (nrepl/evaluate! connection request on-reply))))

(defn evaluate!
  "Evaluates code the view sent, loading first the ns form of a namespace
  the REPL lacks. A server answering from outside the namespace asked for
  is moved into it before each evaluation."
  [{:keys [repl root-directory] :as context} {:keys [id code path line column] :as message}]
  (let [ns (when-not (string/blank? (:ns message)) (:ns message))
        note-ignored-namespace! (fn [ran-in]
                                  (swap! repl assoc :honors-ns? false)
                                  (post-output! context {:id id
                                                         :note (str "The REPL ran this in " ran-in ", not " ns " as asked, "
                                                                    "since it ignores the namespace an evaluation asks for. "
                                                                    "CodeCloud moves it into the namespace first from now on.")}))
        send-code! (fn []
                     (send-request! context id
                                    (fn [connection]
                                      (let [valued? (atom false)]
                                        (run-in-namespace!
                                         context connection
                                         (cond-> {:id id :code code :ns ns}
                                           (and path line) (assoc :file (files/absolute-path root-directory path)
                                                                  :line line :column column))
                                         (fn [{ran-in :ns :keys [value] :as reply}]
                                           (when (and value (not @valued?))
                                             (reset! valued? true)
                                             (when (and ns (not (string/blank? ran-in)) (not= ns ran-in) (not (names-namespace? code)))
                                               (note-ignored-namespace! ran-in)))
                                           (relay! context reply)))))))]
    (if (and ns path (probe-able? ns))
      (task/then (namespace-known context ns)
                 (fn [known?] (if known? (send-code!) (load-ns-form-then! context message send-code!))))
      (send-code!))))

(defn load-file!
  "Loads a file of the cloud with the text the view holds for it."
  [{:keys [root-directory] :as context} {:keys [id code path]}]
  (send-request! context id
                 (fn [connection]
                   (nrepl/load-file! connection {:id id :code code :file (files/absolute-path root-directory path)}
                                     #(relay! context %)))))

(defn load-files!
  "Loads the files at `paths`, in order, with the texts VS Code holds for
  them, as request `id`, noting each as it loads and ending at the first
  that fails or after the last."
  [{:keys [repl read-text root-directory] :as context} {:keys [id paths]}]
  (letfn [(load-next! [remaining loaded]
            (if-let [file-path (first remaining)]
              (-> (read-text file-path)
                  (task/then
                   (fn [text]
                     (if (nil? text)
                       (post-output! context {:id id :err (str "Could not read " file-path ".\n") :status ["done" "eval-error"]})
                       (let [part-id (str id "-" loaded)
                             outcome (atom nil)]
                         (send-request! context part-id id
                                        (fn [connection]
                                          (nrepl/load-file! connection {:id part-id :code (repl-state/file-code file-path text)
                                                                        :file (files/absolute-path root-directory file-path)}
                                                            (fn [reply]
                                                              (passed-output! context id reply)
                                                              (when-let [found (trouble reply)] (reset! outcome found))
                                                              (when (nrepl/done? reply)
                                                                (swap! repl update :pending disj part-id)
                                                                (if @outcome
                                                                  (post-output! context {:id id :status (trouble-status @outcome)})
                                                                  (do (post-output! context {:id id :note (str "Loaded " file-path)})
                                                                      (load-next! (rest remaining) (inc loaded)))))))))))))
                  (task/recover (fn [^js error]
                                  (post-output! context {:id id :err (str (.-message error) "\n") :status ["done" "eval-error"]}))))
              (post-output! context {:id id :value (str "Loaded " loaded (if (= 1 loaded) " file" " files")) :status ["done"]})))]
    (load-next! paths 0)))

(defn interrupt!
  "Asks the server to stop every evaluation of this tab still running,
  saying once, beside the evaluation, when it cannot."
  [{:keys [repl] :as context}]
  (when-let [connection (:connection @repl)]
    (let [told? (atom false)]
      (doseq [id (:pending @repl)]
        (nrepl/interrupt! connection id
                          (fn [{:keys [status]}]
                            (when (and (some #{"unknown-op"} status) (not @told?))
                              (reset! told? true)
                              (post-output! context {:id id :err "This REPL cannot interrupt an evaluation.\n"}))))))))

(ns codecloud.vscode.calva
  "A REPL tab joined to Calva's own REPL through Calva's extension API,
  so the cloud and Calva's output window share one session. Each
  function takes a context of {:calva :api :root-directory :post!
  :read-text :interrupt-all!}, :api being Calva's API, or nil without
  Calva."
  (:require [clojure.string :as string]
            [codecloud.collections :as collections]
            [codecloud.io.files :as files]
            [codecloud.io.nrepl :as nrepl]
            [codecloud.io.task :as task]
            [codecloud.presentation.editing-state :as editing-state]
            [codecloud.presentation.forms-state :as forms-state]
            [codecloud.presentation.repl-state :as repl-state]
            [codecloud.protocol :as protocol]
            [goog.object :as gobj]))

(def ^:private who
  "The name Calva's output window gives the evaluations CodeCloud sends."
  "CodeCloud")

(def ^:private interrupted-error
  "The error Calva ends an evaluation with when it stops it."
  "Evaluation was interrupted")

(defn create
  "The state of a tab not yet joined to Calva. Its :requests map each
  request running to the nREPL message id of the evaluation it waits
  for, and :interrupted holds the requests asked to stop."
  []
  (atom {:subscriptions {} :mirrored 0 :joined? false :requests {} :interrupted #{}}))

(defn running?
  "True while a request the tab with atom `calva` sent through Calva
  runs."
  [calva]
  (boolean (seq (:requests @calva))))

(defn- post-status! [{:keys [post!]} status message]
  (post! (protocol/message :host/repl-status {:status status :message message :via :calva})))

(defn- post-output! [{:keys [post!]} fields]
  (post! (protocol/message :host/repl-output fields)))

(defn- sessions
  "Calva's REPL sessions as [{:key :type :routed?}], or nil when its API
  cannot list them."
  [^js api]
  (when (fn? (some-> api .-repl .-listSessions))
    (mapv (fn [^js session]
            {:key (.-replSessionKey session) :type (.-replType session) :routed? (boolean (.-currentRoutedTarget session))})
          (array-seq (.listSessions (.-repl api))))))

(defn- session-key
  "The key of Calva's session for code from `path`, the first Calva routes
  to among its sessions for the file's dialect, or nil to leave the
  choice to Calva."
  [sessions path]
  (let [path (str path)
        session-type (cond (string/ends-with? path ".cljs") "cljs"
                           (string/ends-with? path ".clj") "clj")
        matching (filter #(= session-type (:type %)) sessions)]
    (when session-type
      (:key (or (first (filter :routed? matching)) (first matching))))))

(defn- joined-message [sessions]
  (case (count sessions)
    0 "Calva's REPL"
    1 (str "Calva's REPL, session " (:key (first sessions)))
    (str "Calva's REPL, sessions " (collections/spoken-list (map :key sessions)))))

;; Following Calva

(defn- mirror!
  "Passes one piece of Calva's output to the view, the code Calva
  evaluated opening a request of its own. Calva starts code and values
  on a fresh line, which the tab's log gives them anyway."
  [{:keys [calva] :as context} ^js message]
  (let [category (.-category message)
        mirrored (:mirrored (if (= "evaluatedCode" category) (swap! calva update :mirrored inc) @calva))
        id (str "calva-" mirrored)
        text (str (.-text message))]
    (case category
      "evaluatedCode" (post-output! context {:id id :input (string/trim text) :ns (.-ns message)})
      "evaluationResults" (post-output! context {:id id :value (string/trim text) :ns (.-ns message)})
      "evaluationOutput" (post-output! context {:id id :out text})
      "evaluationErrorOutput" (post-output! context {:id id :err text})
      nil)))

(defn- subscribe!
  "Hands `listener` to `event`, a subscription of Calva's REPL API, once
  for the tab, keeping what it returns to dispose of on leaving."
  [{:keys [calva ^js api]} event listener]
  (let [^js calva-repl (some-> api .-repl)
        subscribe (some-> calva-repl (gobj/get event))]
    (when (and (fn? subscribe) (not (contains? (:subscriptions @calva) event)))
      (swap! calva assoc-in [:subscriptions event] (.call subscribe calva-repl listener)))))

(defn- unsubscribe! [{:keys [calva]}]
  (doseq [[_event ^js subscription] (:subscriptions @calva)]
    (.dispose subscription))
  (swap! calva assoc :subscriptions {}))

(declare connect!)

(defn- follow-calva!
  "Shows in the tab what Calva evaluates from its own editors and output
  window, and keeps the tab in step with Calva's connections."
  [{:keys [calva api] :as context}]
  (subscribe! context "onOutputLogged"
              (fn [^js message]
                (when-not (= who (.-who message))
                  (mirror! context message))))
  (subscribe! context "onSessionsChanged"
              (fn [_event]
                (let [found (sessions api)
                      joined? (:joined? @calva)]
                  (cond
                    (and joined? found (empty? found))
                    (do (swap! calva assoc :joined? false)
                        (post-status! context :disconnected "Calva's REPL disconnected. The tab joins it again when Calva connects."))

                    (and (not joined?) (seq found))
                    (connect! context))))))

;; Joining

(defn connect!
  "Joins the tab to Calva's REPL when Calva has one connected, telling
  the view how it went. From then on the tab follows Calva, and joins
  when Calva connects a REPL."
  [{:keys [calva api] :as context}]
  (if (nil? api)
    (post-status! context :failed "Calva is not installed, or did not start.")
    (let [found (sessions api)]
      (follow-calva! context)
      (if (and found (empty? found))
        (post-status! context :failed "Calva has no REPL connected yet. Jack in or connect with Calva, and the tab joins it.")
        (do (swap! calva assoc :joined? true)
            (post-status! context :connected (joined-message found)))))))

(defn announce!
  "Tells the view about the tab again, as after it reloaded."
  [{:keys [calva] :as context}]
  (when (:joined? @calva)
    (connect! context)))

(defn close!
  "Lets go of Calva's REPL, which stays as Calva has it, and stops
  following Calva."
  [{:keys [calva] :as context}]
  (unsubscribe! context)
  (swap! calva assoc :joined? false)
  (post-status! context :disconnected "Left Calva's REPL."))

;; Evaluating

(defn- send-request!
  "Starts request `id` by calling `start!`, after joining the tab to
  Calva's REPL when it has not joined. A tab that cannot join ends the
  request as not connected, its status saying why."
  [{:keys [calva] :as context} id start!]
  (when-not (:joined? @calva)
    (connect! context))
  (if (:joined? @calva)
    (do (swap! calva assoc-in [:requests id] nil)
        (start!))
    (post-output! context {:id id :status ["done" "not-connected"]})))

(defn- end! [{:keys [calva] :as context} id fields]
  (swap! calva #(-> % (update :requests dissoc id) (update :interrupted disj id)))
  (post-output! context (assoc fields :id id)))

(defn- interrupted? [{:keys [calva]} id]
  (contains? (:interrupted @calva) id))

(defn- evaluated
  "Resolves to {:value :ns} or {:error}, with :unjoined? once Calva is
  gone, for `code` evaluated through Calva in `ns`, its output passed on
  as it comes."
  [{:keys [calva ^js api] :as context} id code ns {:keys [path file line column]}]
  (if-not (some-> api .-repl)
    (task/resolved {:error "Calva is not running." :unjoined? true})
    (let [^js calva-repl (.-repl api)
          message-id (str "codecloud-" (random-uuid))
          output #js {:stdout #(post-output! context {:id id :out %})
                      :stderr #(post-output! context {:id id :err %})}
          chosen-session (session-key (sessions api) path)
          options (cond-> {:ns ns :who who :output output
                           :nReplOptions (cond-> {:id message-id}
                                           file (assoc :file file :line line :column column))}
                    chosen-session (assoc :sessionKey chosen-session))]
      (swap! calva assoc-in [:requests id] message-id)
      (-> (if (fn? (.-evaluate calva-repl))
            (.evaluate calva-repl code (clj->js options))
            (.evaluateCode calva-repl chosen-session code ns output))
          (task/then (fn [^js result]
                       (if (.-error result)
                         {:error (str (.-error result))}
                         {:value (not-empty (str (.-result result))) :ns (.-ns result)})))
          (task/recover (fn [^js error]
                          {:error (task/error-text error) :unjoined? true}))))))

(defn- finish!
  "Ends request `id` after `outcome`, the end of an evaluation through
  Calva. One the tab or Calva stopped ends as interrupted, and one Calva
  could not take ends as not connected, the tab leaving Calva's REPL."
  [{:keys [calva] :as context} id {:keys [value error unjoined?] result-ns :ns}]
  (cond
    (or (interrupted? context id) (= interrupted-error error)) (end! context id {:status ["done" "interrupted"]})
    unjoined? (do (swap! calva assoc :joined? false)
                  (post-status! context :disconnected error)
                  (end! context id {:status ["done" "not-connected"]}))
    error (end! context id {:ex error :status ["done" "eval-error"]})
    :else (end! context id (cond-> {:status ["done"]} value (assoc :value value :ns result-ns)))))

(defn- missing-namespace?
  "True for an evaluation of `code` that ended with neither a value nor
  an error although the code holds forms, as one ends in a namespace the
  REPL has not loaded."
  [{:keys [value error]} code]
  (and (nil? value) (nil? error) (boolean (seq (forms-state/read-forms (editing-state/text-lines code))))))

(defn- load-ns-form-then!
  "Evaluates through Calva the ns form of the file at `path`, then calls
  `continue!`, ending request `id` when the file names no such namespace
  or the form fails."
  [{:keys [read-text root-directory] :as context} {:keys [id ns path]} continue!]
  (let [end-unloaded! #(end! context id {:status ["done" "namespace-not-found"]})]
    (-> (read-text path)
        (task/then
         (fn [text]
           (if-let [{ns-code :code :keys [line column]} (some-> text (repl-state/ns-form ns))]
             (do (post-output! context {:id id :note (repl-state/ns-form-note ns path)})
                 (task/then (evaluated context id ns-code (repl-state/default-namespace path)
                                       {:path path :file (files/absolute-path root-directory path) :line line :column column})
                            (fn [{:keys [error] :as outcome}]
                              (if (or error (interrupted? context id)) (finish! context id outcome) (continue!)))))
             (end-unloaded!))))
        (task/recover (fn [_error] (end-unloaded!))))))

(defn evaluate!
  "Evaluates code the view sent in Calva's session, marked with its file
  and line when it comes from one. Code for a namespace the REPL has not
  loaded runs again after the ns form of the file the view named."
  [{:keys [root-directory] :as context} {:keys [id code path line column] :as message}]
  (let [ns (if (string/blank? (:ns message)) (repl-state/default-namespace path) (:ns message))
        place (cond-> {:path path}
                (and path line) (assoc :file (files/absolute-path root-directory path) :line line :column column))
        run-code #(evaluated context id code ns place)]
    (send-request! context id
                   (fn []
                     (task/then (run-code)
                                (fn [outcome]
                                  (cond
                                    (or (interrupted? context id) (not (missing-namespace? outcome code))) (finish! context id outcome)
                                    path (load-ns-form-then! context {:id id :ns ns :path path}
                                                             #(task/then (run-code) (fn [retried] (finish! context id retried))))
                                    :else (end! context id {:status ["done" "namespace-not-found"]}))))))))

(defn load-file!
  "Loads a file of the cloud with the text the view holds for it, its
  forms evaluated in order in Calva's session."
  [context {:keys [path] :as message}]
  (evaluate! context (assoc message :ns (repl-state/default-namespace path) :line 1 :column 1)))

(defn load-files!
  "Loads the files at `paths` in order through Calva's session, with the
  texts VS Code holds for them, as request `id`, noting each as it loads
  and ending at the first that fails or after the last."
  [{:keys [read-text root-directory] :as context} {:keys [id paths]}]
  (letfn [(load-next! [remaining loaded]
            (if-let [file-path (first remaining)]
              (-> (read-text file-path)
                  (task/then
                   (fn [text]
                     (cond
                       (interrupted? context id)
                       (finish! context id {})

                       (nil? text)
                       (end! context id {:err (str "Could not read " file-path ".\n") :status ["done" "eval-error"]})

                       :else
                       (task/then (evaluated context id (repl-state/file-code file-path text) (repl-state/default-namespace file-path)
                                             {:path file-path :file (files/absolute-path root-directory file-path)
                                              :line 1 :column 1})
                                  (fn [{:keys [error] :as outcome}]
                                    (if (or error (interrupted? context id))
                                      (finish! context id outcome)
                                      (do (post-output! context {:id id :note (str "Loaded " file-path)})
                                          (load-next! (rest remaining) (inc loaded)))))))))
                  (task/recover (fn [^js error]
                                  (end! context id {:err (str (.-message error) "\n") :status ["done" "eval-error"]}))))
              (end! context id {:value (str "Loaded " loaded (if (= 1 loaded) " file" " files")) :status ["done"]})))]
    (send-request! context id #(load-next! paths 0))))

;; Stopping

(defn- server-addresses
  "The {:host :port} of each nREPL server Calva is connected to, as far as
  its API tells them."
  [^js api]
  (when (fn? (some-> api .-repl .-listConnections))
    (keep (fn [^js connection]
            (when (and (.-host connection) (.-port connection))
              {:host (.-host connection) :port (.-port connection)}))
          (array-seq (.listConnections (.-repl api))))))

(defn- stop-on-server!
  "Asks the nREPL server at `address` to stop the evaluations with
  `message-ids`, resolving once every session answered, after `patience`
  milliseconds, or when the server is out of reach."
  [address message-ids]
  (let [patience 3000]
    (-> (nrepl/connect (assoc address :answer-milliseconds patience))
        (task/then
         (fn [connection]
           (js/Promise.
            (fn [resolve _reject]
              (let [finish! (fn [] (nrepl/close! connection) (resolve nil))
                    timer (js/setTimeout finish! patience)
                    finished! (fn [] (js/clearTimeout timer) (finish!))]
                (nrepl/request! connection {:op "ls-sessions"}
                                (fn [{:keys [sessions] :as reply}]
                                  (when (nrepl/done? reply)
                                    (let [asks (for [session sessions message-id message-ids] [session message-id])
                                          unanswered (atom (count asks))]
                                      (when (empty? asks) (finished!))
                                      (doseq [[session message-id] asks]
                                        (nrepl/request! connection {:op "interrupt" :session session :interrupt-id message-id}
                                                        (fn [answer]
                                                          (when (and (nrepl/done? answer) (zero? (swap! unanswered dec)))
                                                            (finished!))))))))))))))
        (task/recover (fn [_error] nil)))))

(defn interrupt!
  "Stops what the tab runs through Calva. Calva's own command checks the
  JVM in the busy session first, so CodeCloud asks the servers to stop
  the tab's evaluations by id before running it."
  [{:keys [calva api interrupt-all!]}]
  (let [{:keys [requests]} @calva]
    (when (seq requests)
      (swap! calva update :interrupted into (keys requests))
      (-> (task/all (map #(stop-on-server! % (keep val requests)) (server-addresses api)))
          (task/then (fn [_] (interrupt-all!)))))))

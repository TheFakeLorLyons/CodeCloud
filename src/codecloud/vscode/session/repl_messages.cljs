(ns codecloud.vscode.session.repl-messages
  "A session's handlers for REPL tabs, Calva's tab, and shell tabs, and
  evaluation from VS Code's editor."
  (:require ["path" :as path]
            [clojure.string :as string]
            [codecloud.io.task :as task]
            [codecloud.protocol :as protocol]
            [codecloud.vscode.api :as api]
            [codecloud.vscode.calva :as calva]
            [codecloud.vscode.editor-evaluation :as editor-evaluation]
            [codecloud.vscode.repl-host :as repl-host]
            [codecloud.vscode.session.common :as common]
            [codecloud.vscode.shells :as shells]))

(defn repl-context
  "What REPL tab `tab`'s functions need from a session, its REPL made when
  the tab is new. Replies to evaluations from VS Code's editors show
  there as well as in the tab."
  [{:keys [host repls root-directory] :as session} tab]
  {:repl (get (swap! repls #(if (contains? % tab) % (assoc % tab (repl-host/create)))) tab)
   :root-directory root-directory
   :post! (fn [message]
            (editor-evaluation/note-reply! (:editor-results host) message)
            (common/post! session (assoc message :tab tab)))
   :read-text (common/text-reader session)})

(defn with-calva-tab
  "Calls `act` with the context of tab `tab` once Calva's API resolves,
  when the tab is one for Calva's REPL, and returns true; nil for any
  other tab."
  [{:keys [host calva-tabs root-directory] :as session} tab act]
  (when-let [calva-state (get @calva-tabs tab)]
    (task/then (api/calva-api)
               (fn [calva-api]
                 (act {:calva calva-state
                       :api calva-api
                       :root-directory root-directory
                       :post! (fn [message]
                                (editor-evaluation/note-reply! (:editor-results host) message)
                                (common/post! session (assoc message :tab tab)))
                       :read-text (common/text-reader session)
                       :interrupt-all! #(api/execute-command "calva.interruptAllEvaluations")})))
    true))

(defn shell-context
  "What the shell tabs' functions need from a session."
  [{:keys [shells root-directory] :as session}]
  {:shells shells :root-directory root-directory :post! #(common/post! session %)})

(defmethod common/handle-message :view/repl-connect [{:keys [calva-tabs] :as session} {:keys [tab via] :as message}]
  (if (= :calva via)
    (do (swap! calva-tabs #(if (contains? % tab) % (assoc % tab (calva/create))))
        (with-calva-tab session tab calva/connect!))
    (repl-host/connect! (repl-context session tab) message)))

(defmethod common/handle-message :view/repl-disconnect [session {:keys [tab]}]
  (or (with-calva-tab session tab calva/close!)
      (repl-host/close! (repl-context session tab))))

(defmethod common/handle-message :view/repl-eval [session {:keys [tab] :as message}]
  (or (with-calva-tab session tab #(calva/evaluate! % message))
      (repl-host/evaluate! (repl-context session tab) message)))

(defmethod common/handle-message :view/repl-load-file [session {:keys [tab] :as message}]
  (or (with-calva-tab session tab #(calva/load-file! % message))
      (repl-host/load-file! (repl-context session tab) message)))

(defmethod common/handle-message :view/repl-load-files [session {:keys [tab] :as message}]
  (or (with-calva-tab session tab #(calva/load-files! % message))
      (repl-host/load-files! (repl-context session tab) message)))

(defmethod common/handle-message :view/repl-interrupt [session {:keys [tab]}]
  (or (with-calva-tab session tab calva/interrupt!)
      (repl-host/interrupt! (repl-context session tab))))

(defmethod common/handle-message :view/repl-close [{:keys [repls calva-tabs] :as session} {:keys [tab]}]
  (cond
    (contains? @calva-tabs tab) (do (with-calva-tab session tab calva/close!)
                                    (swap! calva-tabs dissoc tab))
    (contains? @repls tab) (do (repl-host/close! (repl-context session tab))
                               (swap! repls dissoc tab))
    :else (shells/close! (shell-context session) tab)))

(defmethod common/handle-message :view/shell-open [session message]
  (shells/open! (shell-context session) message))

(defmethod common/handle-message :view/shell-run [session message]
  (shells/run-command! (shell-context session) message))

(defmethod common/handle-message :view/shell-input [session message]
  (shells/send-input! (shell-context session) message))

(defmethod common/handle-message :view/shell-interrupt [session message]
  (shells/interrupt! (shell-context session) message))

(defmethod common/handle-message :view/list-terminals [session _message]
  (common/post! session (protocol/message :host/terminals {:terminals (shells/list-terminals)
                                                   :calva? (api/calva-installed?)})))

(defn evaluate-from-editor!
  "Passes an evaluation from a VS Code editor, `request` as
  codecloud.vscode.editor-evaluation/remember! returns it, to the
  cloud's view, which sends it to the REPL tab that evaluates."
  [{:keys [root-directory state] :as session} {:keys [id scope code path start namespace-name]}]
  (let [relative (string/replace (path/relative root-directory path) "\\" "/")
        [start-line start-column] start
        page-id (some (fn [[candidate node]] (when (some #(= relative (:path %)) (:files node)) candidate))
                      (:namespaces (:working-snapshot @state)))]
    (common/post! session (protocol/message :host/evaluate {:id id
                                                     :scope scope
                                                     :code code
                                                     :path relative
                                                     :namespace-id page-id
                                                     :namespace-name namespace-name
                                                     :line (inc start-line)
                                                     :column (inc start-column)}))))

(defn interrupt-evaluations!
  "Stops the evaluations of every REPL tab of the cloud."
  [{:keys [repls calva-tabs] :as session}]
  (doseq [tab (keys @repls)]
    (repl-host/interrupt! (repl-context session tab)))
  ;; Calva stops all of its evaluations at once, so one running tab asks.
  (when-let [tab (some (fn [[tab calva-state]] (when (calva/running? calva-state) tab)) @calva-tabs)]
    (with-calva-tab session tab calva/interrupt!)))

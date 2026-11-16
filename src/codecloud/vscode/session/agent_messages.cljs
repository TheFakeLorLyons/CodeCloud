(ns codecloud.vscode.session.agent-messages
  "A session's handlers for agents and the assistant, and what the cloud
  hears of them."
  (:require ["path" :as path]
            [clojure.string :as string]
            [codecloud.io.files :as files]
            [codecloud.model.config :as config]
            [codecloud.model.workspace :as workspace]
            [codecloud.protocol :as protocol]
            [codecloud.vscode.agents :as agents]
            [codecloud.vscode.api :as api]
            [codecloud.vscode.assistant-host :as assistant-host]
            [codecloud.vscode.session.common :as common]
            [codecloud.vscode.settings :as settings]))

(defn presence-message
  "Where agents work, as a cloud's view shows it, with paths relative to
  the cloud's folder and what lies outside that folder left out, while
  every agent stays listed."
  [{:keys [root-directory]} {:keys [agents pins]}]
  (let [relative (fn [file-path]
                   (when file-path
                     (let [found (path/relative root-directory file-path)]
                       (when-not (or (string/blank? found) (string/starts-with? found "..") (path/isAbsolute found))
                         (string/replace found "\\" "/")))))
        within (fn [entries]
                 (into [] (keep (fn [entry] (some->> (relative (:path entry)) (assoc entry :path)))) entries))]
    (protocol/message :host/presence
                      {:agents (mapv (fn [agent]
                                       (-> agent
                                           (update :place (fn [place] (some->> (relative (:path place)) (assoc place :path))))
                                           (update :reads within)
                                           (update :changes within)))
                                     agents)
                       :pins (within pins)})))

(defn assistant-context
  "What the assistant's functions need from a session."
  [{:keys [host chat cloud-file] :as session}]
  {:host host
   :chat chat
   :post! #(common/post! session %)
   :cloud-name (workspace/cloud-name (string/replace cloud-file "\\" "/") config/cloud-file-name)})

(defmethod common/handle-message :view/connect-agent [{:keys [host]} _message]
  (agents/connect-agent! host))

(defmethod common/handle-message :view/agents-reporting [_session {:keys [level]}]
  (settings/save-agents-reporting! level))

(defmethod common/handle-message :view/dismiss-pin [{:keys [host]} {:keys [pin-id]}]
  (agents/dismiss-pin! host pin-id))

(defmethod common/handle-message :view/assistant-ask [session {:keys [text]}]
  (assistant-host/ask! (assistant-context session) text))

(defmethod common/handle-message :view/assistant-stop [session _message]
  (assistant-host/stop! (assistant-context session)))

(defmethod common/handle-message :view/assistant-new [session _message]
  (assistant-host/start-over! (assistant-context session)))

(defmethod common/handle-message :view/assistant-model [{:keys [host]} _message]
  (assistant-host/choose-model! host))

(defmethod common/handle-message :view/focus [{:keys [host root-directory state]} {:keys [editing] :as report}]
  (let [previous (:editing (:focus @state))
        now (js/Date.now)]
    (swap! state assoc :focus (assoc report :at now))
    ;; The caret counts as moved only when the editing caret did, so the
    ;; view's other changes never undo a caret moved since in an editor.
    (when (and editing (not= (dissoc editing :namespace-id) (dissoc previous :namespace-id)))
      (agents/record-caret! host {:file (files/absolute-path root-directory (:path editing))
                                  :line (:line editing)
                                  :column (:column editing)
                                  :anchor-line (:anchor-line editing)
                                  :anchor-column (:anchor-column editing)
                                  :source :cloud
                                  :at now}))))

(defn follow-agents!
  "Sends the view the agents present whenever they change, once it is
  ready. Returns the disposable that stops following them."
  [{:keys [host state] :as session}]
  (add-watch (:agent-summary host) state
             (fn [_key _reference previous current]
               (when (and (not= previous current) (:ready? @state))
                 (common/post! session (protocol/message :host/agents current)))))
  (api/disposable #(remove-watch (:agent-summary host) state)))

(defn follow-presence!
  "Sends the view where agents work whenever that changes, once it is
  ready, gathering changes that come together into one message. Returns
  the disposable that stops following."
  [{:keys [host state] :as session}]
  (let [watch-key [state :presence]
        settle-milliseconds 120]
    (add-watch (:presence-summary host) watch-key
               (fn [_key _reference previous current]
                 (when (and (not= previous current) (:ready? @state))
                   (common/debounce! session :presence settle-milliseconds
                                     #(common/post! session (presence-message session @(:presence-summary host)))))))
    (api/disposable #(remove-watch (:presence-summary host) watch-key))))

(defn follow-assistant!
  "Sends the view the assistant's state again whenever the model chosen
  changes or the agent hub starts or stops serving, once the view is
  ready. Returns the disposable that stops following."
  [{:keys [host state] :as session}]
  (let [watch-key [state :assistant]
        announce! (fn [_key _reference previous current]
                    (when (and (not= previous current) (:ready? @state))
                      (assistant-host/announce! (assistant-context session))))
        serving-watch (fn [key reference previous current]
                        (when (not= (:serving? previous) (:serving? current))
                          (announce! key reference previous current)))]
    (add-watch (:assistant-settings host) watch-key announce!)
    (add-watch (:agent-summary host) watch-key serving-watch)
    (api/disposable (fn []
                      (remove-watch (:assistant-settings host) watch-key)
                      (remove-watch (:agent-summary host) watch-key)))))

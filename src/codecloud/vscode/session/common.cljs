(ns codecloud.vscode.session.common
  "What every part of a session shares, which is posting to its view,
  logging, running work in turn, and the multimethod its message handlers
  extend."
  (:require ["path" :as path]
            [codecloud.io.files :as files]
            [codecloud.io.task :as task]
            [codecloud.protocol :as protocol]
            [codecloud.vscode.api :as api]))

(defn post! [{:keys [panel state]} message]
  (when-not (:disposed? @state)
    (api/post-message! panel (protocol/encode message))))

(defn log! [{:keys [host cloud-file]} text]
  ((:log! host) (str (path/basename cloud-file) ": " text)))

(defn post-status! [session message progress]
  (post! session (protocol/message :host/status {:message message :progress progress})))

(defn post-notice! [session message]
  (post! session (protocol/message :host/notice {:message message})))

(defn debounce!
  "Runs `action` once `milliseconds` pass without another call for the
  same `timer-key`."
  [{:keys [state]} timer-key milliseconds action]
  (js/clearTimeout (get-in @state [:timers timer-key]))
  (swap! state assoc-in [:timers timer-key] (js/setTimeout action milliseconds)))

(defmulti handle-message
  "Methods take the session and a message from the view."
  (fn [_session message] (:type message)))

(defmethod handle-message :default [session message]
  (log! session (str "Ignored a message of type " (:type message))))

(defn text-reader
  "A function resolving a path relative to `root-directory` to its text,
  the unsaved text VS Code holds, or else the file on disk, or nil."
  [{:keys [root-directory]}]
  (fn [relative-path]
    (let [absolute (files/absolute-path root-directory relative-path)]
      (if-let [unsaved (get (api/unsaved-texts) absolute)]
        (task/resolved unsaved)
        (files/read-text absolute)))))

(defn in-turn!
  "Runs `action`, a function returning a task, once every edit before it
  finished, so edits reach the document in the order they were made."
  [{:keys [state]} action]
  (swap! state update :edit-turn
         (fn [previous] (-> (or previous (task/resolved nil))
                            (task/then (fn [_] (action)))
                            (task/recover (fn [_error] nil))))))

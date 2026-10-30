(ns codecloud.render.vscode-main
  "Entry point of the cloud view inside a VS Code webview."
  (:require [cljs.reader :as reader]
            [codecloud.protocol :as protocol]
            [codecloud.render.app :as app]))

(defn- saved-view-state
  "Camera placement the webview stored before VS Code hid or restored it."
  [^js api]
  (try
    (when-let [saved (.getState api)]
      (let [view-state (reader/read-string saved)]
        (when (map? view-state) view-state)))
    (catch :default _error nil)))

(defn- vscode-transport []
  (let [^js api (js/acquireVsCodeApi)]
    {:send! (fn [message] (.postMessage api (protocol/encode message)))
     :subscribe! (fn [handler]
                   (.addEventListener js/window "message"
                                      (fn [^js event]
                                        (when-let [message (protocol/decode (.-data event))]
                                          (handler message)))))
     :view-state (saved-view-state api)
     :save-view-state! (fn [view-state] (.setState api (pr-str view-state)))}))

(defn init! []
  (app/start! (js/document.getElementById "codecloud") (vscode-transport)))

(init!)

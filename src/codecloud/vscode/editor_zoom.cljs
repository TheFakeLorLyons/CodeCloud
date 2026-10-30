(ns codecloud.vscode.editor-zoom
  "Zooming out of the editor into the cloud. The zoom keys step the font
  zoom; past the threshold the font resets and the cloud opens on the
  line in view, at the size last shown."
  (:require [codecloud.io.task :as task]
            [codecloud.model.workspace :as workspace]
            [codecloud.presentation.zoom :as zoom]
            [codecloud.vscode.api :as api]
            [codecloud.vscode.clouds :as clouds]
            [codecloud.vscode.settings :as settings]))

(defn- set-steps! [host steps]
  (reset! (:zoom-steps host) steps)
  (api/set-context! "codecloud.zoomedOut" (pos? steps)))

(defn- restore-font-zoom!
  "Undoes the font zoom-outs CodeCloud made, one step at a time."
  [host]
  (let [steps @(:zoom-steps host)]
    (set-steps! host 0)
    (reduce (fn [chain _] (task/then chain (fn [_] (api/execute-command "editor.action.fontZoomIn"))))
            (task/resolved nil)
            (range steps))))

(defn- arrival-for [{:keys [path cursor-line first-visible-line last-visible-line]} text-scale]
  {:file-path path
   :line (workspace/focus-line cursor-line first-visible-line last-visible-line)
   :text-scale text-scale})

(defn enter-cloud!
  "Opens the cloud covering the active file at the line in view, or offers
  to create one; without an editor, the workspace's cloud."
  [host text-scale]
  (let [editor (api/active-editor-view)]
    (clouds/after-first-search
     (:index host)
     (fn []
       (if (and editor (:on-disk? editor))
         (let [arrival (arrival-for editor text-scale)]
           (if-let [cloud-file (clouds/cloud-file-for (:index host) (:path editor))]
             (clouds/open-cloud! host cloud-file arrival (:view-column editor))
             (clouds/offer-new-cloud! host (:path editor) arrival (:view-column editor))))
         (clouds/choose-cloud! host nil))))))

(defn zoom-out-key
  "The key that zooms out of a source file toward the cloud, as written on
  this platform."
  []
  (let [platform (.-platform js/process)
        mac? (= "darwin" platform)]
    (cond
      (settings/standard-zoom-keys?) (if mac? "Cmd+-" "Ctrl+-")
      mac? "Cmd+Alt+-"
      (= "linux" platform) "Ctrl+Shift+Alt+-"
      :else "Ctrl+Alt+-")))

(defn hint-way-back!
  "Says in the status bar how to zoom back out to the cloud, after the
  cloud opened a file."
  []
  (let [hint-milliseconds 6000]
    (api/status-message! (str "$(cloud) " (zoom-out-key) " zooms back out to the cloud") hint-milliseconds)))

(defn- percent [share]
  (str (js/Math.round (* 100 share)) "%"))

(defn zoom-out!
  "One press of the zoom-out key: shrinks the editor's text a step, or
  past the threshold opens the cloud. Steps count at the press, so a held
  key never outruns the count."
  [host]
  (let [steps @(:zoom-steps host)
        threshold (settings/zoom-out-threshold)
        {:keys [action text-scale]} (zoom/zoom-out-press steps threshold)
        message-milliseconds 2500]
    (if (and (= :shrink action) (api/active-editor-view))
      (do (set-steps! host (inc steps))
          (task/then (api/execute-command "editor.action.fontZoomOut")
                     (fn [_]
                       (api/status-message! (str "$(zoom-out) Code at " (percent text-scale) ", cloud at "
                                                 (percent (zoom/arrival-text-scale threshold)))
                                            message-milliseconds))))
      (task/then (restore-font-zoom! host)
                 (fn [_] (enter-cloud! host text-scale))))))

(defn zoom-in!
  "Undoes one font zoom-out step, or zooms the window in when CodeCloud
  has no step to undo."
  [host]
  (let [steps @(:zoom-steps host)]
    (if (pos? steps)
      (do (set-steps! host (dec steps))
          (api/execute-command "editor.action.fontZoomIn"))
      (api/execute-command "workbench.action.zoomIn"))))

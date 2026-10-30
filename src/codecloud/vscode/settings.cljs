(ns codecloud.vscode.settings
  "CodeCloud's settings in VS Code, read with their defaults and limits."
  (:require [codecloud.collections :as collections]
            [codecloud.presentation.sky :as sky]
            [codecloud.vscode.api :as api]))

(def ^:private section "codecloud")

(defn zoom-out-threshold
  "How far, in percent, the editor zooms out before the cloud opens."
  []
  (collections/clamp 100 200 (api/setting section "zoomOutThreshold" 150)))

(defn standard-zoom-keys?
  "True when the usual zoom keys zoom source files toward the cloud."
  []
  (true? (api/setting section "useStandardZoomKeys" true)))

(defn sky-mode []
  (let [chosen (keyword (api/setting section "sky" "auto"))]
    (if (some #{chosen} sky/mode-order) chosen :auto)))

(defn save-sky-mode! [sky-mode]
  (api/update-setting! section "sky" (name sky-mode)))

(defn editor-colors?
  "True when code on pages takes the editor theme's colors."
  []
  (true? (api/setting section "editorColors" false)))

(defn save-editor-colors! [editor-colors?]
  (api/update-setting! section "editorColors" (boolean editor-colors?)))

(defn history-limit
  "Most commits the timeline offers."
  []
  (collections/clamp 0 400 (api/setting section "historyLimit" 60)))

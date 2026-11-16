(ns codecloud.vscode.settings
  "CodeCloud's settings in VS Code, read with their defaults and limits."
  (:require [clojure.string :as string]
            [codecloud.agents.tools :as tools]
            [codecloud.collections :as collections]
            [codecloud.presentation.environments :as environments]
            [codecloud.presentation.page-commands :as page-commands]
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

(defn spaces
  "The spaces the person describes for the cloud to float in."
  []
  (environments/spaces-from-setting (api/data-setting section "spaces")))

(defn sky-mode
  "The environment chosen for the cloud, a built-in environment's keyword
  or a described space's name."
  [described-spaces]
  (environments/chosen-id (api/setting section "sky" (name environments/default-id)) described-spaces))

(defn save-sky-mode! [sky-mode]
  (api/update-setting! section "sky" (name sky-mode)))

(defn simple-color
  "The color chosen for Simple as #rrggbb, or nil for the editor theme's."
  []
  (environments/chosen-color (api/setting section "simpleColor" "")))

(defn save-simple-color!
  "Keeps `color-hex` as Simple's color, or with nil takes the setting
  away, so Simple follows the editor theme again."
  [color-hex]
  (api/update-setting! section "simpleColor" (or color-hex js/undefined)))

(defn toolbar-colors
  "How the toolbar's icons are colored, :colored with a color each, :plain
  in the text color, or a map of icon names to colors as #rrggbb over the
  colored ones."
  []
  (let [chosen (api/data-setting section "toolbarColors")]
    (cond
      (= "plain" chosen) :plain
      (map? chosen) (into {}
                          (keep (fn [[icon-name color-text]]
                                  (when-let [color-hex (environments/chosen-color color-text)]
                                    [icon-name color-hex])))
                          chosen)
      :else :colored)))

(defn page-commands
  "The commands the codecloud.pageCommands setting adds to every page's
  right-click menu."
  []
  (let [entries (api/data-setting section "pageCommands")]
    (into [] (keep page-commands/from-setting) (when (sequential? entries) entries))))

(defn save-toolbar-colors!
  "Draws the toolbar's icons plain, for :plain, or in their colors,
  taking the setting away so it falls back to them."
  [toolbar-colors]
  (api/update-setting! section "toolbarColors" (if (= :plain toolbar-colors) "plain" js/undefined)))

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

(defn agents-enabled? []
  (true? (api/setting section "agents.enabled" true)))

(defn agents-address
  "The address the agent hub listens on; 127.0.0.1 keeps it to this
  computer."
  []
  (let [address (api/setting section "agents.address" "127.0.0.1")]
    (if (and (string? address) (not (string/blank? address)))
      (string/trim address)
      "127.0.0.1")))

(defn agents-reporting []
  (let [chosen (keyword (api/setting section "agents.reporting" "full"))]
    (if (some #{chosen} tools/reporting-levels) chosen :full)))

(defn save-agents-reporting! [level]
  (when (some #{level} tools/reporting-levels)
    (api/update-setting! section "agents.reporting" (name level))))

(defn agents-wavescope-bands
  "How much of wavescope's view around a line agents get, :fine unless
  codecloud.agents.wavescopeBands asks for :all."
  []
  (let [chosen (keyword (api/setting section "agents.wavescopeBands" "fine"))]
    (if (some #{chosen} tools/wavescope-bands) chosen :fine)))

(defn save-agents-wavescope-bands! [bands]
  (when (some #{bands} tools/wavescope-bands)
    (api/update-setting! section "agents.wavescopeBands" (name bands))))

(defn assistant-cache-lifetime []
  (if (= "1h" (api/setting section "assistant.cacheLifetime" "5m")) "1h" "5m"))

(defn agents-port
  "The port the setting asks the agent hub to take, or 0 for the
  workspace's own."
  []
  (let [port (api/setting section "agents.port" 0)]
    (if (and (integer? port) (<= 0 port 65535)) port 0)))

(ns codecloud.render.hud
  "The overlay, made of panels. A panel is {:id :mount :update}: :mount
  builds its elements once from the dispatch function, and :update keeps
  them current with the state."
  (:require [codecloud.presentation.palette :as palette]
            [codecloud.render.dom :as dom]
            [codecloud.render.hud.arrange :as arrange]
            [codecloud.render.hud.chooser :as chooser]
            [codecloud.render.hud.inspector :as inspector]
            [codecloud.render.hud.legend :as legend]
            [codecloud.render.hud.menu :as menu]
            [codecloud.render.hud.messages :as messages]
            [codecloud.render.hud.pointer :as pointer]
            [codecloud.render.hud.search :as search]
            [codecloud.render.hud.timeline :as timeline]
            [codecloud.render.hud.title :as title]
            [codecloud.render.hud.toolbar :as toolbar]
            [codecloud.render.hud.views :as views]))

(def panels
  "Every overlay panel in drawing order."
  [title/panel toolbar/panel search/panel
   messages/status-panel messages/notices-panel messages/empty-panel
   legend/panel views/panel arrange/panel inspector/panel timeline/panel menu/panel chooser/panel
   pointer/tooltip-panel pointer/dive-panel])

(defn create-hud
  "Mounts every panel into `container`."
  [^js container dispatch]
  (let [^js root (dom/element [:div.hud])
        mounted (into {}
                      (map (fn [{:keys [id mount] :as panel}]
                             (let [instance (mount dispatch)]
                               (.appendChild root (:root instance))
                               [id (assoc instance :update (:update panel))])))
                      panels)]
    (.appendChild container root)
    {:root root :panels mounted}))

(defn update-hud!
  "Brings every panel up to date with the interface state."
  [{:keys [panels]} state]
  (doseq [[_id {:keys [update] :as instance}] panels]
    (update instance state)))

(defn update-pointer-overlays!
  "Moves the tooltip and dive ring with the pointer. Runs every frame."
  [{:keys [panels]} pointer-state]
  (pointer/update-overlays! (:tooltip panels) (:dive panels) pointer-state))

(defn apply-theme!
  "Sets the overlay's colors from the palette for the sky's current theme."
  [{:keys [^js root]} theme-name]
  (let [^js document-element (.-documentElement js/document)
        properties [["--ink" :ink] ["--ink-soft" :ink-soft] ["--cycle" :cycle] ["--cycle-warning" :cycle-warning]
                    ["--parse-error" :parse-error] ["--panel" :panel] ["--panel-edge" :panel-edge]
                    ["--accent" :accent] ["--edge" :edge-strong]]]
    (.setAttribute document-element "data-sky" (name theme-name))
    (doseq [[property color-key] properties]
      (.setProperty (.-style document-element) property (palette/hex-of theme-name color-key)))
    root))

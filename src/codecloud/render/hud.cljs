(ns codecloud.render.hud
  "The overlay, made of panels. A panel is {:id :mount :update}: :mount
  builds its elements once from the dispatch function, and :update keeps
  them current with the state."
  (:require [codecloud.presentation.palette :as palette]
            [codecloud.render.dom :as dom]
            [codecloud.render.hud.arrange :as arrange]
            [codecloud.render.hud.assistant-view :as assistant-view]
            [codecloud.render.hud.chooser :as chooser]
            [codecloud.render.hud.coworkers :as coworkers]
            [codecloud.render.hud.dock :as dock]
            [codecloud.render.hud.inspector :as inspector]
            [codecloud.render.hud.legend :as legend]
            [codecloud.render.hud.menu :as menu]
            [codecloud.render.hud.messages :as messages]
            [codecloud.render.hud.pointer :as pointer]
            [codecloud.render.hud.repl-view :as repl-view]
            [codecloud.render.hud.search :as search]
            [codecloud.render.hud.timeline :as timeline]
            [codecloud.render.hud.title :as title]
            [codecloud.render.hud.toolbar :as toolbar]
            [codecloud.render.hud.views :as views]))

(def panels
  "Every overlay panel in drawing order."
  [title/panel toolbar/panel search/panel
   messages/status-panel messages/notices-panel messages/empty-panel
   legend/panel views/panel arrange/panel coworkers/panel coworkers/following-panel assistant-view/panel inspector/panel
   timeline/panel
   repl-view/panel menu/panel chooser/panel
   pointer/tooltip-panel pointer/dive-panel])

(defn create-hud
  "Mounts every panel into `container`, the side panes into the dock,
  which takes the place of the first of them."
  [^js container dispatch]
  (let [^js root (dom/element [:div.hud])
        side-dock (dock/create-dock dispatch)
        dock-panes (set dock/pane-ids)
        mounted (into {}
                      (map (fn [{:keys [id mount] :as panel}]
                             (let [instance (mount dispatch)]
                               (if (contains? dock-panes id)
                                 (do (when-not (.-parentNode (:root side-dock)) (.appendChild root (:root side-dock)))
                                     (dock/add-pane! side-dock id (:root instance)))
                                 (.appendChild root (:root instance)))
                               [id (assoc instance :update (:update panel))])))
                      panels)]
    (.appendChild container root)
    {:root root :panels mounted :dock side-dock}))

(defn update-hud!
  "Brings every panel up to date with the interface state, then the
  dock holding the side panes."
  [{:keys [panels dock]} state]
  (doseq [[_id {update-panel! :update :as instance}] panels]
    (update-panel! instance state))
  (dock/update-dock! dock state))

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
                    ["--accent" :accent] ["--edge" :edge-strong] ["--problem-error" :problem-error]
                    ["--problem-warning" :problem-warning] ["--problem-information" :problem-information]]]
    (.setAttribute document-element "data-sky" (name theme-name))
    (doseq [[property color-key] properties]
      (.setProperty (.-style document-element) property (palette/hex-of theme-name color-key)))
    root))

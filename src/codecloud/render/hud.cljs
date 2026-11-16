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

(defn- follow-edges!
  "Marks `root` while the pointer is near the top or the bottom edge of
  the view, or over the bars along it, so bars that hide themselves come
  back there, and lets the mark go a moment after the pointer leaves."
  [^js root]
  (let [;; Narrower than the grid's margin, so the pointer reaches the
        ;; headers of the cells along an edge without calling a bar over them.
        reach-pixels 8
        linger-milliseconds 600
        lingering (atom {})
        mark! (fn [class-name near?]
                (let [timer (get @lingering class-name)]
                  (cond
                    near? (do (when timer (js/clearTimeout timer))
                              (swap! lingering dissoc class-name)
                              (dom/toggle-class! root class-name true))
                    (and (nil? timer) (.contains (.-classList root) class-name))
                    (swap! lingering assoc class-name
                           (js/setTimeout (fn []
                                            (swap! lingering dissoc class-name)
                                            (dom/toggle-class! root class-name false))
                                          linger-milliseconds)))))]
    (.addEventListener js/window "pointermove"
                       (fn [^js event]
                         (let [x (.-clientX event)
                               y (.-clientY event)
                               ^js target (.-target event)
                               ;; Over a bar's own box or anything it holds,
                               ;; such as a menu hanging below the toolbar.
                               over? (fn [selector]
                                       (or (and (instance? js/Element target) (some? (.closest target selector)))
                                           (some (fn [^js bar]
                                                   (let [box (.getBoundingClientRect bar)]
                                                     (and (<= (.-left box) x (.-right box)) (<= (.-top box) y (.-bottom box)))))
                                                 (array-seq (.querySelectorAll root selector)))))]
                           (mark! "is-near-top" (or (< y reach-pixels) (over? ".hud-title, .hud-toolbar")))
                           (mark! "is-near-bottom" (or (> y (- (.-innerHeight js/window) reach-pixels))
                                                       (over? ".hud-timeline")))))
                       #js {:passive true})))

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
    (follow-edges! root)
    {:root root :panels mounted :dock side-dock}))

(defn update-hud!
  "Brings every panel up to date with the interface state, then the
  dock holding the side panes, and hides the bars along the edges until
  the pointer reaches them when the viewer chose so."
  [{:keys [^js root panels dock]} state]
  (dom/toggle-class! root "is-auto-hiding" (:auto-hide-bars? state))
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

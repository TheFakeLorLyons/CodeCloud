(ns codecloud.render.presence-view
  "Where agents work, drawn in the cloud every frame of the working tree.
  Each agent's badge sits beside the line it works at and flies there
  when it moves, bands over the lines agents read fade over minutes, the
  lines they changed glow with a mark in the gutter, a chip counts the
  lines changed on each page, and the pins agents set take the camera to
  their line when clicked."
  (:require ["three" :as three]
            [clojure.string :as string]
            [codecloud.presentation.color :as color]
            [codecloud.presentation.page :as page]
            [codecloud.presentation.presence-state :as presence-state]
            [codecloud.render.dom :as dom]
            [codecloud.render.editing-view :as editing-view]
            [codecloud.render.icons :as icons]
            [codecloud.render.pages :as pages]
            [codecloud.render.planes :as planes]))

(defn create-layer
  "The presence's layer, its elements over the labels in `labels-root`
  sending clicks to `dispatch`, with the meshes it draws on pages."
  [^js labels-root dispatch]
  (let [root (dom/element [:div.presence-layer])]
    (.after labels-root root)
    {:root root
     :dispatch dispatch
     :badges (atom {})
     :pins (atom {})
     :chips (atom [])
     :meshes (atom [])
     :index (atom nil)
     :colors (atom {})}))

;; What the layer knows of pages

(defn- page-of-path
  "Map from file path to page id for the shown snapshot, worked out once
  for each snapshot."
  [{:keys [index]} snapshot]
  (let [[indexed-snapshot by-path] @index]
    (if (identical? indexed-snapshot snapshot)
      by-path
      (let [built (presence-state/page-index snapshot)]
        (reset! index [snapshot built])
        built))))

(defn- showing?
  "True when a page is in view, in the cloud or in a grid cell."
  [page-object]
  (and page-object (:on-screen? page-object) (not (:leaving? page-object)) (> (:opacity page-object 0) 0.05)))

(def ^:private cell-room
  "Pixels a mark needs inside a grid cell's edges, [left right], a badge's
  circle centered on its point, a pin starting just right of its point,
  and a chip ending just left of its point."
  {:badge [14 14] :pin [2 32] :chip [80 4]})

(defn- file-line-count [page-object path]
  (some #(when (= path (:path %)) (:line-count %)) (:files (:visual page-object))))

(defn- line-point
  "Screen point of line `line` of `path`, counted from one, at the page's
  left edge or with `right?` its right edge, or nil when the page does
  not hold the file. A line past the file's end stands at its last."
  [page-object path line right?]
  (when-let [line-count (file-line-count page-object path)]
    (when-let [[_ y] (editing-view/line-screen-point page-object path (dec (min (max 1 (or line 1)) (max 1 line-count))) 0)]
      (let [[left-room right-room] (cell-room (if right? :pin :badge))
            point (pages/within-cell page-object
                                     [(if right?
                                        (+ (:screen-x page-object) (:half-width-pixels page-object))
                                        (- (:screen-x page-object) (:half-width-pixels page-object)))
                                      y]
                                     left-room
                                     right-room)]
        (when-not (pages/hidden-at? page-object point)
          point)))))

(defn- color-hex [{:keys [colors]} hue theme-key]
  (let [color-key [:hex hue theme-key]]
    (or (get @colors color-key)
        (let [made (color/oklch->hex (presence-state/agent-color hue theme-key))]
          (swap! colors assoc color-key made)
          made))))

(defn- color-rgb [{:keys [colors]} hue theme-key]
  (let [color-key [:rgb hue theme-key]]
    (or (get @colors color-key)
        (let [made (color/oklch->rgb (presence-state/agent-color hue theme-key))]
          (swap! colors assoc color-key made)
          made))))

;; Bands, glows, and gutter marks

(defn- covered-spans
  "Page-local rectangles covering lines `start` to `end` of `path`, the
  whole file when the lines are unknown, clipped to the file."
  [page-object path start end]
  (when-let [line-count (file-line-count page-object path)]
    (let [{:keys [geometry files]} (:visual page-object)
          last-line (max 1 line-count)
          first-shown (min (max 1 (or start 1)) last-line)
          last-shown (min (max first-shown (or end (if start first-shown last-line))) last-line)]
      (page/line-spans geometry files path first-shown last-shown))))

(defn- marks
  "Every rectangle to draw this frame: {:page-object :x :y :width :height
  :z :rgb :opacity}."
  [layer {:keys [agents]} page-of-path page-layer now theme-key]
  (let [;; Strengths at their freshest: a read's band stays faint, while a
        ;; change glows and marks its gutter strongly at first.
        band-strength 0.2
        glow-strength 0.3
        gutter-strength 0.9
        gutter-width 0.3
        removal-height 0.16
        ;; A change pulses for its first seconds, so the eye finds it.
        pulse-milliseconds 8000
        pulse-period 1200
        page-for (fn [file-path]
                   (let [page-object (pages/page-of page-layer (get page-of-path file-path))]
                     (when (showing? page-object) page-object)))]
    (concat
     (for [{:keys [hue reads]} agents
           {:keys [path start end at]} reads
           :let [page-object (page-for path)
                 strength (presence-state/fade at now presence-state/read-lifetime)]
           :when (and page-object (pos? strength))
           span (covered-spans page-object path start end)]
       (assoc span :page-object page-object :z 0.006 :rgb (color-rgb layer hue theme-key)
                   :opacity (* band-strength strength (:opacity page-object))))
     (for [{:keys [hue changes]} agents
           {:keys [path start end added at]} changes
           :let [page-object (page-for path)
                 strength (presence-state/fade at now presence-state/change-lifetime)
                 age (- now at)
                 pulse (if (< age pulse-milliseconds)
                         (* (- 1 (/ age pulse-milliseconds))
                            (+ 0.5 (* 0.5 (js/Math.sin (/ (* 2 js/Math.PI age) pulse-period)))))
                         0)
                 rgb (color-rgb layer hue theme-key)
                 removal? (and start (= 0 added))]
           :when (and page-object (pos? strength))
           {:keys [x y width height] :as span} (covered-spans page-object path start end)
           mark [(if removal?
                   {:x x :y (+ y (* 0.5 height)) :width width :height removal-height :z 0.008
                    :opacity (* gutter-strength strength)}
                   (assoc span :z 0.007 :opacity (* glow-strength strength (+ 1 (* 0.6 pulse)))))
                 {:x (- x (* 0.5 width) (* 0.5 gutter-width)) :y y :width gutter-width :height height :z 0.008
                  :opacity (* gutter-strength strength)}]]
       (assoc mark :page-object page-object :rgb rgb
                   :opacity (* (:opacity mark) (:opacity page-object)))))))

(defn- draw-marks!
  "Draws each rectangle on its page with a mesh from the pool, hiding the
  meshes left over."
  [layer drawn]
  (let [most-marks 600
        shown (take most-marks drawn)]
    (doseq [[index {:keys [page-object x y z width height opacity] [red green blue] :rgb}] (map-indexed vector shown)]
      (let [^js mesh (planes/pooled-mesh! (:meshes layer) index)
            ^js group (:group page-object)
            ^js material (.-material mesh)]
        (when-not (identical? group (.-parent mesh))
          (when-let [^js parent (.-parent mesh)] (.remove parent mesh))
          (.add group mesh))
        (.set (.-position mesh) x y z)
        (.set (.-scale mesh) (max 0.001 width) (max 0.001 height) 1)
        (.setRGB (.-color material) red green blue three/SRGBColorSpace)
        (set! (.-opacity material) opacity)
        (set! (.-visible mesh) (> opacity 0.003))))
    (doseq [^js mesh (drop (count shown) @(:meshes layer))]
      (set! (.-visible mesh) false))))

;; Badges

(defn- badge-element [{:keys [dispatch]} agent-id]
  (dom/element [:button.agent-badge {:type "button"
                                     :data-agent agent-id
                                     :on-click (fn [^js event]
                                                 (.stopPropagation event)
                                                 (dispatch [:open-coworkers]))}
                [:span.agent-doing]
                [:span.agent-initials]]))

(defn- badge-at! [{:keys [^js root badges] :as layer} agent-id]
  (or (:element (get @badges agent-id))
      (let [^js element (badge-element layer agent-id)]
        (.appendChild root element)
        (swap! badges assoc-in [agent-id :element] element)
        element)))

(defn- badge-title [{agent-name :name :keys [doing intent present? place]}]
  (str agent-name
       (when (false? present?) ", no longer connected")
       (when doing (str ": " doing))
       (when (and intent (not= intent doing)) (str ". Intends " intent))
       (when place (str ". At " (:path place) ", line " (:line place)))
       "."))

(defn- eased [progress]
  (let [clipped (min 1 (max 0 progress))]
    (- 1 (* (- 1 clipped) (- 1 clipped) (- 1 clipped)))))

(defn- badge-point
  "Where agent `agent-id`'s badge is drawn this frame, flying for a moment
  after it moved from its last place to `target`, its new one, each end
  read from where its page is now."
  [{:keys [badges]} agent-id target page-of-path page-layer now]
  (let [flight-milliseconds 700
        known (get @badges agent-id)
        {:keys [previous moved-at]} (if (= target (:shown-target known))
                                      known
                                      (let [moved {:shown-target target :previous (:shown-target known) :moved-at now}]
                                        (swap! badges update agent-id merge moved)
                                        moved))
        point-of (fn [{:keys [path line]}]
                   (let [page-object (pages/page-of page-layer (get page-of-path path))]
                     (when (showing? page-object) (line-point page-object path line false))))
        to-point (point-of target)
        from-point (when previous (point-of previous))
        progress (eased (/ (- now (or moved-at now)) flight-milliseconds))]
    (cond
      (nil? to-point) nil
      (or (nil? from-point) (>= progress 1)) to-point
      :else [(+ (first from-point) (* progress (- (first to-point) (first from-point))))
             (+ (second from-point) (* progress (- (second to-point) (second from-point))))])))

(defn- draw-badges!
  "Places each agent's badge beside the line it works at, agents at one
  place stacked, and hides the badges of agents with no place in view."
  [{:keys [badges] :as layer} {:keys [agents following]} page-of-path page-layer now theme-key]
  (let [stack-pixels 26
        readable-pixels 4
        placed (for [{:keys [id place] :as agent} agents
                     :let [page-object (pages/page-of page-layer (get page-of-path (:path place)))
                           point (when (and place (showing? page-object))
                                   (badge-point layer id place page-of-path page-layer now))]
                     :when point]
                 [agent point page-object])
        stacked (map (fn [[agent point page-object]]
                       (let [below (count (filter (fn [[other other-point]]
                                                    (and (neg? (compare (:id other) (:id agent)))
                                                         (< (js/Math.abs (- (second other-point) (second point))) stack-pixels)
                                                         (< (js/Math.abs (- (first other-point) (first point))) stack-pixels)))
                                                  placed))]
                         [agent [(first point) (+ (second point) (* below stack-pixels))] page-object]))
                     placed)
        placed-ids (into #{} (map (comp :id first)) placed)]
    (doseq [[{:keys [id hue initials doing present?] :as agent} [x y] page-object] stacked]
      (let [^js element (badge-at! layer id)
            style (.-style element)
            title (badge-title agent)]
        (dom/set-text! (dom/find-in element ".agent-initials") initials)
        (dom/set-text! (dom/find-in element ".agent-doing") (or doing ""))
        (when (not= title (.getAttribute element "title"))
          (.setAttribute element "title" title)
          (.setAttribute element "aria-label" title))
        (.setProperty style "--agent" (color-hex layer hue theme-key))
        (dom/toggle-class! element "is-compact" (or (some? (:clip page-object))
                                                     (< (:pixels-per-unit page-object 0) readable-pixels)))
        (dom/toggle-class! element "is-gone" (false? present?))
        (dom/toggle-class! element "is-followed" (= following id))
        (set! (.-transform style) (str "translate(" (js/Math.round x) "px, " (js/Math.round y) "px)"))
        (set! (.-opacity style) (str (:opacity page-object 1)))
        (set! (.-display style) "")))
    (doseq [[agent-id {:keys [^js element]}] @badges
            :when (and element (not (contains? placed-ids agent-id)))]
      (set! (.. element -style -display) "none"))
    (let [present-ids (into #{} (map :id) agents)]
      (doseq [[agent-id {:keys [^js element]}] @badges
              :when (not (contains? present-ids agent-id))]
        (some-> element .remove)
        (swap! badges dissoc agent-id)))))

;; Chips counting the lines changed on a page

(defn- chip-text [{:keys [added removed whole?]}]
  (if (and whole? (zero? added) (zero? removed))
    "changed"
    (str "+" added " −" removed)))

(defn- chip-title [{:keys [agents whole?]}]
  (str "Changed lately by " (if (= 1 (count agents)) (first agents) (str (count agents) " agents: " (string/join ", " agents)))
       (when whole? ", lines unknown for some changes")
       ". CodeCloud credits a change to the agent that last read its file, so the name is a guess."))

(defn- chip-point
  "Where a page's chip stands, at its top right corner, kept inside the
  page's grid cell and below the cell's top as the page scrolls."
  [{:keys [clip] :as page-object}]
  (let [[left-room right-room] (:chip cell-room)
        chip-rise 24
        [x y] (pages/within-cell page-object
                                 [(+ (:screen-x page-object) (:half-width-pixels page-object))
                                  (- (:screen-y page-object) (:half-height-pixels page-object))]
                                 left-room
                                 right-room)]
    [x (if clip (max y (+ (:top clip) chip-rise)) y)]))

(defn- draw-chips!
  "Puts a chip counting lines added and removed at the top right corner
  of each page in view with recent changes."
  [layer presence-state page-of-path page-layer now]
  (let [shown (for [[page-id total] (presence-state/page-changes presence-state page-of-path now)
                    :let [page-object (pages/page-of page-layer page-id)
                          point (when (showing? page-object) (chip-point page-object))]
                    :when (and point (not (pages/hidden-at? page-object point)))]
                [total page-object point])]
    (doseq [[index [total page-object [x y]]] (map-indexed vector shown)]
      (let [^js chip (dom/pooled-element! (:root layer) (:chips layer) index [:span.change-chip])
            style (.-style chip)
            title (chip-title total)]
        (dom/set-text! chip (chip-text total))
        (when (not= title (.getAttribute chip "title"))
          (.setAttribute chip "title" title))
        (set! (.-transform style) (str "translate(" (js/Math.round x) "px, " (js/Math.round y) "px)"))
        (set! (.-opacity style) (str (:opacity page-object 1)))
        (set! (.-display style) "")))
    (doseq [^js chip (drop (count shown) @(:chips layer))]
      (set! (.. chip -style -display) "none"))))

;; Pins

(defn- pin-element [{:keys [dispatch]} pin-id]
  (dom/element [:button.agent-pin {:type "button"
                                   :data-pin pin-id
                                   :on-click (fn [^js event]
                                               (.stopPropagation event)
                                               (dispatch [:show-pin pin-id]))}
                icons/pin
                [:span.agent-pin-note]]))

(defn- draw-pins!
  "Places each pin at the right edge of its line, its note beside it while
  the page reads large enough, and lets go of pins dismissed."
  [{:keys [^js root pins] :as layer} {:keys [agents] pin-list :pins} page-of-path page-layer theme-key]
  (let [readable-pixels 6
        by-id (into {} (map (juxt :id identity)) agents)
        live-ids (into #{} (map :id) pin-list)]
    (doseq [{:keys [id agent-id path line note]} pin-list]
      (let [^js element (or (get @pins id)
                            (let [made (pin-element layer id)]
                              (.appendChild root made)
                              (swap! pins assoc id made)
                              made))
            page-object (pages/page-of page-layer (get page-of-path path))
            point (when (showing? page-object) (line-point page-object path line true))
            {agent-name :name :keys [hue]} (get by-id agent-id)
            title (str (or agent-name "An agent") " pointed here: " note " Click to fly to line " line ".")]
        (if-not point
          (set! (.. element -style -display) "none")
          (let [style (.-style element)]
            (dom/set-text! (dom/find-in element ".agent-pin-note") note)
            (when (not= title (.getAttribute element "title"))
              (.setAttribute element "title" title)
              (.setAttribute element "aria-label" title))
            (.setProperty style "--agent" (color-hex layer (or hue 220) theme-key))
            (dom/toggle-class! element "is-compact" (or (some? (:clip page-object))
                                                         (< (:pixels-per-unit page-object 0) readable-pixels)))
            (set! (.-transform style) (str "translate(" (js/Math.round (first point)) "px, " (js/Math.round (second point)) "px)"))
            (set! (.-opacity style) (str (:opacity page-object 1)))
            (set! (.-display style) "")))))
    (doseq [[pin-id ^js element] @pins
            :when (not (contains? live-ids pin-id))]
      (.remove element)
      (swap! pins dissoc pin-id))))

(defn update!
  "Draws where agents work in the shown working tree, and nothing while a
  commit shows."
  [layer page-layer {:keys [snapshot] presence-state :presence} theme-key]
  (let [now (js/Date.now)
        working? (= :working-tree (:id snapshot))
        shown (if working? presence-state {:agents [] :pins []})
        page-of-path (if working? (page-of-path layer snapshot) {})]
    (draw-marks! layer (marks layer shown page-of-path page-layer now theme-key))
    (draw-badges! layer shown page-of-path page-layer now theme-key)
    (draw-chips! layer shown page-of-path page-layer now)
    (draw-pins! layer shown page-of-path page-layer theme-key)))

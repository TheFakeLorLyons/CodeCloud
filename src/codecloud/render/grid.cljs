(ns codecloud.render.grid
  "The grid that pins up to four pages or windows to the screen, as VS
  Code shows editor groups. Each cell has a header in the overlay and a
  body below it. A page in a cell is drawn in a pass of its own, clipped
  to the body, at the editor's size and scrolled as an editor scrolls;
  a window shows through the body, which the canvas leaves clear."
  (:require ["three" :as three]
            [codecloud.presentation.palette :as palette]
            [codecloud.presentation.scene :as scene]
            [codecloud.presentation.windows-state :as windows-state]
            [codecloud.render.camera :as camera]
            [codecloud.render.dom :as dom]
            [codecloud.render.icons :as icons]
            [codecloud.render.pages :as pages]))

(def ^:private header-pixels
  "Height of a cell's header, above its body."
  32)

(def ^:private cell-gap
  "Pixels between cells."
  10)

(defn create-grid
  "The overlay holding the cells' frames and headers and the borders
  between them, put first in the HUD's `root` so the panes and menus stay
  above it."
  [^js hud-root dispatch]
  (let [^js root (dom/element [:div.hud-grid])]
    (.insertBefore hud-root root (.-firstChild hud-root))
    {:root root
     :dispatch dispatch
     :shown-cells (atom nil)
     :shown-places (atom nil)
     :placed-cells (atom [])
     :placed-area (atom nil)}))

;; Where the cells fall

(defn- title-bottom
  "Where the title in the top left ends, as the title publishes it."
  []
  (let [published (.getPropertyValue (.-style (.-documentElement js/document)) "--title-bottom")
        parsed (js/parseFloat published)]
    (if (js/isNaN parsed) 64 parsed)))

(defn- available-area
  "The part of the view the grid fills, left of the dock and, unless the
  bars along the edges hide themselves, below the title and above the
  timeline."
  [app {:keys [auto-hide-bars?]}]
  (let [{:keys [height]} (camera/viewport app)
        margin 16
        least-top 64
        timeline-pixels 84
        top (if auto-hide-bars? margin (max least-top (+ (title-bottom) 8)))
        bottom (if auto-hide-bars? (- height margin) (- height timeline-pixels))
        right (- (camera/open-width app) margin)]
    {:left margin :top top :width (max 120 (- right margin)) :height (max 120 (- bottom top))}))

(defn- body-of [{:keys [left top width height]}]
  {:left left :top (+ top header-pixels) :width width :height (max 1 (- height header-pixels))})

(defn- inside? [{:keys [left top width height]} x y]
  (and (<= left x (+ left width)) (<= top y (+ top height))))

(defn cells
  "The cells shown this frame, each {:index :cell :rect :body :layer}."
  [app]
  (:grid-cells @(:frame app)))

(defn cell-at
  "The cell under screen point [x y], or nil."
  [app x y]
  (some #(when (inside? (:rect %) x y) %) (cells app)))

(defn covers?
  "True when the shown grid covers screen point [x y], gaps included."
  [app x y]
  (boolean (when-let [grid-area (:grid-area @(:frame app))]
             (and (seq (cells app)) (inside? grid-area x y)))))

(defn cell-of-page [app namespace-id]
  (some #(when (= {:kind :page :id namespace-id} (:cell %)) %) (cells app)))

(defn cell-of-window [app window-id]
  (some #(when (= {:kind :window :id window-id} (:cell %)) %) (cells app)))

;; The cells' frames

(defn- cell-title [state {:keys [kind id]}]
  (if (= :page kind)
    (if-let [node (get-in state [:visible-snapshot :namespaces id])]
      (:leaf (scene/label-parts node))
      id)
    (windows-state/window-title (windows-state/window-of state id))))

(defn- place-style [{:keys [left top width height]}]
  {:left (str left "px") :top (str top "px") :width (str width "px") :height (str height "px")})

(defn- place! [^js element place]
  (doseq [[style-key style-value] (place-style place)]
    (.setProperty (.-style element) (name style-key) style-value)))

(defn- start-cell-drag!
  "Carries the cell at `index` by its header over the others, marking the
  cell under the pointer, and trades their places when the pointer rises
  there. A press that hardly moves is no drag."
  [{:keys [^js root dispatch placed-cells]} index ^js event]
  (when (and (zero? (.-button event)) (not (.closest (.-target event) "button")))
    (let [frames (vec (array-seq (.querySelectorAll root ".grid-cell")))
          start [(.-clientX event) (.-clientY event)]
          least-pixels 4
          target (atom nil)
          mark! (fn [target-index]
                  (doseq [[frame-index frame] (map-indexed vector frames)]
                    (dom/toggle-class! frame "is-drop-target" (and (= frame-index target-index) (not= frame-index index)))))]
      (dom/toggle-class! (get frames index) "is-carried" true)
      (dom/follow-drag! event
                        (fn [dx dy]
                          (when (> (js/Math.hypot dx dy) least-pixels)
                            (let [[x y] (mapv + start [dx dy])]
                              (reset! target (some #(when (inside? (:rect %) x y) (:index %)) @placed-cells))
                              (mark! @target))))
                        (fn []
                          (mark! nil)
                          (dom/toggle-class! (get frames index) "is-carried" false)
                          (when (some? @target)
                            (dispatch [:move-cell index @target])))))))

(defn- start-border-drag!
  "Moves the border on `axis` with the pointer, then keeps where it
  stands."
  [{:keys [dispatch placed-area]} axis ^js event]
  (let [start [(.-clientX event) (.-clientY event)]
        share (atom nil)]
    (dom/follow-drag! event
                      (fn [dx dy]
                        (reset! share (windows-state/share-at axis @placed-area cell-gap (mapv + start [dx dy])))
                        (dispatch [:preview-grid-splits {axis @share}]))
                      (fn []
                        (when (some? @share)
                          (dispatch [:set-grid-splits {axis @share}]))))))

(defn- cell-frame [{:keys [dispatch] :as grid} state {:keys [index cell rect]}]
  (let [{:keys [kind id]} cell
        page-path (when (= :page kind)
                    (:path (first (get-in state [:visible-snapshot :namespaces id :files]))))]
    [:section.grid-cell {:data-kind (name kind) :style (place-style rect)}
     [:header.grid-cell-header {:title "Drag onto another cell to trade places"
                                :on-pointerdown #(start-cell-drag! grid index %)}
      [:span.grid-cell-title (cell-title state cell)]
      (when (= :window kind)
        [:button.grid-cell-button {:title "Reload" :aria-label "Reload" :on-click #(dispatch [:reload-window id])}
         icons/reload])
      (when page-path
        [:button.grid-cell-button {:title "Open in VS Code" :aria-label "Open in VS Code"
                                   :on-click #(dispatch [:dive {:path page-path :line 1}])}
         icons/open-file])
      [:button.grid-cell-button {:title "Back to the cloud" :aria-label "Back to the cloud"
                                 :on-click #(dispatch [:unpin cell])}
       icons/close]]
     [:div.grid-cell-body]]))

(defn- border-element [{:keys [dispatch] :as grid} {:keys [axis] :as border}]
  [:div.grid-border {:class (str "is-" (name axis))
                     :style (place-style border)
                     :title "Drag to resize the cells beside it, or double-click to even them"
                     :on-pointerdown #(start-border-drag! grid axis %)
                     :on-dblclick #(dispatch [:set-grid-splits {axis (get windows-state/even-splits axis)}])}])

(defn layout!
  "Works out where the shown grid's cells and the borders between them
  fall this frame, builds their frames again when the cells change, and
  otherwise moves the frames where the cells now fall."
  [{:keys [^js root shown-cells shown-places placed-cells placed-area] :as grid} app state]
  (let [cells (windows-state/shown-cells state)
        grid-area (available-area app state)
        splits (windows-state/valid-splits (get-in state [:grid :splits]))
        rectangles (windows-state/cell-rectangles (count cells) grid-area cell-gap splits)
        cell-borders (windows-state/borders (count cells) grid-area cell-gap splits)
        placed (mapv (fn [index cell rect]
                       {:index index :cell cell :rect rect :body (body-of rect) :layer (inc index)})
                     (range)
                     cells
                     rectangles)
        titles (mapv #(cell-title state (:cell %)) placed)]
    (swap! (:frame app) assoc :grid-cells placed :grid-area (when (seq placed) grid-area))
    (reset! placed-cells placed)
    (reset! placed-area grid-area)
    (cond
      (not= [cells titles] @shown-cells)
      (do (reset! shown-cells [cells titles])
          (reset! shown-places [rectangles cell-borders])
          (dom/replace-children! root (concat (map #(cell-frame grid state %) placed)
                                              (map #(border-element grid %) cell-borders))))

      (not= [rectangles cell-borders] @shown-places)
      (do (reset! shown-places [rectangles cell-borders])
          (doseq [[^js frame rect] (map vector (array-seq (.querySelectorAll root ".grid-cell")) rectangles)]
            (place! frame rect))
          (doseq [[^js element border] (map vector (array-seq (.querySelectorAll root ".grid-border")) cell-borders)]
            (place! element border))))))

;; Pages in cells

(defn- reading-point
  "World position seen at screen point [x y] as far ahead of the camera as
  code shows at the editor's size."
  [app [screen-x screen-y]]
  (let [{:keys [width height]} (camera/viewport app)
        {:keys [^js right ^js up ^js forward]} (camera/basis app)
        ^js eye (.-position ^js (:camera app))
        reading (camera/reading-pixels app)
        depth (camera/distance-for-pixels-per-unit app reading)
        world-per-pixel (/ 1 reading)
        across (* world-per-pixel (- screen-x (* 0.5 width)))
        rise (* world-per-pixel (- (* 0.5 height) screen-y))]
    [(+ (.-x eye) (* depth (.-x forward)) (* across (.-x right)) (* rise (.-x up)))
     (+ (.-y eye) (* depth (.-y forward)) (* across (.-y right)) (* rise (.-y up)))
     (+ (.-z eye) (* depth (.-z forward)) (* across (.-z right)) (* rise (.-z up)))]))

(defn- clamped-scroll
  "`scroll`, in lines from the page's top, kept between the top and the
  point where the page's end reaches the cell's bottom."
  [app geometry body scroll]
  (let [bottom-margin 3
        most (max 0 (+ bottom-margin (- (:height geometry) (/ (:height body) (camera/reading-pixels app)))))]
    (max 0 (min most (or scroll 0)))))

(defn pinned-pages
  "Where each page in a cell is drawn this frame, {id {:center :clip}},
  its left edge at the cell's left and its top scrolled as the cell is,
  at the editor's size, so a page in a cell reads as code in an editor."
  [app]
  (let [{:keys [visuals grid-scrolls]} @(:frame app)
        reading (camera/reading-pixels app)
        code-margin-pixels 6]
    (into {}
          (keep (fn [{:keys [cell body]}]
                  (when (= :page (:kind cell))
                    (when-let [{:keys [geometry]} (get visuals (:id cell))]
                      (let [scroll (clamped-scroll app geometry body (get grid-scrolls (:id cell)))
                            center-x (+ (:left body) code-margin-pixels (* 0.5 reading (:width geometry)))
                            center-y (+ (:top body) (* reading (- (* 0.5 (:height geometry)) scroll)))]
                        [(:id cell) {:center (reading-point app [center-x center-y]) :clip body}])))))
          (cells app))))

(defn scroll-page!
  "Scrolls the cell holding page `namespace-id` by `pixels`, down when
  positive."
  [app namespace-id pixels]
  (when-let [{:keys [body]} (cell-of-page app namespace-id)]
    (when-let [geometry (get-in @(:frame app) [:visuals namespace-id :geometry])]
      (let [current (get-in @(:frame app) [:grid-scrolls namespace-id] 0)]
        (swap! (:frame app) assoc-in [:grid-scrolls namespace-id]
               (clamped-scroll app geometry body (+ current (/ pixels (camera/reading-pixels app)))))))))

(defn scroll-to-line!
  "Scrolls the cell holding the location's page so its line stands a
  third of the way down the cell. True when a cell holds the page."
  [app {:keys [namespace-id path line]}]
  (boolean
   (when-let [{:keys [body]} (cell-of-page app namespace-id)]
     (when-let [page-object (pages/page-of (:page-layer app) namespace-id)]
       (let [geometry (get-in page-object [:visual :geometry])
             [_ line-y] (pages/line-local-point page-object path line)
             reading (camera/reading-pixels app)
             wanted (- (* 0.5 (:height geometry)) line-y (/ (* 0.33 (:height body)) reading))]
         (swap! (:frame app) assoc-in [:grid-scrolls namespace-id] (clamped-scroll app geometry body wanted))
         true)))))

;; Drawing

(defn assign-layers!
  "Puts every part of each page in a cell on that cell's layer, and every
  other page on the cloud's, so each pass draws only its own pages. The
  caret and marks move between pages, so each frame decides afresh."
  [app]
  (let [layer-of (into {} (keep (fn [{:keys [cell layer]}] (when (= :page (:kind cell)) [(:id cell) layer]))) (cells app))]
    (doseq [page-object (pages/all-pages (:page-layer app))]
      (let [layer (get layer-of (:id page-object) 0)]
        (.traverse ^js (:group page-object) (fn [^js part] (.set (.-layers part) layer)))))))

(defn render-cells!
  "Draws the backdrop over the grid's area, so the cloud shows around the
  grid and not between its cells, then each cell clipped to its body, a
  page's on the panel color and a window's clear so the window below the
  canvas shows."
  [app]
  (when-let [shown (seq (cells app))]
    (let [^js renderer (:renderer app)
          ^js camera (:camera app)
          {backdrop-scene :scene backdrop-camera :camera} (:backdrop app)
          viewport-height (camera/viewport-height app)
          clip-to! (fn [{:keys [left top width height]}]
                     (.setScissor renderer left (- viewport-height top height) width height))
          panel-color (three/Color. (palette/hex-of (:theme-key @(:frame app)) :panel))]
      (.setScissorTest renderer true)
      (clip-to! (:grid-area @(:frame app)))
      (.clear renderer true true false)
      (.render renderer backdrop-scene backdrop-camera)
      (doseq [{:keys [cell body layer]} shown]
        (clip-to! body)
        (if (= :page (:kind cell))
          (do (.setClearColor renderer panel-color 1)
              (.clear renderer true true false)
              (.set (.-layers camera) layer)
              (.render renderer (:scene app) camera))
          (do (.setClearColor renderer 0x000000 0)
              (.clear renderer true true false))))
      (.set (.-layers camera) 0)
      (.setScissorTest renderer false)
      (.setClearColor renderer 0x000000 1))))

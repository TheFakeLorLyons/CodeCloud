(ns codecloud.render.camera
  "Camera geometry: directions, distances for an apparent size, screen
  projection, and the editor line height that sets the reading scale."
  (:require ["three" :as three]
            [codecloud.presentation.zoom :as zoom]
            [codecloud.render.navigation :as navigation]))

(defn viewport [app]
  (:viewport @(:frame app) {:width 1280 :height 800}))

(defn viewport-height [app]
  (:height (viewport app)))

(defn editor-line-pixels
  "Editor line height in CSS pixels from the reported font. VS Code reads
  0 as automatic and values below 8 as multiples of the font size."
  [{:keys [size line-height]}]
  (let [font-size (or size 14)
        automatic-ratio 1.35
        multiple-limit 8]
    (cond
      (or (nil? line-height) (zero? line-height)) (* automatic-ratio font-size)
      (< line-height multiple-limit) (* line-height font-size)
      :else line-height)))

(defn editor-font
  "The editor font with its line height in pixels, as pages draw it."
  [state]
  (let [font (:editor-font state)]
    {:family (:family font)
     :size (or (:size font) 14)
     :line-pixels (editor-line-pixels font)}))

(defn reading-pixels
  "Pixels per line at which code shows at the editor's own size."
  [app]
  (editor-line-pixels (:editor-font @(:state app))))

(defn basis
  "The camera's right, up, and forward directions as Vector3s."
  [app]
  (let [^js camera (:camera app)
        ^js world (.-matrixWorld camera)]
    {:right (.setFromMatrixColumn (three/Vector3.) world 0)
     :up (.setFromMatrixColumn (three/Vector3.) world 1)
     :forward (.getWorldDirection camera (three/Vector3.))}))

(defn vector-of [^js vector]
  [(.-x vector) (.-y vector) (.-z vector)])

(defn depth-along
  "Distance of `point` in front of the camera along `forward`."
  [^js camera ^js forward [x y z]]
  (let [^js camera-position (.-position camera)]
    (+ (* (- x (.-x camera-position)) (.-x forward))
       (* (- y (.-y camera-position)) (.-y forward))
       (* (- z (.-z camera-position)) (.-z forward)))))

(defn screen-point
  "CSS pixel position of a world point."
  [app [x y z]]
  (let [^js projected (.project (three/Vector3. x y z) (:camera app))
        {:keys [width height]} (viewport app)]
    [(* (+ 1 (.-x projected)) 0.5 width) (* (- 1 (.-y projected)) 0.5 height)]))

(defn point-ahead
  "The point `distance` straight ahead of the camera."
  [app distance]
  (let [^js camera (:camera app)
        {:keys [^js forward]} (basis app)]
    (vector-of (.addScaledVector (.clone (.-position camera)) forward distance))))

(defn distance-for-pixels-per-unit [app pixels-per-unit]
  (zoom/distance-for-scale pixels-per-unit navigation/field-of-view (viewport-height app)))

(defn- side-panel-open?
  "True while a panel covers the right side of the view."
  [app]
  (let [{:keys [focused-id lifted-folder arrange-open? views-open?]} @(:state app)]
    (boolean (or focused-id lifted-folder arrange-open? views-open?))))

(defn- open-width
  "Width in CSS pixels of the part of the view that no side panel covers."
  [app]
  (let [{:keys [width]} (viewport app)
        ;; CSS pixels from the right edge that the inspector, the
        ;; arrangement controls, or the views panel covers.
        panel-reach 368
        panel-gap 16]
    (if (side-panel-open? app)
      (max (* 0.5 width) (- width panel-reach panel-gap))
      width)))

(defn fitting-pixels-per-unit
  "Scale at which a `width` by `height` area, centered on the view, fills
  the screen without reaching under a side panel."
  [app width height]
  (let [{viewport-width :width viewport-height :height} (viewport app)
        uncovered-width (open-width app)
        half-open-width (if (< uncovered-width viewport-width)
                          (max (* 0.25 viewport-width) (- uncovered-width (* 0.5 viewport-width)))
                          (* 0.5 viewport-width))
        fill-share 0.84]
    (min (/ (* fill-share viewport-height) height)
         (/ (* fill-share 2 half-open-width) width))))

(defn panel-shift
  "World distance to shift the target right at `distance`, centering the
  view in the part no side panel covers."
  [app distance]
  (let [{:keys [width height]} (viewport app)
        shift-pixels (* 0.5 (- width (open-width app)))
        half-angle-tangent (js/Math.tan (/ (* navigation/field-of-view js/Math.PI) 360))]
    (/ (* shift-pixels 2 distance half-angle-tangent) height)))

(defn fitting-distance
  "Camera distance at which `extents` {:position :width :height} fit the
  uncovered view around `center`, measured across and up the screen."
  [app [center-x center-y center-z] extents]
  (let [{:keys [^js right ^js up ^js forward]} (basis app)
        {:keys [height]} (viewport app)
        title-and-timeline-pixels 190
        open-share (/ (max 200 (- height title-and-timeline-pixels)) height)
        half-angle-tangent (js/Math.tan (/ (* navigation/field-of-view js/Math.PI) 360))
        tangent-vertical (* open-share half-angle-tangent)
        tangent-horizontal (* half-angle-tangent (/ (open-width app) height))
        margin 1.04
        ^js offset (three/Vector3.)]
    (* margin
       (reduce max 1
               (for [{[x y z] :position extent-width :width extent-height :height} extents
                     :let [_ (.set offset (- x center-x) (- y center-y) (- z center-z))
                           toward-camera (- (.dot offset forward))]]
                 (max (+ toward-camera (/ (+ (js/Math.abs (.dot offset right)) (* 0.5 extent-width)) tangent-horizontal))
                      (+ toward-camera (/ (+ (js/Math.abs (.dot offset up)) (* 0.5 extent-height)) tangent-vertical))))))))

(defn pointer-ray
  "Raycaster from the camera through a pointer position in normalized
  device coordinates."
  [app ndc-x ndc-y]
  (let [^js raycaster (three/Raycaster.)]
    (.setFromCamera raycaster (three/Vector2. ndc-x ndc-y) (:camera app))
    raycaster))

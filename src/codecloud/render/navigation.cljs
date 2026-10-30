(ns codecloud.render.navigation
  "Camera controls: drag orbits, right- or shift-drag pans, scroll zooms
  toward the pointer, and framing flies to the cloud or a page. Drags
  keep some momentum and flights ease in and out."
  (:require ["three" :as three]
            [codecloud.collections :as collections]))

(def field-of-view
  "Vertical field of view of the cloud camera, in degrees."
  50)

(def ^:private polar-margin
  "Radians the camera keeps from looking straight down or up, where
  orbiting would flip the view."
  0.08)

(defn- bounded-polar [polar]
  (collections/clamp polar-margin (- js/Math.PI polar-margin) polar))

(defn create-controller [^js camera]
  {:camera camera
   :target (three/Vector3. 0 0 0)
   ;; The camera opens a little above the horizon, looking down -z.
   :spherical (let [opening-distance 160
                    opening-polar 1.32]
                (three/Spherical. opening-distance opening-polar 0.0))
   :momentum (atom {:azimuth 0 :polar 0})
   :flight (atom nil)
   :limits {:min-distance 1.0 :max-distance 1500000}})

(defn distance [{:keys [^js spherical]}]
  (.-radius spherical))

(defn target-position [{:keys [^js target]}]
  [(.-x target) (.-y target) (.-z target)])

(defn cancel-flight! [{:keys [flight]}]
  (reset! flight nil))

(defn orbit!
  "Turns the camera around its target by a pointer movement in pixels."
  [{:keys [^js spherical momentum] :as controller} delta-x delta-y viewport-height]
  (cancel-flight! controller)
  (let [turn-per-pixel (/ (* 2 js/Math.PI) viewport-height)
        azimuth-change (* -1 turn-per-pixel delta-x)
        polar-change (* -1 turn-per-pixel delta-y)]
    (set! (.-theta spherical) (+ (.-theta spherical) azimuth-change))
    (set! (.-phi spherical) (bounded-polar (+ (.-phi spherical) polar-change)))
    (reset! momentum {:azimuth azimuth-change :polar polar-change :dragging? true})))

(defn pan!
  "Slides the camera and its target across the screen plane by a pointer
  movement in pixels, so the point under the pointer follows it."
  [{:keys [^js camera ^js target ^js spherical] :as controller} delta-x delta-y viewport-height]
  (cancel-flight! controller)
  (let [units-per-pixel (/ (* 2 (.-radius spherical) (js/Math.tan (/ (* field-of-view js/Math.PI) 360)))
                           viewport-height)
        ^js right (.setFromMatrixColumn (three/Vector3.) (.-matrix camera) 0)
        ^js up (.setFromMatrixColumn (three/Vector3.) (.-matrix camera) 1)]
    (.addScaledVector target right (* -1 delta-x units-per-pixel))
    (.addScaledVector target up (* delta-y units-per-pixel))))

(defn zoom-about!
  "Scales the camera distance by `factor` about `focus-point`, which stays
  put on screen. Returns the new distance."
  [{:keys [^js target ^js spherical limits] :as controller} [focus-x focus-y focus-z] factor]
  (cancel-flight! controller)
  (let [{:keys [min-distance max-distance]} limits
        current (.-radius spherical)
        bounded-factor (/ (collections/clamp min-distance max-distance (* current factor)) current)
        ^js focus (three/Vector3. focus-x focus-y focus-z)]
    (.addScaledVector target (.sub (.clone focus) target) (- 1 bounded-factor))
    (set! (.-radius spherical) (* current bounded-factor))
    (.-radius spherical)))

(defn retarget-along-view!
  "Moves the point the camera turns around to `distance` straight ahead,
  leaving the camera where it is and looking the same way."
  [{:keys [^js camera ^js target ^js spherical] :as controller} distance]
  (cancel-flight! controller)
  (let [^js forward (.getWorldDirection camera (three/Vector3.))]
    (.copy target (.addScaledVector (.clone (.-position camera)) forward distance))
    (set! (.-radius spherical) distance)))

(defn- ease-in-out [fraction]
  (if (< fraction 0.5)
    (* 4 fraction fraction fraction)
    (- 1 (/ (js/Math.pow (+ (* -2 fraction) 2) 3) 2))))

(defn fly-to!
  "Animates the target to [x y z] and the distance to `to-distance` over
  `duration` seconds, keeping the viewing angle."
  [{:keys [^js target ^js spherical flight momentum]} [x y z] to-distance duration]
  (reset! momentum {:azimuth 0 :polar 0 :dragging? false})
  (reset! flight {:from-target (.clone target)
                  :to-target (three/Vector3. x y z)
                  :from-distance (.-radius spherical)
                  :to-distance to-distance
                  :elapsed 0
                  :duration (max 0.001 duration)}))

(defn jump-to!
  "Places the camera at once, for an arrival from the editor."
  [{:keys [^js target ^js spherical] :as controller} [x y z] to-distance]
  (cancel-flight! controller)
  (.set target x y z)
  (set! (.-radius spherical) to-distance))

(defn flying? [{:keys [flight]}]
  (some? @flight))

(defn update!
  "Advances momentum and flights by `elapsed` seconds and places the camera."
  [{:keys [^js camera ^js target ^js spherical momentum flight]} elapsed]
  (when-let [{:keys [from-target to-target from-distance to-distance duration] :as current-flight} @flight]
    (let [progress (min 1 (/ (+ (:elapsed current-flight) elapsed) duration))
          eased (ease-in-out progress)]
      (.lerpVectors target from-target to-target eased)
      (set! (.-radius spherical) (* from-distance (js/Math.pow (/ to-distance from-distance) eased)))
      (if (>= progress 1)
        (reset! flight nil)
        (swap! flight assoc :elapsed (+ (:elapsed current-flight) elapsed)))))
  (let [{:keys [azimuth polar dragging?]} @momentum
        share-left-after-a-second 0.002]
    (when (and (not dragging?)
               (or (> (js/Math.abs azimuth) 0.00001) (> (js/Math.abs polar) 0.00001)))
      (let [decay (js/Math.pow share-left-after-a-second elapsed)]
        (set! (.-theta spherical) (+ (.-theta spherical) azimuth))
        (set! (.-phi spherical) (bounded-polar (+ (.-phi spherical) polar)))
        (reset! momentum {:azimuth (* azimuth decay) :polar (* polar decay) :dragging? false}))))
  (.makeSafe spherical)
  (.add (.setFromSpherical (.-position camera) spherical) target)
  (.lookAt camera target)
  (.updateMatrixWorld camera))

(defn release-drag!
  "Ends a drag, keeping its last movement as momentum unless the pointer
  had paused before letting go."
  [{:keys [momentum]} paused?]
  (if paused?
    (reset! momentum {:azimuth 0 :polar 0 :dragging? false})
    (swap! momentum assoc :dragging? false)))

(defn focus-point-under
  "World point under a pointer position in device coordinates, on the
  plane through the target facing the camera."
  [{:keys [^js camera ^js target]} pointer-x pointer-y]
  (let [^js raycaster (three/Raycaster.)
        ^js normal (.negate (.getWorldDirection camera (three/Vector3.)))
        ^js plane (.setFromNormalAndCoplanarPoint (three/Plane.) normal target)
        ^js hit (three/Vector3.)]
    (.setFromCamera raycaster (three/Vector2. pointer-x pointer-y) camera)
    (if (.intersectPlane (.-ray raycaster) plane hit)
      [(.-x hit) (.-y hit) (.-z hit)]
      [(.-x target) (.-y target) (.-z target)])))

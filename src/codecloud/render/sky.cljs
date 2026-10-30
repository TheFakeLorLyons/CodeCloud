(ns codecloud.render.sky
  "The sky behind the cloud, a full-screen pass looked up by view
  direction, so it turns with the camera: clouds and a low sun by day,
  stars, moon, and aurora by night. Birds or an airplane cross every
  minute or two unless motion is reduced."
  (:require ["three" :as three]
            [goog.object :as object]
            [codecloud.presentation.color :as color]
            [codecloud.presentation.palette :as palette]
            [codecloud.render.shaders :as shaders]))

(defn- color-uniform [theme-name color-key]
  (let [[red green blue] (color/oklch->rgb (palette/color-of theme-name color-key))]
    #js {:value (three/Vector3. red green blue)}))

(defn- vector-uniform [[x y z]]
  #js {:value (three/Vector3. x y z)})

(defn- float-uniform [value]
  #js {:value value})

(def ^:private trail-share
  "Length of a contrail as a share of the airplane's crossing."
  0.35)

(defn create-sky
  "A scene and camera holding only the sky, rendered before the cloud."
  []
  (let [;; Angular sizes in radians of half of one bird and of an airplane's speck.
        flock-reach 0.009
        plane-reach 0.0016
        ^js material (three/ShaderMaterial.
                      #js {:vertexShader shaders/sky-vertex
                           :fragmentShader shaders/sky-fragment
                           :depthTest false
                           :depthWrite false
                           :uniforms #js {:inverseProjection #js {:value (three/Matrix4.)}
                                          :cameraWorld #js {:value (three/Matrix4.)}
                                          :time (float-uniform 0.0)
                                          :night (float-uniform 0.0)
                                          :dusk (float-uniform 0.0)
                                          :dayZenith (color-uniform :day :sky-zenith)
                                          :dayHorizon (color-uniform :day :sky-horizon)
                                          :duskZenith (color-uniform :dusk :sky-zenith)
                                          :duskHorizon (color-uniform :dusk :sky-horizon)
                                          :nightZenith (color-uniform :night :sky-zenith)
                                          :nightHorizon (color-uniform :night :sky-horizon)
                                          :sunColor (color-uniform :day :sun)
                                          :moonColor (color-uniform :night :moon)
                                          :auroraColor (color-uniform :night :aurora)
                                          :sunDirection (vector-uniform [0 1 0])
                                          :moonDirection (vector-uniform [0 1 0])
                                          :flockCenter (vector-uniform [0 0 -1])
                                          :flockHeading (float-uniform 1.0)
                                          :flockScale (float-uniform flock-reach)
                                          :flockAlpha (float-uniform 0.0)
                                          :flapTime (float-uniform 0.0)
                                          :planeHead (vector-uniform [0 0 -1])
                                          :planeTail (vector-uniform [0 0 -1])
                                          :planeScale (float-uniform plane-reach)
                                          :planeAlpha (float-uniform 0.0)}})
        ^js scene (three/Scene.)
        ^js mesh (three/Mesh. (three/PlaneGeometry. 2 2) material)]
    (set! (.-frustumCulled mesh) false)
    (.add scene mesh)
    {:scene scene
     :camera (three/OrthographicCamera. -1 1 1 -1 0 1)
     :material material
     :visitors (atom {:next-at nil :visit nil})}))

;; Directions in the sky

(defn- normalized [[x y z]]
  (let [length (max 0.000001 (js/Math.hypot x y z))]
    [(/ x length) (/ y length) (/ z length)]))

(defn- blend
  "Direction a share of the way from `from` to `to` along the sky."
  [[from-x from-y from-z] [to-x to-y to-z] share]
  (normalized [(+ from-x (* share (- to-x from-x)))
               (+ from-y (* share (- to-y from-y)))
               (+ from-z (* share (- to-z from-z)))]))

(defn- heading-direction
  "Direction from azimuth and elevation in radians. Azimuth zero points
  down -z, where the camera first looks, and grows to the right."
  [azimuth elevation]
  [(* (js/Math.sin azimuth) (js/Math.cos elevation))
   (js/Math.sin elevation)
   (* -1 (js/Math.cos azimuth) (js/Math.cos elevation))])

(defn- sun-direction
  "The sun sits low to the right of the opening view and sets as the
  night deepens."
  [night]
  (let [azimuth (/ (* 22 js/Math.PI) 180)
        day-elevation 0.07
        night-elevation -0.13]
    (heading-direction azimuth (+ day-elevation (* night (- night-elevation day-elevation))))))

(defn- moon-direction
  "The moon rises to the left of the opening view once the dusk has
  passed, so a twilight sky holds only the setting sun."
  [night]
  (let [azimuth (/ (* -18 js/Math.PI) 180)
        day-elevation -0.15
        night-elevation 0.06
        rising (max 0 (* 2 (- night 0.5)))]
    (heading-direction azimuth (+ day-elevation (* rising (- night-elevation day-elevation))))))

(defn- view-direction
  "Direction from the camera through a point of the screen given in
  normalized device coordinates."
  [^js camera screen-x screen-y]
  (let [^js point (.unproject (three/Vector3. screen-x screen-y 0.5) camera)
        ^js eye (.-position camera)]
    (normalized [(- (.-x point) (.-x eye)) (- (.-y point) (.-y eye)) (- (.-z point) (.-z eye))])))

;; Birds and airplanes

(defn- random-between [low high]
  (+ low (* (js/Math.random) (- high low))))

(defn- next-visit-time
  "When the next visitor crosses, which is soon after the cloud opens and
  every minute or two after that."
  [time first?]
  (if first?
    (+ time (random-between 16 32))
    (+ time (random-between 55 120))))

(defn- new-visit
  "A flock or an airplane crossing the view, of `wanted-kind` when given.
  Birds stay home at night unless sent for."
  [^js camera time night wanted-kind]
  (let [kind (cond
               wanted-kind wanted-kind
               (or (> night 0.5) (< (js/Math.random) 0.45)) :plane
               :else :flock)
        rightward? (< (js/Math.random) 0.5)
        offscreen 1.3
        [start-x end-x] (if rightward? [(- offscreen) offscreen] [offscreen (- offscreen)])
        height (random-between 0.25 0.75)
        drift (random-between -0.15 0.15)
        duration (if (= :flock kind) (random-between 13 19) (random-between 38 56))]
    {:kind kind
     :start (view-direction camera start-x (- height drift))
     :end (view-direction camera end-x (+ height drift))
     :started-at time
     :duration duration
     :ends-at (+ time (* duration (if (= :plane kind) (+ 1 trail-share) 1)))}))

(defn- fade
  "Visibility over a crossing's progress, rising over its first stretch
  and falling over its last."
  [progress]
  (let [edge 0.08]
    (min 1 (/ progress edge) (/ (- 1 progress) edge))))

(defn- flock-uniforms
  "The flock's position and heading along the horizon: 1 toward the
  shader's east, -1 toward its west."
  [{:keys [start end started-at duration]} time night]
  (let [progress (/ (- time started-at) duration)
        center (blend start end progress)
        [center-x _ center-z] center
        [start-x _ start-z] start
        [end-x _ end-z] end
        east-x (- center-z)
        east-z center-x
        heading (if (pos? (+ (* east-x (- end-x start-x)) (* east-z (- end-z start-z)))) 1 -1)]
    {:flock-center center
     :flock-heading heading
     :flock-alpha (* (max 0 (fade progress)) (- 1 night))
     :flap-time (- time started-at)}))

(defn- plane-uniforms [{:keys [start end started-at duration]} time]
  (let [progress (/ (- time started-at) duration)
        head (blend start end (min 1 progress))
        tail (blend start end (max 0 (- progress trail-share)))
        arriving (min 1 (/ progress 0.05))
        dissolving (if (> progress 1) (- 1 (/ (- progress 1) trail-share)) 1)]
    {:plane-head head
     :plane-tail tail
     :plane-alpha (max 0 (min arriving dissolving))}))

(defn- advance-visitors!
  "Starts and ends visits as time passes, returning the visit under way,
  or nil."
  [{:keys [visitors]} ^js camera time night motion]
  (let [{:keys [next-at visit sent-for]} @visitors]
    (cond
      (zero? motion) (do (reset! visitors {:next-at nil :visit nil}) nil)
      (and visit (>= time (:ends-at visit))) (do (reset! visitors {:next-at (next-visit-time time false) :visit nil})
                                                 nil)
      visit visit
      (or sent-for (and next-at (>= time next-at))) (let [started (new-visit camera time night sent-for)]
                                                      (reset! visitors {:next-at nil :visit started})
                                                      started)
      (nil? next-at) (do (swap! visitors assoc :next-at (next-visit-time time true)) nil)
      :else nil)))

(defn send-for!
  "Sends a visitor of `kind`, :flock or :plane, across the sky on the next
  frame, as a way to see one without waiting."
  [{:keys [visitors]} kind]
  (swap! visitors assoc :sent-for kind))

(defn- set-vector! [^js uniforms uniform-name [x y z]]
  (.set (.-value ^js (object/get uniforms uniform-name)) x y z))

(defn- set-float! [^js uniforms uniform-name value]
  (set! (.-value ^js (object/get uniforms uniform-name)) value))

(defn update-sky!
  "Points the sky at the main camera, sets the hour, and moves any bird or
  airplane crossing it."
  [{:keys [^js material] :as sky} ^js camera {:keys [time night dusk motion]}]
  (let [^js uniforms (.-uniforms material)
        visit (advance-visitors! sky camera time night motion)
        {:keys [flock-center flock-heading flock-alpha flap-time]} (when (= :flock (:kind visit))
                                                                       (flock-uniforms visit time night))
        {:keys [plane-head plane-tail plane-alpha]} (when (= :plane (:kind visit))
                                                      (plane-uniforms visit time))]
    (.copy (.-value (.-inverseProjection uniforms)) (.-projectionMatrixInverse camera))
    (.copy (.-value (.-cameraWorld uniforms)) (.-matrixWorld camera))
    (set-float! uniforms "time" time)
    (set-float! uniforms "night" night)
    (set-float! uniforms "dusk" dusk)
    (set-vector! uniforms "sunDirection" (sun-direction night))
    (set-vector! uniforms "moonDirection" (moon-direction night))
    (set-float! uniforms "flockAlpha" (or flock-alpha 0))
    (when flock-center
      (set-vector! uniforms "flockCenter" flock-center)
      (set-float! uniforms "flockHeading" flock-heading)
      (set-float! uniforms "flapTime" flap-time))
    (set-float! uniforms "planeAlpha" (or plane-alpha 0))
    (when plane-head
      (set-vector! uniforms "planeHead" plane-head)
      (set-vector! uniforms "planeTail" plane-tail))))

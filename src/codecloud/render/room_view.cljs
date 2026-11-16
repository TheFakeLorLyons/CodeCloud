(ns codecloud.render.room-view
  "The room backdrop, a space around the eye of one room or a maze of
  rooms, its walls, floor, and ceiling wearing the colors and patterns
  its environment gives, under lamps that flicker now and then."
  (:require ["three" :as three]
            [goog.object :as object]
            [codecloud.presentation.environments :as environments]
            [codecloud.render.navigation :as navigation]
            [codecloud.render.shaders :as shaders]
            [codecloud.render.uniforms :as uniforms]))

(def ^:private pattern-numbers
  "The room shader's number for each pattern."
  (zipmap environments/patterns (range)))

(def ^:private surfaces
  "Each surface's key in an environment's space and its name in the room
  shader."
  {:walls "wall" :floor "floor" :ceiling "ceiling"})

(defn create-room []
  (let [^js shader-uniforms (js/Object.assign
                             (uniforms/view-uniforms)
                             #js {:time (uniforms/float-uniform 0.0)
                                  :motion (uniforms/float-uniform 1.0)
                                  :pixelAngle (uniforms/float-uniform 0.001)
                                  :maze (uniforms/float-uniform 0.0)
                                  :lampColor (uniforms/vector-uniform [1 1 1])
                                  :lampsOn (uniforms/float-uniform 1.0)
                                  :hazeColor (uniforms/vector-uniform [0.5 0.5 0.5])})]
    (doseq [shader-name (vals surfaces)]
      (object/set shader-uniforms (str shader-name "Color") (uniforms/vector-uniform [1 1 1]))
      (object/set shader-uniforms (str shader-name "Accent") (uniforms/vector-uniform [1 1 1]))
      (object/set shader-uniforms (str shader-name "Pattern") (uniforms/float-uniform 0.0))
      (object/set shader-uniforms (str shader-name "Scale") (uniforms/float-uniform 1.0)))
    {:material (three/ShaderMaterial. #js {:vertexShader shaders/backdrop-vertex
                                           :fragmentShader shaders/room-fragment
                                           :depthTest false
                                           :depthWrite false
                                           :uniforms shader-uniforms})
     :shown-space (atom nil)}))

(defn- show-space!
  "Sets the uniforms describing the layout, surfaces, and lamps of
  `space`, and the haze its far rooms fade into."
  [^js shader-uniforms {:keys [layout lamps haze] :as space}]
  (uniforms/set-float! shader-uniforms "maze" (if (= :maze layout) 1.0 0.0))
  (doseq [[surface-key shader-name] surfaces
          :let [{:keys [color accent pattern scale]} (get space surface-key)]]
    (uniforms/set-vector! shader-uniforms (str shader-name "Color") color)
    (uniforms/set-vector! shader-uniforms (str shader-name "Accent") accent)
    (uniforms/set-float! shader-uniforms (str shader-name "Pattern") (get pattern-numbers pattern 0))
    (uniforms/set-float! shader-uniforms (str shader-name "Scale") scale))
  (uniforms/set-float! shader-uniforms "lampsOn" (if lamps 1.0 0.0))
  (uniforms/set-vector! shader-uniforms "lampColor" (or lamps [1 1 1]))
  (uniforms/set-vector! shader-uniforms "hazeColor" haze))

(defn update-room!
  [{:keys [^js material shown-space]} ^js camera {:keys [time motion space viewport-height pixel-ratio]}]
  (let [^js shader-uniforms (.-uniforms material)
        half-angle-tangent (js/Math.tan (/ (* navigation/field-of-view js/Math.PI) 360))]
    (uniforms/look-along! shader-uniforms camera)
    (uniforms/set-float! shader-uniforms "time" time)
    (uniforms/set-float! shader-uniforms "motion" motion)
    ;; How wide one drawn pixel is, as an angle, so patterns finer than a
    ;; pixel even out to their average.
    (uniforms/set-float! shader-uniforms "pixelAngle" (/ (* 2 half-angle-tangent) (max 1 (* viewport-height pixel-ratio))))
    (when (and space (not= space @shown-space))
      (reset! shown-space space)
      (show-space! shader-uniforms space))))

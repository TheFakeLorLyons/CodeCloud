(ns codecloud.render.plain-view
  "The plain backdrop, the sky's colors from horizon to zenith with none of
  its clouds, lights, or visitors, so it costs almost nothing to draw, or
  the colors of a gradient a person chose."
  (:require ["three" :as three]
            [codecloud.render.shaders :as shaders]
            [codecloud.render.uniforms :as uniforms]))

(defn create-plain []
  {:material (three/ShaderMaterial.
              #js {:vertexShader shaders/backdrop-vertex
                   :fragmentShader shaders/plain-fragment
                   :depthTest false
                   :depthWrite false
                   :uniforms (js/Object.assign
                              (uniforms/view-uniforms)
                              #js {:night (uniforms/float-uniform 0.0)
                                   :dayZenith (uniforms/palette-color :day :sky-zenith)
                                   :dayHorizon (uniforms/palette-color :day :sky-horizon)
                                   :nightZenith (uniforms/palette-color :night :sky-zenith)
                                   :nightHorizon (uniforms/palette-color :night :sky-horizon)
                                   :chosen (uniforms/float-uniform 0.0)
                                   :chosenZenith (uniforms/vector-uniform [0 0 0])
                                   :chosenHorizon (uniforms/vector-uniform [0 0 0])})})})

(defn update-plain!
  "Looks along `camera` with this frame's night, in the colors of
  `gradient`, {:zenith :horizon}, when a person chose them."
  [{:keys [^js material]} ^js camera {:keys [night gradient]}]
  (let [^js shader-uniforms (.-uniforms material)]
    (uniforms/look-along! shader-uniforms camera)
    (uniforms/set-float! shader-uniforms "night" night)
    (uniforms/set-float! shader-uniforms "chosen" (if gradient 1.0 0.0))
    (when gradient
      (uniforms/set-vector! shader-uniforms "chosenZenith" (:zenith gradient))
      (uniforms/set-vector! shader-uniforms "chosenHorizon" (:horizon gradient)))))

(ns codecloud.render.plain-view
  "The plain backdrop, the sky's colors from horizon to zenith with none of
  its clouds, lights, or visitors, so it costs almost nothing to draw."
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
                                   :nightHorizon (uniforms/palette-color :night :sky-horizon)})})})

(defn update-plain! [{:keys [^js material]} ^js camera {:keys [night]}]
  (let [^js shader-uniforms (.-uniforms material)]
    (uniforms/look-along! shader-uniforms camera)
    (uniforms/set-float! shader-uniforms "night" night)))

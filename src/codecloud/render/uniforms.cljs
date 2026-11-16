(ns codecloud.render.uniforms
  "Shader uniforms the backdrops share, made from palette colors, color
  components, and numbers, and changed in place each frame."
  (:require ["three" :as three]
            [goog.object :as object]
            [codecloud.presentation.color :as color]
            [codecloud.presentation.palette :as palette]))

(defn palette-color
  "A uniform holding the color the palette gives `color-key` under
  `theme-name`."
  [theme-name color-key]
  (let [[red green blue] (color/oklch->rgb (palette/color-of theme-name color-key))]
    #js {:value (three/Vector3. red green blue)}))

(defn vector-uniform [[x y z]]
  #js {:value (three/Vector3. x y z)})

(defn float-uniform [value]
  #js {:value value})

(defn view-uniforms
  "The uniforms turning a backdrop's screen position into a view direction."
  []
  #js {:inverseProjection #js {:value (three/Matrix4.)}
       :cameraWorld #js {:value (three/Matrix4.)}})

(defn set-vector! [^js uniforms uniform-name [x y z]]
  (.set (.-value ^js (object/get uniforms uniform-name)) x y z))

(defn set-float! [^js uniforms uniform-name value]
  (set! (.-value ^js (object/get uniforms uniform-name)) value))

(defn look-along!
  "Points a backdrop's view uniforms along `camera`."
  [^js uniforms ^js camera]
  (.copy (.-value (.-inverseProjection uniforms)) (.-projectionMatrixInverse camera))
  (.copy (.-value (.-cameraWorld uniforms)) (.-matrixWorld camera)))

(ns codecloud.render.planes
  "Flat geometry the views share: the unit square every flat mesh scales
  to its size, the transparent rectangles drawn over a page's text, kept
  in pools, and buffer attributes rewritten every frame."
  (:require ["three" :as three]))

(defonce unit-plane (three/PlaneGeometry. 1 1))

(defn overlay-mesh
  "A rectangle drawn over the page's text, unseen until given an opacity."
  []
  (let [^js mesh (three/Mesh. unit-plane (three/MeshBasicMaterial. #js {:transparent true :depthTest false
                                                                         :depthWrite false :opacity 0}))]
    (set! (.-renderOrder mesh) 2)
    mesh))

(defn pooled-mesh!
  "The `index`th overlay mesh of `pool`, an atom holding a vector, made
  when the pool holds fewer."
  [pool index]
  (or (get @pool index)
      (let [mesh (overlay-mesh)]
        (swap! pool conj mesh)
        mesh)))

(defn dynamic-attribute
  "A buffer attribute of `attribute-class` holding `item-size` numbers for
  each of `item-count` items, rewritten every frame."
  [attribute-class item-size item-count]
  (let [^js attribute (new attribute-class (js/Float32Array. (* item-size item-count)) item-size)]
    (.setUsage attribute three/DynamicDrawUsage)
    attribute))

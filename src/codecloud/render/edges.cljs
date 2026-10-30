(ns codecloud.render.edges
  "Dependency edges as ribbons of constant screen width on shallow arcs,
  one mesh bent per edge by the vertex shader. Each fades from its page's
  color to its target's as light flows toward the target. Cycle edges are
  red, import loop edges amber, and mutual requires arc apart."
  (:require ["three" :as three]
            [codecloud.presentation.palette :as palette]
            [codecloud.presentation.scene :as scene]
            [codecloud.render.shaders :as shaders]))

(def ^:private segments-per-edge
  "Straight pieces that make up one arc."
  20)

(def ^:private vertices-per-edge (* 2 (inc segments-per-edge)))

(def ^:private per-edge-attributes
  "Attributes that hold one value per edge, by name and size."
  {"edgeStart" 3
   "edgeEnd" 3
   "endRadii" 2
   "startColor" 4
   "endColor" 3
   "edgeWidth" 1
   "flow" 1})

(defn- edge-material []
  (three/ShaderMaterial.
   #js {:vertexShader shaders/edge-vertex
        :fragmentShader shaders/edge-fragment
        :transparent true
        :depthWrite false
        :side three/DoubleSide
        :uniforms #js {:viewport #js {:value (three/Vector2. 1 1)}
                       :cameraRight #js {:value (three/Vector3. 1 0 0)}
                       :cameraUp #js {:value (three/Vector3. 0 1 0)}
                       :time #js {:value 0.0}
                       :motion #js {:value 1.0}}}))

(defn create-layer [^js scene]
  {:scene scene
   :material (edge-material)
   :mesh (atom nil)
   :edges (atom [])
   :topology (atom nil)})

(defn- per-edge-attribute
  "An attribute holding `item-size` numbers for each of `edge-count`
  edges, shared by every vertex of that edge's ribbon."
  [item-size edge-count]
  (let [^js buffer (three/InstancedBufferAttribute. (js/Float32Array. (* item-size edge-count)) item-size)]
    (.setUsage buffer three/DynamicDrawUsage)
    buffer))

(defn- build-geometry
  "One ribbon of `segments-per-edge` segments, drawn once for each edge,
  with each edge's ends and look given per edge."
  [edge-count]
  (let [progress (js/Float32Array. vertices-per-edge)
        sides (js/Float32Array. vertices-per-edge)
        bends (js/Float32Array. vertices-per-edge)
        indices (js/Uint16Array. (* 6 segments-per-edge))
        ^js geometry (three/InstancedBufferGeometry.)
        bend-amount 0.12]
    (dotimes [point-index (inc segments-per-edge)]
      (doseq [[side-index side] [[0 -1] [1 1]]]
        (let [vertex (+ (* 2 point-index) side-index)]
          (aset progress vertex (/ point-index segments-per-edge))
          (aset sides vertex side)
          (aset bends vertex bend-amount))))
    (dotimes [segment segments-per-edge]
      (doseq [[slot offset] (map-indexed vector [0 1 2 1 3 2])]
        (aset indices (+ (* 6 segment) slot) (+ (* 2 segment) offset))))
    (.setAttribute geometry "position" (three/BufferAttribute. (js/Float32Array. (* 3 vertices-per-edge)) 3))
    (.setAttribute geometry "progress" (three/BufferAttribute. progress 1))
    (.setAttribute geometry "side" (three/BufferAttribute. sides 1))
    (.setAttribute geometry "bend" (three/BufferAttribute. bends 1))
    (.setIndex geometry (three/BufferAttribute. indices 1))
    (doseq [[attribute-name item-size] per-edge-attributes]
      (.setAttribute geometry attribute-name (per-edge-attribute item-size edge-count)))
    (set! (.-instanceCount geometry) edge-count)
    geometry))

(defn sync-edges!
  "Takes the edges to draw. The mesh is rebuilt only when the set of edges
  changes; looks are refreshed every frame."
  [{:keys [scene material mesh edges topology]} edge-visuals]
  (let [next-topology (mapv (juxt :from :to) edge-visuals)]
    (reset! edges edge-visuals)
    (when (not= next-topology @topology)
      (reset! topology next-topology)
      (when-let [^js previous @mesh]
        (.remove ^js scene previous)
        (.dispose (.-geometry previous)))
      (if (seq edge-visuals)
        (let [^js edge-mesh (three/Mesh. (build-geometry (count edge-visuals)) material)]
          (set! (.-frustumCulled edge-mesh) false)
          (set! (.-renderOrder edge-mesh) 0)
          (.add ^js scene edge-mesh)
          (reset! mesh edge-mesh))
        (reset! mesh nil)))))

(defn- edge-look
  "Opacity, width in CSS pixels, and flow strength of one edge."
  [{:keys [cyclic? cycle-warning? heavy? role kind emphasis]}]
  (let [quiet? (contains? scene/quiet-edge-kinds kind)]
    {:alpha (* emphasis (cond cyclic? 1.0 cycle-warning? 0.9 role 0.95 heavy? 0.85 quiet? 0.4 :else 0.6))
     :width (cond cyclic? 4.0 heavy? 3.6 cycle-warning? 3.2 role 3.0 quiet? 1.6 :else 2.2)
     :flow (cond cyclic? 1.0 cycle-warning? 0.8 role 0.7 heavy? 0.5 :else 0.15)}))

(defn- write! [^js array index values]
  (let [base (* index (count values))]
    (dotimes [offset (count values)]
      (aset array (+ base offset) (nth values offset)))))

(defn update-edges!
  "Refreshes every edge each frame. `ends-of` gives an id's {:position
  :radius :presence :color} or nil; edges fade with their fainter end and
  take their colors at the ink amount pages follow."
  [{:keys [material mesh edges]} {:keys [ends-of ink ^js camera viewport-width viewport-height pixel-ratio time motion]}]
  (when-let [^js edge-mesh @mesh]
    (let [^js geometry (.-geometry edge-mesh)
          array-of (fn [attribute-name] (.-array (.getAttribute geometry attribute-name)))
          start-array (array-of "edgeStart")
          end-array (array-of "edgeEnd")
          radii-array (array-of "endRadii")
          start-color-array (array-of "startColor")
          end-color-array (array-of "endColor")
          width-array (array-of "edgeWidth")
          flow-array (array-of "flow")
          ^js uniforms (.-uniforms ^js material)
          ^js world (.-matrixWorld camera)
          cycle-color (palette/blended-rgb :cycle ink)
          warning-color (palette/blended-rgb :cycle-warning ink)
          plain-color (palette/blended-rgb :edge ink)
          missing-end {:position [0 0 0] :radius 0 :presence 0 :color plain-color}]
      (doseq [[edge-index {:keys [from to cyclic? cycle-warning?] :as edge}] (map-indexed vector @edges)]
        (let [from-end (ends-of from)
              to-end (ends-of to)
              {:keys [alpha width flow]} (edge-look edge)
              {start-position :position start-radius :radius start-presence :presence start-color :color} (or from-end missing-end)
              {end-position :position end-radius :radius end-presence :presence end-color :color} (or to-end missing-end)
              presence (if (and from-end to-end) (min start-presence end-presence) 0)
              loop-color (cond cyclic? cycle-color cycle-warning? warning-color :else nil)
              [start-red start-green start-blue] (or loop-color start-color plain-color)]
          (write! start-array edge-index start-position)
          (write! end-array edge-index end-position)
          (write! radii-array edge-index [start-radius end-radius])
          (write! start-color-array edge-index [start-red start-green start-blue (* alpha presence)])
          (write! end-color-array edge-index (or loop-color end-color plain-color))
          (aset width-array edge-index (* width pixel-ratio))
          (aset flow-array edge-index flow)))
      (doseq [attribute-name (keys per-edge-attributes)]
        (set! (.-needsUpdate (.getAttribute geometry attribute-name)) true))
      (.set (.-value (.-viewport uniforms)) (* viewport-width pixel-ratio) (* viewport-height pixel-ratio))
      (.setFromMatrixColumn (.-value (.-cameraRight uniforms)) world 0)
      (.setFromMatrixColumn (.-value (.-cameraUp uniforms)) world 1)
      (set! (.-value (.-time uniforms)) time)
      (set! (.-value (.-motion uniforms)) motion))))

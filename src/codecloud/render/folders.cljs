(ns codecloud.render.folders
  "Folder clouds as one mesh of puffs following the pages, lit from above
  and tinted toward each folder's hue. Billows spread over a sphere, so a
  cloud is round from every side."
  (:require ["three" :as three]
            [codecloud.presentation.color :as color]
            [codecloud.presentation.layout :as layout]
            [codecloud.presentation.palette :as palette]
            [codecloud.presentation.sky :as sky]
            [codecloud.render.shaders :as shaders]))

(def ^:private puff-attributes
  "Attributes that hold one value per puff, written to each of its four
  corners, by name and size."
  {"puffCenter" 3
   "position" 3
   "puffSize" 1
   "puffSeed" 1
   "puffAlpha" 1
   "puffLight" 3
   "puffShade" 3
   "cloudFrame" 4})

(defn- folder-material []
  (three/ShaderMaterial.
   #js {:vertexShader shaders/folder-puff-vertex
        :fragmentShader shaders/folder-puff-fragment
        :transparent true
        :depthWrite false
        :depthTest false
        :uniforms #js {:time #js {:value 0.0}
                       :motion #js {:value 1.0}}}))

(defn create-layer [^js scene]
  {:scene scene
   :material (folder-material)
   :mesh (atom nil)
   :capacity (atom 0)
   :tints (atom {})})

(defn- dynamic-attribute [item-size vertex-count]
  (let [^js buffer (three/BufferAttribute. (js/Float32Array. (* item-size vertex-count)) item-size)]
    (.setUsage buffer three/DynamicDrawUsage)
    buffer))

(defn- puff-geometry
  "Room for `puff-capacity` puffs, four corners each."
  [puff-capacity]
  (let [vertex-count (* 4 puff-capacity)
        corners (js/Float32Array. (* 2 vertex-count))
        indices (js/Uint32Array. (* 6 puff-capacity))
        ^js geometry (three/BufferGeometry.)
        corner-offsets [[-1 -1] [1 -1] [1 1] [-1 1]]]
    (dotimes [puff puff-capacity]
      (doseq [[corner-index [corner-x corner-y]] (map-indexed vector corner-offsets)]
        (let [vertex (+ (* 4 puff) corner-index)]
          (aset corners (* 2 vertex) corner-x)
          (aset corners (inc (* 2 vertex)) corner-y)))
      (doseq [[slot corner-index] (map-indexed vector [0 1 2 0 2 3])]
        (aset indices (+ (* 6 puff) slot) (+ (* 4 puff) corner-index))))
    (doseq [[attribute-name item-size] puff-attributes]
      (.setAttribute geometry attribute-name (dynamic-attribute item-size vertex-count)))
    (.setAttribute geometry "corner" (three/BufferAttribute. corners 2))
    (.setIndex geometry (three/BufferAttribute. indices 1))
    geometry))

(defn- seeded [text salt]
  (layout/unit-random (str text ":" salt)))

(def ^:private member-puff-shape
  "Jitter, size, and noise seed of the puff behind one page, the same in
  every frame, so computed once per page."
  (memoize (fn [id]
             {:jitter [(- (seeded id "x") 0.5) (- (seeded id "y") 0.5) (- (seeded id "z") 0.5)]
              :size (+ 1.55 (* 0.3 (seeded id "size")))
              :seed (seeded id "seed")})))

(def ^:private billow-shape
  "Direction, size, and noise seed of one rim billow, once per folder and
  billow. Billows spread evenly over a sphere a little lower than wide,
  each turned by the golden angle from the last, with a jittered rise."
  (memoize (fn [path billow billow-count]
             (let [golden-angle (* js/Math.PI (- 3 (js/Math.sqrt 5)))
                   height-share 0.8
                   rise-jitter 0.3
                   rise (- 1 (/ (* 2 (+ billow 0.5)) billow-count))
                   around (js/Math.sqrt (- 1 (* rise rise)))
                   turn (+ (* golden-angle billow) (* 2 js/Math.PI (seeded path "turn")))
                   jittered-rise (+ rise (* rise-jitter (- (seeded path (str "lift" billow)) 0.5)))]
               {:direction [(* around (js/Math.cos turn))
                            (* height-share jittered-rise)
                            (* around (js/Math.sin turn))]
                :size (+ 0.3 (* 0.14 (seeded path (str "size" billow))))
                :seed (seeded path (str "seed" billow))}))))

(defn folder-extent
  "Center and radius of a sphere around a folder's pages, from members
  given as {:position :radius}."
  [members]
  (let [member-count (count members)
        center (mapv (fn [axis] (/ (reduce + (map #(nth (:position %) axis) members)) member-count)) [0 1 2])
        radius (reduce max 1
                       (map (fn [{:keys [position radius]}]
                              (+ radius (js/Math.hypot (- (nth position 0) (nth center 0))
                                                       (- (nth position 1) (nth center 1))
                                                       (- (nth position 2) (nth center 2)))))
                            members))]
    {:center center :radius radius}))

(defn- folder-puffs
  "Puffs of one folder as {:center :size :seed :alpha}: one in the
  middle, billows around the rim, and one behind each page."
  [path members {:keys [center radius]} alpha]
  (let [presence (reduce max 0 (map :presence members))
        [center-x center-y center-z] center
        member-puffs (map (fn [{:keys [id position presence] page-radius :radius}]
                            (let [{[jitter-x jitter-y jitter-z] :jitter size :size seed :seed} (member-puff-shape id)]
                              {:center [(+ (nth position 0) (* 0.3 page-radius jitter-x))
                                        (+ (nth position 1) (* 0.2 page-radius jitter-y))
                                        (+ (nth position 2) (* 0.3 page-radius jitter-z))]
                               :size (* page-radius size)
                               :seed seed
                               :alpha (* alpha presence)}))
                          members)
        least-billows 9
        most-billows 18
        billow-count (min most-billows (+ least-billows (count members)))
        rim-reach 0.62
        rim-puffs (map (fn [billow]
                         (let [{[direction-x direction-y direction-z] :direction size :size seed :seed}
                               (billow-shape path billow billow-count)
                               reach (* rim-reach radius)]
                           {:center [(+ center-x (* reach direction-x))
                                     (+ center-y (* reach direction-y))
                                     (+ center-z (* reach direction-z))]
                            :size (* radius size)
                            :seed seed
                            :alpha (* 0.85 alpha presence)}))
                       (range billow-count))
        core-puff {:center center
                   :size (* 0.75 radius)
                   :seed (seeded path "core")
                   :alpha (* 0.8 alpha presence)}]
    (concat [core-puff] rim-puffs member-puffs)))

(def ^:private dusk-rgb
  "The warm light and mauve shade sunset lends every folder cloud."
  {:light (color/oklch->rgb (palette/color-of :dusk :cloud))
   :shade (color/oklch->rgb (palette/color-of :dusk :cloud-shade))})

(defn- cloud-colors
  "Lit and shaded colors of a folder's cloud for this moment of the day,
  kept per folder until its hue or tint changes."
  [{:keys [tints]} path hue tint night]
  (let [{remembered :tinted colors :colors} (get @tints path)
        colors (if (and colors (= [hue tint] remembered))
                 colors
                 (let [computed (palette/folder-cloud-rgb hue tint)]
                   (swap! tints assoc path {:tinted [hue tint] :colors computed})
                   computed))]
    (let [dusk (sky/dusk-amount night)
          ;; Sunset lights a cloud warm and shades it mauve, short of
          ;; hiding its folder's color.
          dusk-share 0.4]
      {:light (palette/mix-rgb (palette/mix-rgb (get-in colors [:day :light]) (get-in colors [:night :light]) night)
                               (get dusk-rgb :light)
                               (* dusk-share dusk))
       :shade (palette/mix-rgb (palette/mix-rgb (get-in colors [:day :shade]) (get-in colors [:night :shade]) night)
                               (get dusk-rgb :shade)
                               (* dusk-share dusk))})))

(defn- ensure-capacity!
  "Rebuilds the mesh twice as large when `puff-count` puffs overflow it."
  [{:keys [^js scene ^js material mesh capacity]} puff-count]
  (when (> puff-count @capacity)
    (let [smallest-capacity 64
          new-capacity (max smallest-capacity (* 2 puff-count))]
      (when-let [^js previous @mesh]
        (.remove scene previous)
        (.dispose (.-geometry previous)))
      (let [^js puff-mesh (three/Mesh. (puff-geometry new-capacity) material)]
        (set! (.-frustumCulled puff-mesh) false)
        (set! (.-renderOrder puff-mesh) -1)
        (.add scene puff-mesh)
        (reset! mesh puff-mesh)
        (reset! capacity new-capacity)))))

(defn- folder-clouds
  "Folders with pages to gather, each with its extent and colors."
  [layer folders tint night]
  (into []
        (keep (fn [[path {:keys [members emphasis hue]}]]
                (when (seq members)
                  {:path path
                   :members members
                   :emphasis emphasis
                   :colors (cloud-colors layer path hue tint night)
                   :extent (folder-extent members)})))
        folders))

(defn- cloud-alpha
  "Opacity of the clouds, firmer by day, faded together by `presence`."
  [night presence]
  (let [day-alpha 0.92
        night-alpha 0.6]
    (* (or presence 1) (+ day-alpha (* night (- night-alpha day-alpha))))))

(defn- write-corners!
  "Writes one attribute's `values` into the four corners of a puff."
  [^js array puff-index values]
  (let [item-size (count values)
        first-vertex (* 4 puff-index)]
    (dotimes [corner 4]
      (let [base (* item-size (+ first-vertex corner))]
        (dotimes [slot item-size]
          (aset array (+ base slot) (nth values slot)))))))

(defn- write-puffs!
  "Writes the puffs of every cloud into the mesh's attributes, each cloud
  given as {:puffs :colors :extent}, and draws that many."
  [^js geometry clouds]
  (let [array-of (fn [attribute-name] (.-array (.getAttribute geometry attribute-name)))
        centers (array-of "puffCenter")
        positions (array-of "position")
        sizes (array-of "puffSize")
        seeds (array-of "puffSeed")
        alphas (array-of "puffAlpha")
        lights (array-of "puffLight")
        shades (array-of "puffShade")
        frames (array-of "cloudFrame")
        puff-count (reduce (fn [puff-index {:keys [puffs colors extent]}]
                             (let [{:keys [light shade]} colors
                                   [cloud-x cloud-y cloud-z] (:center extent)
                                   cloud-frame [cloud-x cloud-y cloud-z (:radius extent)]]
                               (reduce (fn [index {:keys [center size seed alpha]}]
                                         (write-corners! centers index center)
                                         (write-corners! positions index center)
                                         (write-corners! sizes index [size])
                                         (write-corners! seeds index [seed])
                                         (write-corners! alphas index [alpha])
                                         (write-corners! lights index light)
                                         (write-corners! shades index shade)
                                         (write-corners! frames index cloud-frame)
                                         (inc index))
                                       puff-index
                                       puffs)))
                           0
                           clouds)]
    (doseq [attribute-name (keys puff-attributes)]
      (set! (.-needsUpdate (.getAttribute geometry attribute-name)) true))
    (.setDrawRange geometry 0 (* 6 puff-count))))

(defn update-folders!
  "Rebuilds every puff from this frame's page positions. `folders` maps
  paths to {:members [{:id :position :radius :presence}] :emphasis :hue},
  and `tint` says how strongly clouds take their hues. Returns each
  folder's {:center :radius}."
  [{:keys [^js material mesh] :as layer} {:keys [folders tint presence night time motion]}]
  (let [base-alpha (cloud-alpha night presence)
        ;; A dimmed folder's cloud stays faintly in view.
        least-emphasis 0.35
        clouds (mapv (fn [{:keys [path members extent emphasis] :as cloud}]
                       (assoc cloud :puffs (vec (folder-puffs path members extent
                                                              (* base-alpha (max least-emphasis emphasis))))))
                     (folder-clouds layer folders tint night))]
    (ensure-capacity! layer (reduce + 0 (map (comp count :puffs) clouds)))
    (when-let [^js puff-mesh @mesh]
      (write-puffs! (.-geometry puff-mesh) clouds)
      (let [^js uniforms (.-uniforms material)]
        (set! (.-value (.-time uniforms)) time)
        (set! (.-value (.-motion uniforms)) motion)))
    (into {} (map (juxt :path :extent)) clouds)))

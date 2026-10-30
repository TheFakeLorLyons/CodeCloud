(ns codecloud.presentation.layout
  "A force simulation placing pages and folder clouds in three dimensions,
  in flat arrays stepped in place. Hashed starts keep a codebase's cloud
  the same, and warm starts let pages drift between commits.")

(defn- to-unsigned-32 [value]
  #?(:clj (bit-and value 0xFFFFFFFF)
     :cljs (unsigned-bit-shift-right value 0)))

(defn- multiply-32 [left right]
  #?(:clj (bit-and (unchecked-multiply (long left) (long right)) 0xFFFFFFFF)
     :cljs (unsigned-bit-shift-right (js/Math.imul left right) 0)))

(defn- character-code [text index]
  #?(:clj (int (.charAt ^String text (int index)))
     :cljs (.charCodeAt text index)))

(defn stable-hash
  "32-bit FNV-1a with the MurmurHash3 finalizer, so names one character
  apart land far apart. The same on every platform."
  [text]
  (let [fnv-offset-basis 2166136261
        fnv-prime 16777619
        murmur-first 0x85ebca6b
        murmur-second 0xc2b2ae35
        fnv (loop [index 0
                   hash-value fnv-offset-basis]
              (if (< index (count text))
                (recur (inc index)
                       (multiply-32 (to-unsigned-32 (bit-xor hash-value (character-code text index)))
                                    fnv-prime))
                hash-value))
        shift-mix (fn [value shift] (to-unsigned-32 (bit-xor value (unsigned-bit-shift-right value shift))))]
    (-> fnv
        (shift-mix 16)
        (multiply-32 murmur-first)
        (shift-mix 13)
        (multiply-32 murmur-second)
        (shift-mix 16))))

(defn unit-random
  "A number in [0, 1) determined by `text`."
  [text]
  (/ (stable-hash text) 4294967296))

(defn- unit-ball-point
  "A point inside the unit ball determined by `seed-text`, spread evenly
  through the volume."
  [seed-text]
  (let [polar (Math/acos (- 1 (* 2 (unit-random (str seed-text "|polar")))))
        azimuth (* 2 Math/PI (unit-random (str seed-text "|azimuth")))
        reach (Math/cbrt (unit-random (str seed-text "|reach")))]
    [(* reach (Math/sin polar) (Math/cos azimuth))
     (* reach (Math/cos polar))
     (* reach (Math/sin polar) (Math/sin azimuth))]))

(def default-tuning
  "Force constants in layout units, median radius 5: neighbors sit about
  half a page apart in a bank about twice as wide as tall."
  {:repulsion 1.0
   :collision 1.5
   :collision-reach 1.1
   :link-strength 0.03
   :link-rest-scale 1.15
   :link-rest-gap 2.0
   :folder-cohesion 0.05
   :parent-cohesion 0.01
   :anchor-containment 0.0
   :anchor-cohesion 0.0
   :anchor-depth-squash 2.0
   :start-height-share 0.5
   :start-depth-share 1.0
   :gravity 0.015
   :vertical-squash 3.0
   :depth-squash 1.7
   :same-folder-repulsion 0.6
   :other-folder-repulsion 1.7
   :velocity-retention 0.62
   :maximum-speed 6.0
   :alpha-decay 0.982
   :alpha-minimum 0.004
   :neighbor-cutoff-node-count 260
   :neighbor-cutoff 45.0})

(defn- number-array
  "Zero-filled array of doubles; a typed array in JavaScript, where the
  simulation's inner loops run."
  [length]
  #?(:clj (double-array length)
     :cljs (js/Float64Array. length)))

(defn- zeroed-index-array
  "Zero-filled array of integers."
  [length]
  #?(:clj (int-array length)
     :cljs (js/Int32Array. length)))

(defn- index-array
  "Array of the integer values in `values`."
  [values]
  #?(:clj (int-array values)
     :cljs (let [result (js/Int32Array. (count values))]
             (doseq [[index value] (map-indexed vector values)]
               (aset result index value))
             result)))

(def ^:private softening
  "Added to squared distances so nodes at the same point never divide by
  zero."
  0.01)

(defn- cold-position
  "Starting point for a node without an earlier position, near its
  folder's hashed point, which sits near its parent folder's. The bank
  starts shorter and shallower than wide by the tuning's start shares."
  [id group parent-group node-count {:keys [start-height-share start-depth-share]}]
  (let [;; Room grows with the square root of the node count, as the area
        ;; of a bank of clouds would.
        spread (+ 20 (* 9 (Math/sqrt node-count)))
        parent-share 0.6
        group-share 0.35
        member-share 0.2
        points [(unit-ball-point (str "group:" parent-group))
                (unit-ball-point (str "group:" group))
                (unit-ball-point id)]
        blended (fn [axis]
                  (+ (* parent-share (nth (nth points 0) axis))
                     (* group-share (nth (nth points 1) axis))
                     (* member-share (nth (nth points 2) axis))))]
    [(* spread (blended 0))
     (* start-height-share spread (blended 1))
     (* start-depth-share spread (blended 2))]))

(defn- anchored-position
  "Starting point for a node held by an anchor, a hashed point inside the
  anchor's sphere, flattened front to back like the sphere itself."
  [id {:keys [center radius]} depth-squash]
  (let [[ball-x ball-y ball-z] (unit-ball-point id)
        [center-x center-y center-z] center
        reach-share 0.6
        reach (* reach-share radius)]
    [(+ center-x (* reach ball-x))
     (+ center-y (* reach ball-y))
     (+ center-z (/ (* reach ball-z) depth-squash))]))

(defn- warm-position
  "Starting point for a node new to this snapshot, beside the neighbors
  that had positions, or nil when none did."
  [id neighbors-of previous-positions radius]
  (let [known (keep previous-positions (neighbors-of id))]
    (when (seq known)
      (let [neighbor-count (count known)
            [jitter-x jitter-y jitter-z] (unit-ball-point (str "jitter:" id))
            jitter-scale (* 3 radius)]
        [(+ (/ (reduce + (map #(nth % 0) known)) neighbor-count) (* jitter-scale jitter-x))
         (+ (/ (reduce + (map #(nth % 1) known)) neighbor-count) (* jitter-scale jitter-y))
         (+ (/ (reduce + (map #(nth % 2) known)) neighbor-count) (* jitter-scale jitter-z))]))))

(defn- layout-unit
  "World size of one layout unit, the median radius over five, so the
  tuning holds whether the nodes are pages or folder clouds."
  [radii]
  (let [sorted-radii (vec (sort radii))
        typical-radius 5.0]
    (if (empty? sorted-radii)
      1.0
      (max 0.000001 (/ (nth sorted-radii (quot (count sorted-radii) 2)) typical-radius)))))

(defn- scaled-anchor [unit {:keys [center radius]}]
  {:center (mapv #(/ % unit) center) :radius (/ radius unit)})

(defn- scaled-positions
  "Positions from world units into layout units."
  [positions unit]
  (into {} (map (fn [[id [x y z]]] [id [(/ x unit) (/ y unit) (/ z unit)]])) positions))

(defn- group-indexes
  "Map from each distinct value of `group-key` among `nodes`, with nil
  read as the empty string, to a dense index."
  [nodes group-key]
  (zipmap (distinct (map #(or (group-key %) "") nodes)) (range)))

(defn- write-triple!
  "Writes `x`, `y`, and `z` into the three slots of `array` for item
  `index`."
  [array index x y z]
  (let [offset (* 3 index)]
    (aset array offset (double x))
    (aset array (+ 1 offset) (double y))
    (aset array (+ 2 offset) (double z))))

(defn- link-arrays
  "Links between distinct known nodes as flat source, target, and weight
  arrays, with each node's neighbors."
  [links index-of]
  (let [valid-links (filterv (fn [{:keys [from to]}]
                               (and (contains? index-of from) (contains? index-of to) (not= from to)))
                             links)
        weights (number-array (count valid-links))]
    (doseq [[index {:keys [weight]}] (map-indexed vector valid-links)]
      (aset weights index (double (or weight 1))))
    {:link-sources (index-array (map #(get index-of (:from %)) valid-links))
     :link-targets (index-array (map #(get index-of (:to %)) valid-links))
     :link-weights weights
     :neighbors (reduce (fn [by-id {:keys [from to]}]
                          (-> by-id
                              (update from (fnil conj []) to)
                              (update to (fnil conj []) from)))
                        {}
                        valid-links)}))

(defn- starting-position
  "Where a node starts: its earlier spot, beside its neighbors when new to
  a warm start, in its anchor, or near its folder's hashed point."
  [{:keys [id group parent-group]} scaled-radius anchor {:keys [previous neighbors warm? tuning node-count]}]
  (or (get previous id)
      (when warm? (warm-position id #(get neighbors % []) previous scaled-radius))
      (when anchor (anchored-position id anchor (:anchor-depth-squash tuning)))
      (cold-position id (or group "") (or parent-group "") node-count tuning)))

(defn- node-arrays
  "Each node's start, radius, group, parent group, and anchor as flat
  arrays indexed like `nodes`. Unanchored nodes have anchor radius -1."
  [nodes unit anchors start]
  (let [node-count (count nodes)
        group-index (group-indexes nodes :group)
        parent-index (group-indexes nodes :parent-group)
        positions (number-array (* 3 node-count))
        radii (number-array node-count)
        group-of (zeroed-index-array node-count)
        parent-of (zeroed-index-array node-count)
        anchor-centers (number-array (* 3 node-count))
        anchor-radii (number-array node-count)]
    (doseq [[index {:keys [id radius group parent-group] :as node}] (map-indexed vector nodes)]
      (let [scaled-radius (/ radius unit)
            anchor (get anchors id)
            [x y z] (starting-position node scaled-radius anchor start)]
        (write-triple! positions index x y z)
        (aset radii index (double scaled-radius))
        (aset group-of index (int (get group-index (or group ""))))
        (aset parent-of index (int (get parent-index (or parent-group ""))))
        (if anchor
          (let [[center-x center-y center-z] (:center anchor)]
            (write-triple! anchor-centers index center-x center-y center-z)
            (aset anchor-radii index (double (:radius anchor))))
          (aset anchor-radii index -1.0))))
    {:positions positions
     :radii radii
     :group-of group-of
     :group-count (count group-index)
     :parent-of parent-of
     :parent-count (count parent-index)
     :anchor-centers anchor-centers
     :anchor-radii anchor-radii}))

(defn create-simulation
  "Simulation for `nodes` {:id :radius :group :parent-group} joined by
  `links` {:from :to :weight}. `previous-positions` start it cooler;
  options hold :tuning, :anchors {id {:center :radius}}, and :alpha."
  ([nodes links previous-positions]
   (create-simulation nodes links previous-positions {}))
  ([nodes links previous-positions {:keys [tuning anchors alpha]}]
   (let [tuning (merge default-tuning tuning)
         ids (mapv :id nodes)
         node-count (count ids)
         unit (layout-unit (map :radius nodes))
         index-of (zipmap ids (range))
         {:keys [neighbors] :as joined} (link-arrays links index-of)
         scaled-anchors (into {} (map (fn [[id anchor]] [id (scaled-anchor unit anchor)])) anchors)
         warm? (boolean (seq previous-positions))]
     (merge (node-arrays nodes unit scaled-anchors {:previous (scaled-positions previous-positions unit)
                                                     :neighbors neighbors
                                                     :warm? warm?
                                                     :tuning tuning
                                                     :node-count node-count})
            (dissoc joined :neighbors)
            {:ids ids
             :unit unit
             :tuning tuning
             :index-of index-of
             :node-count node-count
             :velocities (number-array (* 3 node-count))
             :forces (number-array (* 3 node-count))
             :anchored? (boolean (seq scaled-anchors))
             :alpha (volatile! (or alpha (if warm? 0.35 1.0)))}))))

(defn- add-force! [forces index x y z]
  (let [offset (* 3 index)]
    (aset forces offset (+ (aget forces offset) x))
    (aset forces (+ 1 offset) (+ (aget forces (+ 1 offset)) y))
    (aset forces (+ 2 offset) (+ (aget forces (+ 2 offset)) z))))

(defn- pair-repulsion
  "Repulsion between two nodes, weaker within a group and stronger across
  groups."
  [group-of first-index second-index repulsion same-group-share other-group-share]
  (* repulsion (if (= (aget group-of first-index) (aget group-of second-index))
                 same-group-share
                 other-group-share)))

(defn- repel-pair!
  "Pushes two nodes apart with an inverse-square repulsion scaled by
  their combined radius, and pushes harder while they overlap."
  [positions radii forces first-index second-index repulsion collision collision-reach]
  (let [first-offset (* 3 first-index)
        second-offset (* 3 second-index)
        delta-x (- (aget positions second-offset) (aget positions first-offset))
        delta-y (- (aget positions (+ 1 second-offset)) (aget positions (+ 1 first-offset)))
        delta-z (- (aget positions (+ 2 second-offset)) (aget positions (+ 2 first-offset)))
        distance-squared (+ (* delta-x delta-x) (* delta-y delta-y) (* delta-z delta-z) softening)
        distance (Math/sqrt distance-squared)
        reach (+ (aget radii first-index) (aget radii second-index))
        overlap (- (* reach collision-reach) distance)
        push (/ (+ (/ (* repulsion reach reach) distance-squared)
                   (if (pos? overlap) (* collision overlap) 0))
                distance)
        push-x (* push delta-x)
        push-y (* push delta-y)
        push-z (* push delta-z)]
    (aset forces first-offset (- (aget forces first-offset) push-x))
    (aset forces (+ 1 first-offset) (- (aget forces (+ 1 first-offset)) push-y))
    (aset forces (+ 2 first-offset) (- (aget forces (+ 2 first-offset)) push-z))
    (aset forces second-offset (+ (aget forces second-offset) push-x))
    (aset forces (+ 1 second-offset) (+ (aget forces (+ 1 second-offset)) push-y))
    (aset forces (+ 2 second-offset) (+ (aget forces (+ 2 second-offset)) push-z))))

(defn- repel-all-pairs! [{:keys [positions radii forces node-count group-of tuning]}]
  (let [{:keys [repulsion collision collision-reach same-folder-repulsion other-folder-repulsion]} tuning]
    (dotimes [first-index node-count]
      (loop [second-index (inc first-index)]
        (when (< second-index node-count)
          (repel-pair! positions radii forces first-index second-index
                       (pair-repulsion group-of first-index second-index
                                       repulsion same-folder-repulsion other-folder-repulsion)
                       collision collision-reach)
          (recur (inc second-index)))))))

(defn- grid-cells
  "Buckets nodes into a grid of cubes at least `cutoff` wide by counting
  sort, allocating only flat arrays. Returns the dimensions, each node's
  cell, and each cell's member range."
  [positions node-count cutoff]
  (let [axis-bounds (fn [axis]
                      (loop [index 0 low ##Inf high ##-Inf]
                        (if (< index node-count)
                          (let [value (aget positions (+ axis (* 3 index)))]
                            (recur (inc index) (min low value) (max high value)))
                          [low high])))
        bounds (mapv axis-bounds [0 1 2])
        widest-span (reduce max 1 (map (fn [[low high]] (- high low)) bounds))
        most-cells-per-axis 32
        cell-size (max cutoff (/ widest-span most-cells-per-axis))
        dimensions (mapv (fn [[low high]] (inc (int (Math/floor (/ (- high low) cell-size))))) bounds)
        [width height depth] dimensions
        cell-count (* width height depth)
        cell-of (zeroed-index-array node-count)
        starts (zeroed-index-array (inc cell-count))
        members (zeroed-index-array node-count)
        cursor (zeroed-index-array cell-count)]
    (dotimes [index node-count]
      (let [cell-coordinate (fn [axis]
                              (int (Math/floor (/ (- (aget positions (+ axis (* 3 index))) (first (nth bounds axis)))
                                                  cell-size))))
            cell (+ (* (+ (* (cell-coordinate 0) height) (cell-coordinate 1)) depth) (cell-coordinate 2))]
        (aset cell-of index cell)
        (aset starts (inc cell) (inc (aget starts (inc cell))))))
    (dotimes [cell cell-count]
      (aset starts (inc cell) (+ (aget starts (inc cell)) (aget starts cell))))
    (dotimes [index node-count]
      (let [cell (aget cell-of index)
            slot (+ (aget starts cell) (aget cursor cell))]
        (aset members slot index)
        (aset cursor cell (inc (aget cursor cell)))))
    {:dimensions dimensions :cell-of cell-of :starts starts :members members}))

(defn- repel-neighbors!
  "Repulsion between nodes in neighboring grid cells only, for codebases
  too large for every pair of clouds to meet every step."
  [{:keys [positions radii forces node-count group-of tuning]}]
  (let [{:keys [repulsion collision collision-reach neighbor-cutoff
                same-folder-repulsion other-folder-repulsion]} tuning
        {:keys [dimensions cell-of starts members]} (grid-cells positions node-count neighbor-cutoff)
        [width height depth] dimensions]
    (dotimes [first-index node-count]
      (let [cell (aget cell-of first-index)
            cell-x (quot cell (* height depth))
            cell-y (quot (mod cell (* height depth)) depth)
            cell-z (mod cell depth)]
        (loop [neighbor-x (max 0 (dec cell-x))]
          (when (< neighbor-x (min width (+ cell-x 2)))
            (loop [neighbor-y (max 0 (dec cell-y))]
              (when (< neighbor-y (min height (+ cell-y 2)))
                (loop [neighbor-z (max 0 (dec cell-z))]
                  (when (< neighbor-z (min depth (+ cell-z 2)))
                    (let [neighbor-cell (+ (* (+ (* neighbor-x height) neighbor-y) depth) neighbor-z)
                          last-slot (aget starts (inc neighbor-cell))]
                      (loop [slot (aget starts neighbor-cell)]
                        (when (< slot last-slot)
                          (let [second-index (aget members slot)]
                            (when (> second-index first-index)
                              (repel-pair! positions radii forces first-index second-index
                                           (pair-repulsion group-of first-index second-index
                                                           repulsion same-folder-repulsion other-folder-repulsion)
                                           collision collision-reach)))
                          (recur (inc slot)))))
                    (recur (inc neighbor-z))))
                (recur (inc neighbor-y))))
            (recur (inc neighbor-x))))))))

(defn- pull-links! [{:keys [positions radii forces link-sources link-targets link-weights tuning]}]
  (let [{:keys [link-strength link-rest-scale link-rest-gap]} tuning]
    (dotimes [link-index (alength link-sources)]
      (let [source (aget link-sources link-index)
            target (aget link-targets link-index)
            source-offset (* 3 source)
            target-offset (* 3 target)
            delta-x (- (aget positions target-offset) (aget positions source-offset))
            delta-y (- (aget positions (+ 1 target-offset)) (aget positions (+ 1 source-offset)))
            delta-z (- (aget positions (+ 2 target-offset)) (aget positions (+ 2 source-offset)))
            distance (Math/sqrt (+ (* delta-x delta-x) (* delta-y delta-y) (* delta-z delta-z) softening))
            rest-length (+ link-rest-gap (* link-rest-scale (+ (aget radii source) (aget radii target))))
            pull (/ (* link-strength (aget link-weights link-index) (- distance rest-length)) distance)]
        (add-force! forces source (* pull delta-x) (* pull delta-y) (* pull delta-z))
        (add-force! forces target (* (- pull) delta-x) (* (- pull) delta-y) (* (- pull) delta-z))))))

(defn- gather!
  "Pulls every node toward the center of its group, for groups given as
  an index per node."
  [{:keys [positions forces node-count]} group-of group-count strength]
  (when (pos? strength)
    (let [sums (number-array (* 3 group-count))
          counts (number-array group-count)]
      (dotimes [index node-count]
        (let [group (aget group-of index)
              offset (* 3 index)
              group-offset (* 3 group)]
          (aset counts group (+ 1 (aget counts group)))
          (dotimes [axis 3]
            (aset sums (+ group-offset axis) (+ (aget sums (+ group-offset axis)) (aget positions (+ offset axis)))))))
      (dotimes [index node-count]
        (let [group (aget group-of index)
              member-count (aget counts group)]
          (when (> member-count 1)
            (let [offset (* 3 index)
                  group-offset (* 3 group)]
              (add-force! forces index
                          (* strength (- (/ (aget sums group-offset) member-count) (aget positions offset)))
                          (* strength (- (/ (aget sums (+ 1 group-offset)) member-count) (aget positions (+ 1 offset))))
                          (* strength (- (/ (aget sums (+ 2 group-offset)) member-count) (aget positions (+ 2 offset))))))))))))

(defn- hold-to-anchors!
  "Keeps anchored nodes inside their flattened anchor sphere, pulling
  weakly toward the center and stiffly at the edge."
  [{:keys [positions forces radii node-count anchor-centers anchor-radii tuning]}]
  (let [{:keys [anchor-containment anchor-cohesion anchor-depth-squash]} tuning]
    (dotimes [index node-count]
      (let [anchor-radius (aget anchor-radii index)]
        (when (>= anchor-radius 0)
          (let [offset (* 3 index)
                to-x (- (aget anchor-centers offset) (aget positions offset))
                to-y (- (aget anchor-centers (+ 1 offset)) (aget positions (+ 1 offset)))
                to-z (* anchor-depth-squash (- (aget anchor-centers (+ 2 offset)) (aget positions (+ 2 offset))))
                distance (Math/sqrt (+ (* to-x to-x) (* to-y to-y) (* to-z to-z) softening))
                allowed (max 0 (- anchor-radius (aget radii index)))
                outside (max 0 (- distance allowed))
                pull (+ anchor-cohesion (/ (* anchor-containment outside) distance))]
            (add-force! forces index (* pull to-x) (* pull to-y) (* pull to-z anchor-depth-squash))))))))

(defn- apply-gravity!
  "Pulls every node toward the middle, harder up and down and front to
  back, so the cloud spreads into a wide bank facing the default view."
  [{:keys [positions forces node-count tuning]}]
  (let [{:keys [gravity vertical-squash depth-squash]} tuning]
    (when (pos? gravity)
      (dotimes [index node-count]
        (let [offset (* 3 index)]
          (add-force! forces index
                      (* (- gravity) (aget positions offset))
                      (* (- gravity) vertical-squash (aget positions (+ 1 offset)))
                      (* (- gravity) depth-squash (aget positions (+ 2 offset)))))))))

(defn- integrate! [{:keys [positions velocities forces node-count tuning]} current-alpha]
  (let [{:keys [velocity-retention maximum-speed]} tuning]
    (dotimes [index node-count]
      (let [offset (* 3 index)
            velocity-x (* velocity-retention (+ (aget velocities offset) (* current-alpha (aget forces offset))))
            velocity-y (* velocity-retention (+ (aget velocities (+ 1 offset)) (* current-alpha (aget forces (+ 1 offset)))))
            velocity-z (* velocity-retention (+ (aget velocities (+ 2 offset)) (* current-alpha (aget forces (+ 2 offset)))))
            speed (Math/sqrt (+ (* velocity-x velocity-x) (* velocity-y velocity-y) (* velocity-z velocity-z)))
            speed-scale (if (> speed maximum-speed) (/ maximum-speed speed) 1)]
        (aset velocities offset (* velocity-x speed-scale))
        (aset velocities (+ 1 offset) (* velocity-y speed-scale))
        (aset velocities (+ 2 offset) (* velocity-z speed-scale))
        (aset positions offset (+ (aget positions offset) (aget velocities offset)))
        (aset positions (+ 1 offset) (+ (aget positions (+ 1 offset)) (aget velocities (+ 1 offset))))
        (aset positions (+ 2 offset) (+ (aget positions (+ 2 offset)) (aget velocities (+ 2 offset))))
        (aset forces offset 0.0)
        (aset forces (+ 1 offset) 0.0)
        (aset forces (+ 2 offset) 0.0)))))

(defn moving?
  "True until the simulation has cooled to rest."
  [simulation]
  (> @(:alpha simulation) (:alpha-minimum (:tuning simulation))))

(defn step!
  "Advances the simulation one iteration if it is still moving. Returns the
  simulation."
  [{:keys [tuning] :as simulation}]
  (when (moving? simulation)
    (if (> (:node-count simulation) (:neighbor-cutoff-node-count tuning))
      (repel-neighbors! simulation)
      (repel-all-pairs! simulation))
    (pull-links! simulation)
    (gather! simulation (:group-of simulation) (:group-count simulation) (:folder-cohesion tuning))
    (gather! simulation (:parent-of simulation) (:parent-count simulation) (:parent-cohesion tuning))
    (when (:anchored? simulation)
      (hold-to-anchors! simulation))
    (apply-gravity! simulation)
    (integrate! simulation @(:alpha simulation))
    (vswap! (:alpha simulation) * (:alpha-decay tuning)))
  simulation)

(defn settle!
  "Steps until the simulation rests or `step-limit` iterations ran."
  [simulation step-limit]
  (loop [remaining step-limit]
    (when (and (pos? remaining) (moving? simulation))
      (step! simulation)
      (recur (dec remaining))))
  simulation)

(defn reheat!
  "Wakes a resting simulation, for example after a cloud grew."
  [simulation alpha]
  (vswap! (:alpha simulation) max alpha)
  simulation)

(defn set-radius!
  "Changes the size a node occupies, so neighbors make room when a page
  grows and close in again when it shrinks."
  [simulation id radius]
  (when-let [index (get (:index-of simulation) id)]
    (aset (:radii simulation) index (double (/ radius (:unit simulation)))))
  simulation)

(defn position
  "World [x y z] of node `id`, or nil when the simulation does not hold it."
  [{:keys [index-of positions unit]} id]
  (when-let [index (get index-of id)]
    (let [offset (* 3 index)]
      [(* unit (aget positions offset))
       (* unit (aget positions (+ 1 offset)))
       (* unit (aget positions (+ 2 offset)))])))

(defn positions
  "Map of every node id to its [x y z]."
  [simulation]
  (into {} (map (fn [id] [id (position simulation id)])) (:ids simulation)))

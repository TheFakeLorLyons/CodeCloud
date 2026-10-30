(ns codecloud.render.placement
  "Where every page belongs, from a force simulation or a ranked wall,
  scaled by the spacing about the pivot. The frame holds :placement,
  :spacing, and :targets."
  (:require [codecloud.presentation.arrangement :as arrangement]
            [codecloud.presentation.layout :as layout]
            [codecloud.presentation.scene :as scene]
            [codecloud.render.camera :as camera]
            [codecloud.render.folders :as folders]
            [codecloud.render.navigation :as navigation]))

(def ^:private spacing-tolerance
  "How near the spacing must come to its target to count as there."
  0.0005)

(defn- add [first-vector second-vector] (mapv + first-vector second-vector))

(defn- subtract [from-vector taken-vector] (mapv - from-vector taken-vector))

(defn- scale-by [factor vector-value] (mapv #(* factor %) vector-value))

(defn- average [points]
  (if (seq points)
    (scale-by (/ 1 (count points)) (reduce add [0 0 0] points))
    [0 0 0]))

;; Layout positions

(defn layout-positions
  "Map of page id to [x y z] before spacing, from the simulation or the
  wall."
  [app]
  (let [{:keys [simulation wall-positions]} (:placement @(:frame app))]
    (cond
      simulation (layout/positions simulation)
      wall-positions wall-positions
      :else {})))

(defn- wall-positions
  "World positions of wall slots on the plane through `center` spanned by
  the camera's right and up directions."
  [slots center app]
  (let [{:keys [right up]} (camera/basis app)
        right-vector (camera/vector-of right)
        up-vector (camera/vector-of up)]
    (into {}
          (map (fn [[id [x y]]] [id (add center (add (scale-by x right-vector) (scale-by y up-vector)))]))
          slots)))

(defn- start-placement
  "A fresh placement for `arrangement-key` from the shown pages, starting
  where the pages stand so they travel to their new places."
  [app arrangement-key visuals snapshot visible-ids previous-positions changed-arrangement?]
  (let [edges (scene/layout-links snapshot visible-ids)
        {:keys [kind slots links options folder-plan order]} (arrangement/layout-request arrangement-key snapshot visuals
                                                                                         edges scene/layout-radius)]
    (if (= :slots kind)
      {:arrangement arrangement-key
       :order order
       :wall-positions (wall-positions slots (average (vals previous-positions)) app)}
      (let [restart-alpha 0.9]
        {:arrangement arrangement-key
         :folder-plan folder-plan
         :simulation (layout/create-simulation (scene/layout-nodes visuals)
                                               links
                                               previous-positions
                                               (cond-> options
                                                 changed-arrangement? (assoc :alpha restart-alpha)))}))))

(defn sync!
  "Brings the placement in line with the shown pages, restarting for a new
  arrangement, snapshot, or page set, or resizing changed pages."
  [app {:keys [visuals previous-visuals snapshot visible-ids arrangement-key rebuild?]}]
  (let [{:keys [placement]} @(:frame app)
        changed-arrangement? (not= arrangement-key (:arrangement placement))]
    (if (or rebuild? changed-arrangement? (nil? placement))
      (swap! (:frame app) assoc :placement
             (start-placement app arrangement-key visuals snapshot visible-ids
                              (layout-positions app) (and placement changed-arrangement?)))
      (let [resized (filter (fn [[id visual]]
                              (not= (get-in visual [:geometry :radius]) (get-in previous-visuals [id :geometry :radius])))
                            visuals)]
        (when (seq resized)
          (if-let [simulation (:simulation placement)]
            (let [resize-alpha 0.2]
              (doseq [[id visual] resized]
                (layout/set-radius! simulation id (scene/layout-radius (:geometry visual))))
              (layout/reheat! simulation resize-alpha))
            (swap! (:frame app) assoc :placement
                   (start-placement app arrangement-key visuals snapshot visible-ids (layout-positions app) false))))))))

(defn presettle!
  "Runs the first layout steps before anything is drawn, within a time
  budget, so the cloud opens close to its final shape."
  [app]
  (when-let [simulation (get-in @(:frame app) [:placement :simulation])]
    (let [budget-milliseconds 220
          step-limit 400
          started (js/performance.now)]
      (loop [steps 0]
        (when (and (layout/moving? simulation)
                   (< (- (js/performance.now) started) budget-milliseconds)
                   (< steps step-limit))
          (layout/step! simulation)
          (recur (inc steps)))))))

(defn settled?
  "True once the layout has come to rest."
  [app]
  (if-let [simulation (get-in @(:frame app) [:placement :simulation])]
    (not (layout/moving? simulation))
    true))

(defn ready?
  "True once there is a placement to draw from."
  [app]
  (some? (:placement @(:frame app))))

;; Spacing

(defn- spacing [app]
  (:spacing @(:frame app) {:scale 1.0 :offset [0 0 0]}))

(defn to-world
  "World position of a layout position under the current spacing."
  [app raw-position]
  (let [{:keys [scale offset]} (spacing app)]
    (add (scale-by scale raw-position) offset)))

(defn aim-spacing!
  "Sets the spacing the cloud eases toward, keeping the pivot in place.
  Before the first layout it applies at once."
  [app target-scale]
  (let [{:keys [scale offset] :as current} (spacing app)
        anchor-world (navigation/target-position (:controller app))]
    (swap! (:frame app) assoc :spacing
           (if (ready? app)
             (assoc current
                    :target target-scale
                    :anchor-world anchor-world
                    :anchor-layout (scale-by (/ 1 scale) (subtract anchor-world offset)))
             {:scale target-scale :offset offset :target target-scale}))))

(defn- ease-spacing!
  "Moves the spacing toward its target, holding the anchor still."
  [app elapsed]
  (let [{:keys [scale target anchor-world anchor-layout] :as current} (spacing app)
        rate 5.0]
    (when (and target anchor-layout (> (js/Math.abs (- target scale)) spacing-tolerance))
      (let [eased (+ scale (* (- target scale) (- 1 (js/Math.exp (- (* rate elapsed))))))
            next-scale (if (< (js/Math.abs (- target eased)) spacing-tolerance) target eased)]
        (swap! (:frame app) assoc :spacing
               (assoc current
                      :scale next-scale
                      :offset (subtract anchor-world (scale-by next-scale anchor-layout))))))))

(defn spacing-moving?
  "True while the spacing eases toward a new value."
  [app]
  (let [{:keys [scale target]} (spacing app)]
    (and target (> (js/Math.abs (- target scale)) spacing-tolerance))))

;; Each frame

(defn advance!
  "Steps the layout and the spacing, and works out every page's world
  target again when either moved or the placement changed."
  [app elapsed]
  (let [{:keys [placement]} @(:frame app)
        simulation (:simulation placement)
        layout-moving? (and simulation (layout/moving? simulation))]
    (when layout-moving?
      (let [steps-per-frame 2]
        (dotimes [_ steps-per-frame] (layout/step! simulation))))
    (ease-spacing! app elapsed)
    (let [{:keys [spacing targets-for]} @(:frame app)
          [placed-for spaced-for] targets-for]
      (when (or layout-moving? (not (identical? placement placed-for)) (not (identical? spacing spaced-for)))
        (swap! (:frame app) assoc
               :targets (into {} (map (fn [[id raw-position]] [id (to-world app raw-position)])) (layout-positions app))
               :targets-for [placement spacing])))))

(defn targets
  "Map of page id to where it belongs this frame, in world units."
  [app]
  (:targets @(:frame app) {}))

(defn target-of [app id]
  (get (targets app) id))

(defn extents
  "Every shown page as {:position :width :height} where it belongs."
  [app]
  (let [{:keys [visuals]} @(:frame app)]
    (keep (fn [[id position]]
            (when-let [{:keys [geometry]} (get visuals id)]
              {:position position :width (:width geometry) :height (:height geometry)}))
          (targets app))))

(defn- camera-box
  "Bounds of pages {:position :width :height} along the camera's `right`,
  `up`, and `forward`, as [least-across most-across least-rise most-rise
  nearest farthest]."
  [page-extents ^js right ^js up ^js forward]
  (let [along (fn [^js direction [x y z]]
                (+ (* x (.-x direction)) (* y (.-y direction)) (* z (.-z direction))))]
    (reduce (fn [[least-across most-across least-rise most-rise nearest farthest] {:keys [position width height]}]
              (let [across (along right position)
                    rise (along up position)
                    depth (along forward position)]
                [(min least-across (- across (* 0.5 width))) (max most-across (+ across (* 0.5 width)))
                 (min least-rise (- rise (* 0.5 height))) (max most-rise (+ rise (* 0.5 height)))
                 (min nearest depth) (max farthest depth)]))
            [##Inf ##-Inf ##Inf ##-Inf ##Inf ##-Inf]
            page-extents)))

(defn center
  "Middle of the camera-square box holding every page at home. Framing it
  centers the cloud however its pages are sized, as on a ranked wall."
  [app]
  (let [page-extents (extents app)]
    (if (empty? page-extents)
      [0 0 0]
      (let [{:keys [right up forward]} (camera/basis app)
            [least-across most-across least-rise most-rise nearest farthest] (camera-box page-extents right up forward)]
        (reduce add [0 0 0] [(scale-by (* 0.5 (+ least-across most-across)) (camera/vector-of right))
                             (scale-by (* 0.5 (+ least-rise most-rise)) (camera/vector-of up))
                             (scale-by (* 0.5 (+ nearest farthest)) (camera/vector-of forward))])))))

(defn folder-extent
  "Center and radius of a folder's pages where they belong."
  [app directory]
  (let [{:keys [directory-visuals visuals]} @(:frame app)
        members (keep (fn [id]
                        (when-let [position (target-of app id)]
                          {:position position :radius (scene/layout-radius (get-in visuals [id :geometry]))}))
                      (get-in directory-visuals [directory :member-ids]))]
    (when (seq members)
      (folders/folder-extent members))))

(defn order
  "Ids in a ranked arrangement's order, or nil."
  [app]
  (get-in @(:frame app) [:placement :order]))

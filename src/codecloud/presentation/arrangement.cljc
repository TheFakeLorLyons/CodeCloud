(ns codecloud.presentation.arrangement
  "Ways to arrange the cloud: :folders, a cloud per folder with pages
  leaning toward what they use; :relations, placed by dependencies alone;
  and ranked walls in reading order, largest first."
  (:require [codecloud.model.source :as source]
            [codecloud.presentation.layout :as layout]))

(def arrangements
  "Every arrangement in menu order, with how it sorts when it ranks, and a
  :description where the label alone leaves the placement unclear."
  [{:key :folders
    :label "Folders"
    :description "One cloud per folder, pages leaning toward what they use."}
   {:key :relations
    :label "Relations"
    :description "Grouped by dependencies, regardless of folder."}
   {:key :lines
    :label "Longest files"
    :metric :lines
    :unit ["line" "lines"]}
   {:key :functions
    :label "Most functions"
    :metric :functions
    :unit ["function" "functions"]}
   {:key :references
    :label "Most used"
    :metric :references
    :unit ["use" "uses"]}
   {:key :dependencies
    :label "Most dependencies"
    :metric :dependencies
    :unit ["dependency" "dependencies"]}
   {:key :complection
    :label "Most complected"
    :metric :complection
    :unit ["point" "points"]}])

(def ^:private by-key
  (into {} (map (juxt :key identity)) arrangements))

(defn arrangement
  "The arrangement named `arrangement-key`, falling back to folders."
  [arrangement-key]
  (get by-key arrangement-key (by-key :folders)))

(defn ranked?
  "True for arrangements that lay pages out in order of a measurement."
  [arrangement-key]
  (some? (:metric (arrangement arrangement-key))))

(defn shows-folder-clouds?
  "True when an arrangement keeps each folder's pages together, which
  folder clouds need."
  [arrangement-key]
  (= :folders (:key (arrangement arrangement-key))))

(defn metric-value
  "A namespace's value for a ranking measurement."
  [node metric]
  (get-in node [:metrics metric] 0))

(defn ranked-ids
  "Ids of `ids` ordered by `metric`, largest first, with names breaking
  ties."
  [snapshot ids metric]
  (sort-by (fn [id]
             (let [node (get-in snapshot [:namespaces id])]
               [(- (metric-value node metric)) (:name node)]))
           ids))

(defn- rows-of
  "Splits `ordered-ids` into rows no wider than `row-width`, each row a
  vector of ids, never leaving a row empty."
  [ordered-ids widths row-width gap]
  (let [[rows current _] (reduce (fn [[rows current used] id]
                                   (let [width (get widths id)]
                                     (if (and (seq current) (> (+ used gap width) row-width))
                                       [(conj rows current) [id] width]
                                       [rows (conj current id) (if (seq current) (+ used gap width) width)])))
                                 [[] [] 0]
                                 ordered-ids)]
    (cond-> rows (seq current) (conj current))))

(defn shelf-slots
  "Page centers in reading order on a plane, y up, centered on the origin.
  `sizes` maps id to [width height]; rows wrap so the block is about
  `aspect` times wider than tall."
  [ordered-ids sizes {:keys [aspect gap]}]
  (let [widths (into {} (map (fn [[id [width _]]] [id width])) sizes)
        heights (into {} (map (fn [[id [_ height]]] [id height])) sizes)
        total-area (reduce + 0 (map (fn [id] (* (+ gap (get widths id)) (+ gap (get heights id)))) ordered-ids))
        row-width (max (reduce max 0 (vals widths)) (Math/sqrt (* aspect total-area)))
        rows (rows-of ordered-ids widths row-width gap)
        row-heights (mapv (fn [row] (reduce max 0 (map heights row))) rows)
        block-height (+ (reduce + 0 row-heights) (* gap (max 0 (dec (count rows)))))]
    (loop [remaining-rows rows
           row-index 0
           row-top (/ block-height 2)
           slots {}]
      (if-let [row (first remaining-rows)]
        (let [used-width (+ (reduce + 0 (map widths row)) (* gap (dec (count row))))
              slots (first
                     (reduce (fn [[placed left] id]
                               (let [width (get widths id)]
                                 [(assoc placed id [(+ left (/ width 2)) (- row-top (/ (get heights id) 2))])
                                  (+ left width gap)]))
                             [slots (- (/ used-width 2))]
                             row))]
          (recur (rest remaining-rows)
                 (inc row-index)
                 (- row-top (nth row-heights row-index) gap)
                 slots))
        slots))))

(defn- folder-radius
  "Room a folder cloud needs for pages of `page-radii`, loosely packed,
  never tighter than its largest page."
  [page-radii]
  (let [packing-room 1.2
        largest-share 0.3]
    (+ (* packing-room (Math/sqrt (reduce + 0 (map #(* % %) page-radii))))
       (* largest-share (reduce max 0 page-radii)))))

(defn- folder-links
  "Links between folders: one per dependent pair, weighted by count, and
  lighter ones keeping siblings and parents close."
  [folder-of-id edges folders]
  (let [dependency-pairs (frequencies
                          (keep (fn [{:keys [from to]}]
                                  (let [from-folder (folder-of-id from)
                                        to-folder (folder-of-id to)]
                                    (when (and from-folder to-folder (not= from-folder to-folder))
                                      (sort [from-folder to-folder]))))
                                edges))
        folder-set (set folders)
        family-pairs (for [folder folders
                           other folders
                           :when (and (neg? (compare folder other))
                                      (or (= (source/directory-of folder) (source/directory-of other))
                                          (= folder (source/directory-of other))
                                          (= other (source/directory-of folder))))]
                       [folder other])
        sibling-weight 0.6
        dependency-weight-cap 4]
    (concat (map (fn [[[from to] edge-count]]
                   {:from from :to to :weight (min dependency-weight-cap (Math/sqrt edge-count))})
                 dependency-pairs)
            (keep (fn [[from to]]
                    (when (and (contains? folder-set from) (contains? folder-set to))
                      {:from from :to to :weight sibling-weight}))
                  family-pairs))))

(defn folder-plan
  "Folder path to {:center :radius} in world units, from the shown pages'
  `visuals` and the dependency `edges`."
  [visuals edges radius-of]
  (let [members-by-folder (group-by (comp :directory val) visuals)
        folders (sort (keys members-by-folder))
        radii (into {}
                    (map (fn [[folder members]]
                           [folder (folder-radius (map (comp radius-of :geometry val) members))]))
                    members-by-folder)
        folder-of-id (fn [id] (get-in visuals [id :directory]))
        ;; Firm collisions keep folder clouds apart. The bank first settles
        ;; flat and wide under gravity squashed up and down and front to
        ;; back, then rounds out under a gentler squash, keeping the
        ;; side-by-side order the first stage found.
        flat-steps 450
        rounding-steps 400
        rounding-alpha 0.35
        folder-tuning {:collision 2.0
                       :collision-reach 1.45
                       :repulsion 0.5
                       :link-strength 0.02
                       :folder-cohesion 0.0
                       :parent-cohesion 0.0
                       :gravity 0.02
                       :vertical-squash 3.0
                       :depth-squash 5.0
                       :start-height-share 0.05
                       :start-depth-share 0.3
                       :same-folder-repulsion 1.0
                       :other-folder-repulsion 1.0}
        rounding-tuning (assoc folder-tuning :vertical-squash 1.6 :depth-squash 1.8)
        simulation (layout/create-simulation (mapv (fn [folder]
                                                     {:id folder
                                                      :radius (get radii folder)
                                                      :group (source/directory-of folder)
                                                      :parent-group (source/directory-of (source/directory-of folder))})
                                                   folders)
                                             (folder-links folder-of-id edges folders)
                                             nil
                                             {:tuning folder-tuning})
        rounding (-> (layout/settle! simulation flat-steps)
                     (assoc :tuning (merge layout/default-tuning rounding-tuning))
                     (layout/reheat! rounding-alpha)
                     (layout/settle! rounding-steps))]
    (into {}
          (map (fn [folder] [folder {:center (layout/position rounding folder) :radius (get radii folder)}]))
          folders)))

(defn- folder-page-links
  "`edges` as links for pages inside folder clouds: full strength within a
  folder, and a light pull across folders, so each cloud keeps its round
  shape and only leans toward the folders it uses."
  [edges visuals]
  (let [across-share 0.15]
    (mapv (fn [{:keys [from to] :as edge}]
            (assoc edge :weight (if (= (get-in visuals [from :directory]) (get-in visuals [to :directory]))
                                  1.0
                                  across-share)))
          edges)))

(defn layout-request
  "What the renderer needs to place pages: {:kind :simulation :links
  :options}, with :anchors and :folder-plan for folders, or {:kind :slots
  :order :slots} for a ranked wall."
  [arrangement-key snapshot visuals edges radius-of]
  (let [{chosen-key :key metric :metric} (arrangement arrangement-key)
        ;; Inside a folder's cloud, anchors hold the pages and gravity is
        ;; off. A firm pull toward the middle packs the pages into a ball.
        folder-page-tuning {:gravity 0.0
                            :folder-cohesion 0.0
                            :parent-cohesion 0.0
                            :anchor-containment 0.25
                            :anchor-cohesion 0.03
                            :anchor-depth-squash 1.2
                            :same-folder-repulsion 0.5
                            :other-folder-repulsion 1.0
                            :link-strength 0.02}
        relations-tuning {:folder-cohesion 0.0
                          :parent-cohesion 0.0
                          :same-folder-repulsion 1.0
                          :other-folder-repulsion 1.0
                          :link-strength 0.045}]
    (cond
      metric
      (let [ordered (ranked-ids snapshot (keys visuals) metric)
            sizes (into {} (map (fn [[id {:keys [geometry]}]] [id [(:width geometry) (:height geometry)]])) visuals)
            heights (sort (map second (vals sizes)))
            ;; The median page height sets the gap between pages.
            median-height (if (seq heights) (nth heights (quot (count heights) 2)) 10)
            gap (max 2 (* 0.12 median-height))
            wall-aspect 1.8]
        {:kind :slots
         :order (vec ordered)
         :slots (shelf-slots ordered sizes {:aspect wall-aspect :gap gap})})

      (= :folders chosen-key)
      (let [plan (folder-plan visuals edges radius-of)]
        {:kind :simulation
         :folder-plan plan
         :links (folder-page-links edges visuals)
         :options {:tuning folder-page-tuning
                   :anchors (into {}
                                  (map (fn [[id {:keys [directory]}]] [id (get plan directory)]))
                                  visuals)}})

      :else
      {:kind :simulation
       :links edges
       :options {:tuning relations-tuning}})))

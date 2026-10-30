(ns codecloud.render.overlay
  "Per-frame overlay facts: labels, folder clouds on screen, and the
  hovered thing."
  (:require [codecloud.presentation.arrangement :as arrangement]
            [codecloud.presentation.ui-state :as ui-state]
            [codecloud.presentation.wording :as wording]
            [codecloud.presentation.zoom :as zoom]
            [codecloud.render.camera :as camera]
            [codecloud.render.gestures :as gestures]
            [codecloud.render.navigation :as navigation]
            [codecloud.render.pages :as pages]))

(defn folder-screens
  "Where each folder's cloud falls on screen, from {path {:center
  :radius}}."
  [app extents]
  (let [^js camera (:camera app)
        {:keys [forward]} (camera/basis app)
        {:keys [width height]} (camera/viewport app)]
    (into {}
          (map (fn [[path {:keys [center radius]}]]
                 (let [depth (camera/depth-along camera forward center)
                       [screen-x screen-y] (camera/screen-point app center)
                       radius-pixels (if (pos? depth)
                                       (* radius (zoom/pixels-per-unit depth navigation/field-of-view height))
                                       0)]
                   [path {:center center
                          :depth depth
                          :screen-x screen-x
                          :screen-y screen-y
                          :radius-pixels radius-pixels
                          :on-screen? (and (pos? depth)
                                           (< (- screen-x radius-pixels) width) (> (+ screen-x radius-pixels) 0)
                                           (< (- screen-y radius-pixels) height) (> (+ screen-y radius-pixels) 0))}])))
          extents)))

(defn- lifted-rectangle
  "Screen rectangle of the lifted page, which labels keep clear of."
  [app]
  (when-let [page-object (some->> (get-in @(:frame app) [:lifts :page :id]) (pages/page-of (:page-layer app)))]
    (let [{:keys [screen-x screen-y half-width-pixels half-height-pixels]} page-object
          title-room 24]
      {:left (- screen-x half-width-pixels) :right (+ screen-x half-width-pixels)
       :top (- screen-y half-height-pixels title-room) :bottom (+ screen-y half-height-pixels)})))

(defn- occluder-table
  "Pages solid enough to hide a label, nearest first, as [depth left right
  top bottom id] vectors for a fast scan."
  [app]
  (->> (pages/all-pages (:page-layer app))
       (filter #(and (:on-screen? %) (> (:opacity % 0) 0.45)))
       (map (fn [{:keys [id depth screen-x screen-y half-width-pixels half-height-pixels]}]
              [depth
               (- screen-x half-width-pixels) (+ screen-x half-width-pixels)
               (- screen-y half-height-pixels) (+ screen-y half-height-pixels)
               id]))
       (sort-by first)
       vec))

(defn- clear-test
  "Predicate that keeps a label off nearer pages and off a lifted page."
  [app]
  (let [occluders (occluder-table app)
        clear-of (lifted-rectangle app)
        covered? (fn [{label-key :key :keys [x y depth]}]
                   (let [own-id (when (= :page (first label-key)) (second label-key))
                         nearer-depth (* 0.98 depth)]
                     (reduce (fn [_ [occluder-depth left right top bottom occluder-id]]
                               (cond
                                 (>= occluder-depth nearer-depth) (reduced false)
                                 (and (not= occluder-id own-id) (< left x right) (< top y bottom)) (reduced true)
                                 :else false))
                             false
                             occluders)))]
    (fn [{:keys [x y] :as candidate}]
      (and (not (covered? candidate))
           (not (and clear-of
                     (< (:left clear-of) x (:right clear-of))
                     (< (:top clear-of) y (:bottom clear-of))))))))

(defn- page-labels
  "Label candidates for pages too far away to read their name band."
  [app state lifting?]
  (let [readable-pixels 9
        label-gap 3]
    (for [page-object (pages/all-pages (:page-layer app))
          :let [{:keys [id visual on-screen? pixels-per-unit screen-x screen-y half-height-pixels opacity]} page-object]
          :when (and on-screen? (< pixels-per-unit readable-pixels) (> (or opacity 0) 0.12)
                     (not (:leaving? page-object))
                     (not (and lifting? (< (:emphasis visual 1) 0.5))))]
      {:key [:page id]
       :kind :page
       :badge (some->> (:rank visual) (str "#"))
       :prefix (:prefix visual)
       :leaf (:leaf visual)
       :x screen-x
       :y (- screen-y (max 0 half-height-pixels) label-gap)
       :depth (:depth page-object)
       ;; Hovered and lifted pages win, then pages in a cycle, the first
       ;; places of a ranked wall, and large, important pages.
       :priority (+ (:importance visual 0)
                    (* 0.004 half-height-pixels)
                    (if (or (= id (:hovered-id state)) (:lifted? visual)) 10 0)
                    (if (:cyclic? visual) 0.5 0)
                    (if-let [rank (:rank visual)] (/ 4 rank) 0))
       :classes {"is-cyclic" (:cyclic? visual)
                 "is-looped" (:cycle-warning? visual)
                 "is-parse-error" (:parse-error? visual)
                 "is-dim" (< (:emphasis visual 1) 0.5)}})))

(defn- folder-labels
  "Label candidates for folder clouds seen from afar."
  [app lifting?]
  (let [{:keys [directory-visuals folder-screens hover]} @(:frame app)
        smallest-pixels 26
        largest-share 0.9
        label-rise 0.72]
    (for [[path {:keys [screen-x screen-y radius-pixels on-screen? depth]}] folder-screens
          :let [{:keys [prefix leaf emphasis]} (get directory-visuals path)]
          :when (and on-screen? (< smallest-pixels radius-pixels (* largest-share (camera/viewport-height app)))
                     (not (and lifting? (< (or emphasis 1) 0.5))))]
      {:key [:folder path]
       :kind :folder
       :prefix prefix
       :leaf leaf
       :x screen-x
       :y (- screen-y (* label-rise radius-pixels))
       :depth depth
       :priority (+ 1.4 (* 0.002 radius-pixels) (if (= path (:path hover)) 10 0))
       :classes {"is-dim" (< (or emphasis 1) 0.5)}})))

(defn label-candidates
  "Labels for distant pages and folder clouds, with the test that keeps
  them off nearer pages and the lifted page."
  [app]
  (let [state @(:state app)
        lifting? (seq (:lifts @(:frame app)))]
    {:candidates (concat (when (arrangement/shows-folder-clouds? (:arrangement state))
                           (folder-labels app lifting?))
                         (when-not (= :names (:display-mode state))
                           (page-labels app state lifting?)))
     :clear? (clear-test app)}))

(defn refresh-hover!
  "Picks under the pointer when it moved and reports the hovered page."
  [app]
  (let [{:keys [pointer drag carried]} @(:frame app)]
    (when (and (:moved? pointer) (nil? drag) (nil? carried))
      (swap! (:frame app) assoc-in [:pointer :moved?] false)
      (let [hit (when (:inside? pointer) (gestures/pick app (:ndc-x pointer) (:ndc-y pointer)))
            hover (when hit (select-keys hit [:kind :id :node :origin :path]))
            previous (:hover @(:frame app))]
        (swap! (:frame app) assoc :hover hover)
        (when (not= (select-keys previous [:kind :id :path]) (select-keys hover [:kind :id :path]))
          (swap! (:frame app) assoc :hover-since (js/performance.now)))
        (let [hovered-id (when (contains? #{:namespace :code} (:kind hit)) (:id hit))]
          (when (not= hovered-id (:hovered-id @(:state app)))
            ((:dispatch! app) [:hover hovered-id])))
        (set! (.. ^js (.-domElement ^js (:renderer app)) -style -cursor)
              (cond
                (gestures/text-cursor? app hit) "text"
                hit "pointer"
                :else "grab")))))
  (when (:carried @(:frame app))
    (set! (.. ^js (.-domElement ^js (:renderer app)) -style -cursor) "grabbing")))

(defn shown-hover
  "What the tooltip describes: the hovered thing, unless lifted or
  carried. Code waits until the pointer rests on it."
  [app now]
  (let [{:keys [hover hover-since pointer directory-visuals visuals carried]} @(:frame app)
        state @(:state app)
        code-hint-delay 900]
    (when (and (:inside? pointer)
               (nil? carried)
               (not (and (contains? #{:namespace :code} (:kind hover))
                         (= (:id hover) (:focused-id state))))
               (or (not= :code (:kind hover))
                   (> (- now (or hover-since now)) code-hint-delay)))
      (case (:kind hover)
        :folder (let [folder (get directory-visuals (:path hover))]
                  (assoc hover
                         :folder folder
                         :unit-words (wording/unit-words {:namespaces (select-keys (get-in state [:snapshot :namespaces])
                                                                                   (:member-ids folder))})))
        :code (assoc hover :editable? (ui-state/editable? state (:node hover) (get-in hover [:origin :path])))
        :namespace (assoc hover
                          :rank (get-in visuals [(:id hover) :rank])
                          :ranked-by (:label (arrangement/arrangement (:arrangement state)))
                          :moved? (contains? (:moved-pages state) (:id hover)))
        hover))))

(ns codecloud.render.flights
  "Camera flights to the cloud, a folder, a page, or a line; arrivals from
  the editor; and lifts that bring a page or folder toward the viewer."
  (:require [codecloud.presentation.page :as page]
            [codecloud.presentation.zoom :as zoom]
            [codecloud.render.camera :as camera]
            [codecloud.render.navigation :as navigation]
            [codecloud.render.pages :as pages]
            [codecloud.render.placement :as placement]))

(defn examine!
  "Marks the page the camera is headed for, so pages nearer the camera
  step aside while it rests there, or with nil marks none."
  [app id]
  (swap! (:frame app) assoc :examined-id id))

(defn- shifted
  "`point` moved `distance` along the Vector3 `direction`."
  [[x y z] ^js direction distance]
  [(+ x (* distance (.-x direction)))
   (+ y (* distance (.-y direction)))
   (+ z (* distance (.-z direction)))])

(defn frame-everything!
  "Flies to where the whole cloud fits the part of the view that no side
  panel covers."
  [app duration]
  (when (placement/ready? app)
    (let [center (placement/center app)
          distance (camera/fitting-distance app center (placement/extents app))
          {:keys [right]} (camera/basis app)]
      (examine! app nil)
      (navigation/fly-to! (:controller app)
                          (shifted center right (camera/panel-shift app distance))
                          distance
                          duration))))

(defn page-center
  "Where a page is drawn now, or where it belongs."
  [app id]
  (or (:position (pages/page-of (:page-layer app) id))
      (placement/target-of app id)))

(defn page-point
  "World position of page-local point [x y] on the page `id`. Pages face
  the camera, so local axes follow the camera's right and up."
  [app id [local-x local-y]]
  (when-let [[x y z] (page-center app id)]
    (let [{:keys [right up]} (camera/basis app)]
      [(+ x (* local-x (.-x right)) (* local-y (.-x up)))
       (+ y (* local-x (.-y right)) (* local-y (.-y up)))
       (+ z (* local-x (.-z right)) (* local-y (.-z up)))])))

(defn fly-to-page!
  "Flies to a page until it fills the open part of the screen, stopping
  short of making its code larger than the editor's."
  [app id]
  (when-let [{:keys [geometry]} (get-in @(:frame app) [:visuals id])]
    (when-let [position (placement/target-of app id)]
      (examine! app id)
      (navigation/fly-to! (:controller app) position
                          (camera/distance-for-pixels-per-unit
                           app (min (camera/fitting-pixels-per-unit app (:width geometry) (:height geometry))
                                    (camera/reading-pixels app)))
                          1.0))))

(defn fly-to-line!
  "Flies to a line of a page until its code shows at `reading-share` of
  the editor's size."
  [app {:keys [namespace-id path line]} reading-share duration]
  (when-let [page-object (pages/page-of (:page-layer app) namespace-id)]
    (when-let [point (page-point app namespace-id (pages/line-local-point page-object path line))]
      (examine! app namespace-id)
      (navigation/fly-to! (:controller app) point
                          (camera/distance-for-pixels-per-unit app (* reading-share (camera/reading-pixels app)))
                          duration))))

(defn frame-directory!
  "Flies to where one folder's cloud fills the view."
  [app directory]
  (when-let [{:keys [center radius]} (placement/folder-extent app directory)]
    (let [{:keys [width height]} (camera/viewport app)]
      (examine! app nil)
      (navigation/fly-to! (:controller app) center
                          (zoom/framing-distance radius navigation/field-of-view (/ width height))
                          1.0))))

;; Arriving from the editor

(defn- arrival-point
  "Where the camera looks on arriving from the editor, the middle of the
  code on the line the viewer left."
  [app {:keys [namespace-id path line]}]
  (when-let [{:keys [files geometry]} (get-in @(:frame app) [:visuals namespace-id])]
    (let [panel-line (or (page/origin-panel-line files path line) 0)
          [line-x line-y] (page/line-center geometry panel-line)
          reading-x (+ line-x (* 0.5 (:code-characters geometry) (:character-width geometry)))]
      (page-point app namespace-id [reading-x line-y]))))

(defn- hold-arrival!
  "Keeps the camera on the arrival line with code at the size the editor
  last showed it, following the page while the layout settles."
  [app {:keys [text-scale] :as arrival}]
  (when-let [point (arrival-point app arrival)]
    (let [default-threshold-percent 150
          scale (or text-scale (zoom/arrival-text-scale default-threshold-percent))]
      (navigation/jump-to! (:controller app) point
                           (camera/distance-for-pixels-per-unit app (* scale (camera/reading-pixels app)))))))

(defn arrive!
  "Starts the camera on the line the viewer left in the editor, where it
  stays until the code shows and then draws back."
  [app {:keys [namespace-id] :as arrival}]
  (swap! (:frame app) dissoc :dived)
  (when (get-in @(:frame app) [:visuals namespace-id])
    (hold-arrival! app arrival)
    (examine! app namespace-id)
    (swap! (:frame app) assoc :arrival (assoc arrival :started-at (js/performance.now)))))

(defn- draw-back!
  "Flies back from an arrival until the page fills about half the open
  screen, or at least far enough to show the cloud around a short page."
  [app namespace-id]
  (when-let [{:keys [geometry]} (get-in @(:frame app) [:visuals namespace-id])]
    (when-let [position (page-center app namespace-id)]
      (let [controller (:controller app)
            page-share 0.6
            least-draw-back 1.8
            duration 2.6]
        (navigation/fly-to! controller position
                            (max (* least-draw-back (navigation/distance controller))
                                 (camera/distance-for-pixels-per-unit
                                  app (* page-share (camera/fitting-pixels-per-unit app (:width geometry) (:height geometry)))))
                            duration)))))

(defn return-from-dive!
  "Draws back from the page the viewer dove into, once the cloud comes
  back into view without an arrival from the editor, as after VS Code's
  Go Back or a click on the cloud's tab."
  [app]
  (when-let [{:keys [namespace-id]} (:dived @(:frame app))]
    (swap! (:frame app) dissoc :dived)
    (when-not (:arrival @(:frame app))
      (draw-back! app namespace-id))))

(defn advance-arrival!
  "After arriving from the editor, holds on the line until the code shows,
  then draws back, so leaving the editor reads as one zoom out."
  [app now]
  (when-let [{:keys [namespace-id started-at] :as arrival} (:arrival @(:frame app))]
    (let [shown-text-share 0.6
          text-ready? (> (:text-share (pages/page-of (:page-layer app) namespace-id) 0) shown-text-share)
          waited (- now started-at)
          shortest-wait 250
          longest-wait 1200]
      (if (or (and text-ready? (> waited shortest-wait)) (> waited longest-wait))
        (do (swap! (:frame app) dissoc :arrival)
            (draw-back! app namespace-id))
        (hold-arrival! app arrival)))))

;; Framing when the cloud first appears

(defn first-framing!
  "Frames the cloud once its first snapshot has a layout, unless an
  arrival from the editor or a restored camera says where to look."
  [app]
  (let [frame @(:frame app)]
    (when (and (not (:framed? frame)) (placement/ready? app) (seq (:visuals frame)) (seq (placement/targets app)))
      (placement/presettle! app)
      (swap! (:frame app) assoc :framed? true)
      (when-not (or (:arrival frame) (:restored-view? frame))
        (let [center (placement/center app)
              start-farther 1.6]
          (navigation/jump-to! (:controller app) center
                               (* start-farther (camera/fitting-distance app center (placement/extents app))))
          (navigation/update! (:controller app) 0)
          (frame-everything! app 2.2))))))

(defn frame-when-settled!
  "Frames the cloud again once the first layout comes to rest, unless the
  viewer has already taken the camera somewhere."
  [app]
  (let [{:keys [framed? settle-framed? user-moved? arrival restored-view?]} @(:frame app)]
    (when (and framed? (not settle-framed?) (placement/settled? app))
      (swap! (:frame app) assoc :settle-framed? true)
      (when-not (or user-moved? arrival restored-view?)
        (frame-everything! app 1.6)))))

(defn refit-later!
  "Frames the whole cloud once a changed layout has had a moment to take
  shape, unless the viewer moves the camera or lifts something first."
  [app]
  (let [settle-milliseconds 700
        requested-at (js/performance.now)]
    (js/setTimeout (fn []
                     (let [{:keys [lifts last-interaction]} @(:frame app)]
                       (when (and (empty? lifts) (< (or last-interaction 0) requested-at))
                         (frame-everything! app 1.2))))
                   settle-milliseconds)))

;; Lifting pages and folders toward the viewer

(defn- lift-distance
  "Distance at which a lifted `width` by `height` area fills the open
  screen, with text at most twice the editor's size."
  [app width height]
  (let [largest-text-share 2
        pixels-per-unit (min (camera/fitting-pixels-per-unit app width height)
                             (* largest-text-share (camera/reading-pixels app)))]
    (camera/distance-for-pixels-per-unit app pixels-per-unit)))

(defn- pivot-on-lift!
  "Moves the pivot to the lifted object along the line of sight, so
  orbiting circles it. The first lift remembers the old pivot distance."
  [app distance]
  (let [controller (:controller app)]
    (when-not (:pivot-distance @(:frame app))
      (swap! (:frame app) assoc :pivot-distance (navigation/distance controller)))
    (navigation/retarget-along-view! controller distance)))

(defn restore-pivot!
  "Returns the pivot to where it was before anything was lifted."
  [app]
  (let [{:keys [lifts pivot-distance]} @(:frame app)]
    (when (and pivot-distance (empty? lifts))
      (navigation/retarget-along-view! (:controller app) pivot-distance)
      (swap! (:frame app) dissoc :pivot-distance))))

(defn lift-page!
  "Lifts page `id` toward the viewer until it fills the open part of the
  screen."
  [app id]
  (when-let [{:keys [geometry]} (get-in @(:frame app) [:visuals id])]
    (let [distance (lift-distance app (:width geometry) (:height geometry))]
      (swap! (:frame app) assoc-in [:lifts :page] {:id id :destination (camera/point-ahead app distance)})
      (pivot-on-lift! app distance))))

(defn lift-folder!
  "Lifts a folder's cloud toward the viewer, with its pages in their
  places, in place of any lifted page."
  [app directory]
  (when-let [{:keys [radius]} (placement/folder-extent app directory)]
    (let [clearance 1.15
          distance (max (* clearance radius) (lift-distance app (* 2 radius) (* 2 radius)))]
      (swap! (:frame app) update :lifts #(-> %
                                             (assoc :folder {:path directory :destination (camera/point-ahead app distance)})
                                             (dissoc :page)))
      (pivot-on-lift! app distance))))

(defn set-down!
  "Sets down a lifted page (`lift-kind` :page) or folder (:folder)."
  [app lift-kind]
  (swap! (:frame app) update :lifts dissoc lift-kind)
  (restore-pivot! app))

(defn set-down-everything! [app]
  (swap! (:frame app) assoc :lifts {})
  (restore-pivot! app))

(defn lifted-ids
  "Ids of the lifted page and of every page of the lifted folder."
  [app]
  (let [{:keys [lifts directory-visuals]} @(:frame app)]
    (into (set (some-> (get-in lifts [:page :id]) vector))
          (get-in directory-visuals [(get-in lifts [:folder :path]) :member-ids]))))

(defn lift-depth
  "Distance in front of the camera of the lifted page, or nil."
  [app]
  (when-let [destination (get-in @(:frame app) [:lifts :page :destination])]
    (camera/depth-along (:camera app) (:forward (camera/basis app)) destination)))

(defn offset-function
  "Function of a page id and home position giving its offset: to the lift
  point, with a lifted folder, or as moved by hand."
  [app]
  (let [{:keys [lifts visuals carried]} @(:frame app)
        moved-pages (:moved-pages @(:state app))
        {page-lift :page folder-lift :folder} lifts
        folder-center (when folder-lift (:center (placement/folder-extent app (:path folder-lift))))
        difference (fn [[to-x to-y to-z] [from-x from-y from-z]] [(- to-x from-x) (- to-y from-y) (- to-z from-z)])]
    (fn [id position]
      (cond
        (and page-lift (= id (:id page-lift))) (difference (:destination page-lift) position)
        (and folder-lift folder-center (= (get-in visuals [id :directory]) (:path folder-lift)))
        (difference (:destination folder-lift) folder-center)
        (and carried (= id (:id carried))) (:offset carried)
        :else (get moved-pages id [0 0 0])))))

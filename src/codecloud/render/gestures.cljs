(ns codecloud.render.gestures
  "Pointer and keyboard input: picking, orbiting, panning, zooming toward
  the pointer, diving into the editor, carrying pages, and pointing into
  code to edit it. Keys reach the state through :dispatch!."
  (:require ["three" :as three]
            [codecloud.presentation.editing :as text-editing]
            [codecloud.presentation.page :as page]
            [codecloud.presentation.ui-state :as ui-state]
            [codecloud.presentation.zoom :as zoom]
            [codecloud.render.camera :as camera]
            [codecloud.render.editing :as editing]
            [codecloud.render.flights :as flights]
            [codecloud.render.navigation :as navigation]
            [codecloud.render.pages :as pages]
            [codecloud.render.placement :as placement]
            [codecloud.render.typing :as typing]))

(defn- dispatch! [app event]
  ((:dispatch! app) event))

;; Picking

(defn page-hit
  "The nearest page under the pointer, with the code line there."
  [app ^js raycaster]
  (let [pairs (pages/body-meshes (:page-layer app))
        page-of-mesh (js/Map.)]
    (doseq [[mesh page-object] pairs] (.set page-of-mesh mesh page-object))
    (when-let [^js hit (first (array-seq (.intersectObjects raycaster (into-array (map first pairs)) false)))]
      (let [page-object (.get page-of-mesh (.-object hit))
            display-mode (:display-mode @(:state app))
            local (pages/local-point page-object (.-uv hit) display-mode)
            line (when (= :code display-mode) (pages/line-at-point page-object local))]
        (cond-> {:kind (if line :code :namespace)
                 :id (:id page-object)
                 :page page-object
                 :local local
                 :point (camera/vector-of (.-point hit))
                 :distance (.-distance hit)
                 :node (get-in @(:state app) [:visible-snapshot :namespaces (:id page-object)])}
          line (assoc :origin (:origin line) :panel-line (:panel-line line)))))))

(defn- folder-hit
  "The innermost folder cloud whose circle on screen holds the pointer."
  [app]
  (let [{:keys [pointer folder-screens]} @(:frame app)
        {:keys [x y]} pointer
        largest-share 1.4
        reach 0.82
        smallest-pixels 18]
    (when-let [[path {:keys [center]}]
               (->> folder-screens
                    (filter (fn [[_path {:keys [screen-x screen-y radius-pixels on-screen?]}]]
                              (and on-screen?
                                   (< smallest-pixels radius-pixels (* largest-share (camera/viewport-height app)))
                                   (< (js/Math.hypot (- x screen-x) (- y screen-y)) (* reach radius-pixels)))))
                    (sort-by (comp :radius-pixels val))
                    first)]
      {:kind :folder :path path :point center})))

(defn pick
  "What lies under a pointer position: a line of code, a page, or a folder
  cloud, preferring pages."
  [app ndc-x ndc-y]
  (or (page-hit app (camera/pointer-ray app ndc-x ndc-y))
      (folder-hit app)))

;; Zooming and diving into the editor

(defn- push-dive!
  "Builds dive pressure while scrolling into code at editor size, opening
  the editor when full. Wheel events right after a dive are ignored,
  since trackpads keep scrolling."
  [app hit wheel-pixels]
  (let [now (js/performance.now)
        {:keys [pressure last-push resting-until]} (:dive @(:frame app))
        elapsed (/ (- now (or last-push now)) 1000)
        next-pressure (zoom/press-dive (or pressure 0) wheel-pixels elapsed)
        rest-milliseconds 1200]
    (cond
      (< now (or resting-until 0)) nil
      (>= next-pressure 1) (do (swap! (:frame app) assoc
                                      :dive {:pressure 0
                                             :last-push now
                                             :resting-until (+ now rest-milliseconds)}
                                      :dived {:namespace-id (:id hit)})
                               (dispatch! app [:dive (:origin hit)]))
      :else (swap! (:frame app) assoc :dive {:pressure next-pressure :last-push now}))))

(defn- wheel-pixels [^js event]
  (let [line-pixels 16
        page-pixels 800]
    (* (.-deltaY event) (case (.-deltaMode event) 1 line-pixels 2 page-pixels 1))))

(defn- note-interaction!
  "Records that the viewer took the camera, which ends an arrival from
  the editor and any pending reframing."
  [app]
  (swap! (:frame app) #(-> %
                           (assoc :last-interaction (js/performance.now) :user-moved? true)
                           (dissoc :arrival)
                           (assoc-in [:pointer :moved?] true))))

(defn- on-wheel
  "Zooms toward the pointer. Over code, zooming stops where the code
  reaches the editor's size, and further scrolling dives into the file."
  [app ^js event]
  (.preventDefault event)
  (let [{:keys [ndc-x ndc-y]} (:pointer @(:frame app))
        pixels (wheel-pixels event)
        zoom-in? (neg? pixels)
        zoom-rate 0.0015
        largest-step 400
        factor (js/Math.pow (+ 1 zoom-rate) (max (- largest-step) (min largest-step pixels)))
        hit (page-hit app (camera/pointer-ray app ndc-x ndc-y))
        controller (:controller app)
        editor-pixels (camera/reading-pixels app)
        code-pixels (when (= :code (:kind hit)) (get-in hit [:page :pixels-per-unit]))
        at-floor? (and zoom-in? code-pixels (>= code-pixels (* 0.97 editor-pixels)))
        floor-factor (if (and zoom-in? code-pixels)
                       (min 1 (max factor (/ code-pixels editor-pixels)))
                       factor)]
    (note-interaction! app)
    (cond
      at-floor? (push-dive! app hit pixels)
      hit (navigation/zoom-about! controller (:point hit) floor-factor)
      :else (navigation/zoom-about! controller (navigation/focus-point-under controller ndc-x ndc-y) factor))))

;; Pointing into code

(defn editable-code?
  "True when `hit` lands on code that can be edited where it shows: real
  text, large enough to point into, in a file the cloud can edit."
  [app {:keys [kind page node origin]}]
  (let [least-line-pixels 8
        text-showing 0.5]
    (and (= :code kind)
         (>= (:pixels-per-unit page 0) least-line-pixels)
         (> (:text-share page 0) text-showing)
         (ui-state/editable? @(:state app) node (:path origin)))))

(defn- editing-buffer
  "The buffer being edited, with its :path, or nil."
  [app]
  (let [{:keys [editing buffers]} @(:state app)]
    (get buffers (:path editing))))

(defn- begin-editing-at!
  "Puts the caret into the code under `hit`, selecting the word or line
  there when `select` says so. Returns true when editing began."
  [app {:keys [id page origin local]} select]
  (let [state @(:state app)
        path (:path origin)
        buffer (or (get-in state [:buffers path])
                   (when-let [source-text (some #(when (= path (:path %)) (:text %)) (ui-state/source-of state id))]
                     (assoc (text-editing/buffer source-text) :path path)))
        position (when buffer (editing/text-position page local buffer false))
        {:keys [columns rows]} (get-in page [:visual :geometry])]
    (when position
      (dispatch! app [:begin-editing {:namespace-id id :path path :position position :select select
                                      :columns columns :rows rows}])
      (typing/focus! (:input app))
      true)))

(defn- page-plane-local
  "Page-local [x y] where the pointer's ray meets the plane of
  `page-object`, which faces the camera, or nil."
  [app page-object ndc-x ndc-y]
  (let [^js raycaster (camera/pointer-ray app ndc-x ndc-y)
        {:keys [^js right ^js up ^js forward]} (camera/basis app)
        [center-x center-y center-z] (:position page-object)
        ^js center (three/Vector3. center-x center-y center-z)
        ^js plane (.setFromNormalAndCoplanarPoint (three/Plane.) (.negate (.clone forward)) center)
        ^js meeting (three/Vector3.)]
    (when (.intersectPlane (.-ray raycaster) plane meeting)
      (let [^js offset (.sub meeting center)]
        [(.dot offset right) (.dot offset up)]))))

(defn- on-edited-page?
  "True when `hit` lands below the name band of the page being edited."
  [app {:keys [id page local]}]
  (let [{:keys [editing]} @(:state app)]
    (and (= id (:namespace-id editing))
         (editing-buffer app)
         local
         (< (second local) (page/code-top (get-in page [:visual :geometry]))))))

(defn- count-press!
  "How many quick presses in one spot this one makes, one to three, for
  choosing a word or a line."
  [app ^js event]
  (let [now (js/performance.now)
        {:keys [at x y presses]} (:last-press @(:frame app))
        quick-milliseconds 450
        near-pixels 5
        presses (if (and at (< (- now at) quick-milliseconds)
                         (< (js/Math.hypot (- (.-clientX event) x) (- (.-clientY event) y)) near-pixels))
                  (min 3 (inc presses))
                  1)]
    (swap! (:frame app) assoc :last-press {:at now :x (.-clientX event) :y (.-clientY event) :presses presses})
    presses))

(defn- start-selecting!
  "Places the caret where the pointer went down on the page being edited,
  extending the selection with Shift, or picks a word on the second press
  and a line on the third; dragging then extends the selection."
  [app {:keys [page local]} ^js event presses]
  (let [buffer (editing-buffer app)
        position (editing/text-position page local buffer true)]
    (case presses
      1 (dispatch! app [:edit :place position (.-shiftKey event)])
      2 (dispatch! app [:edit :select-word position])
      (dispatch! app [:edit :select-line position]))
    (swap! (:frame app) assoc :selecting true)
    (typing/focus! (:input app))))

(defn text-cursor?
  "True when the pointer over `hit` points into text: code that a click
  can edit, or the page being edited."
  [app hit]
  (boolean (and hit (or (editable-code? app hit) (on-edited-page? app hit)))))

(defn- extend-selection!
  "Extends the selection to the text nearest the pointer."
  [app]
  (let [{:keys [ndc-x ndc-y]} (:pointer @(:frame app))
        {:keys [editing]} @(:state app)
        buffer (editing-buffer app)]
    (when-let [page-object (when buffer (pages/page-of (:page-layer app) (:namespace-id editing)))]
      (when-let [local (page-plane-local app page-object ndc-x ndc-y)]
        (dispatch! app [:edit :place (editing/text-position page-object local buffer true) true])))))

;; Carrying a page

(defn- carry-modifier?
  "True when the pointer goes down with the key that picks pages up:
  Ctrl, Cmd on macOS, or Alt."
  [^js event]
  (or (.-ctrlKey event) (.-metaKey event) (.-altKey event)))

(defn- plane-point
  "Where the pointer's ray meets the plane through `point` facing the
  camera, or nil."
  [app [x y z] ndc-x ndc-y]
  (let [^js raycaster (camera/pointer-ray app ndc-x ndc-y)
        ^js normal (.negate (.getWorldDirection ^js (:camera app) (three/Vector3.)))
        ^js plane (.setFromNormalAndCoplanarPoint (three/Plane.) normal (three/Vector3. x y z))
        ^js meeting (three/Vector3.)]
    (when (.intersectPlane (.-ray raycaster) plane meeting)
      (camera/vector-of meeting))))

(defn- start-carry!
  "Picks up the page under the pointer unless it is lifted, returning
  whether it did."
  [app {:keys [id point]}]
  (let [lifted? (contains? (flights/lifted-ids app) id)]
    (when-let [center (and (not lifted?) (:position (pages/page-of (:page-layer app) id)))]
      (let [[point-x point-y point-z] point
            [center-x center-y center-z] center]
        (swap! (:frame app) assoc :carried
               {:id id
                :grab [(- point-x center-x) (- point-y center-y) (- point-z center-z)]
                :plane-anchor point
                :offset (get-in @(:state app) [:moved-pages id] [0 0 0])})
        true))))

(defn- carry-to-pointer!
  "Keeps the carried page's grab point under the pointer, measuring the
  offset from its layout position."
  [app]
  (let [{:keys [carried pointer]} @(:frame app)
        {:keys [id grab plane-anchor]} carried]
    (when-let [[meeting-x meeting-y meeting-z] (plane-point app plane-anchor (:ndc-x pointer) (:ndc-y pointer))]
      (when-let [[anchor-x anchor-y anchor-z] (or (:layout-position (pages/page-of (:page-layer app) id))
                                                  (placement/target-of app id))]
        (let [[grab-x grab-y grab-z] grab]
          (swap! (:frame app) assoc-in [:carried :offset]
                 [(- meeting-x grab-x anchor-x) (- meeting-y grab-y anchor-y) (- meeting-z grab-z anchor-z)]))))))

(defn- set-down-carried!
  "Leaves the carried page where it is and remembers how far it moved."
  [app]
  (when-let [{:keys [id offset]} (:carried @(:frame app))]
    (swap! (:frame app) dissoc :carried)
    (dispatch! app [:move-page id offset])))

;; Pointer

(defn update-pointer! [app ^js event]
  (let [^js bounds (.getBoundingClientRect ^js (.-domElement ^js (:renderer app)))
        x (- (.-clientX event) (.-left bounds))
        y (- (.-clientY event) (.-top bounds))]
    (swap! (:frame app) update :pointer assoc
           :x x :y y
           :inside? true
           :ndc-x (- (* 2 (/ x (.-width bounds))) 1)
           :ndc-y (- 1 (* 2 (/ y (.-height bounds))))
           :moved? true)))

(defn- on-pointer-down
  "Starts a selection on the page being edited, a carry with Ctrl or Alt,
  or a drag that orbits or pans."
  [app ^js event]
  (update-pointer! app event)
  (.setPointerCapture ^js (.-target event) (.-pointerId event))
  (let [{:keys [ndc-x ndc-y]} (:pointer @(:frame app))
        left? (zero? (.-button event))
        presses (when left? (count-press! app event))
        hit (when left? (page-hit app (camera/pointer-ray app ndc-x ndc-y)))]
    (swap! (:frame app) dissoc :arrival)
    (cond
      (and hit (not (carry-modifier? event)) (on-edited-page? app hit))
      (start-selecting! app hit event presses)

      (and hit (carry-modifier? event) (start-carry! app hit))
      nil

      :else
      (swap! (:frame app) assoc :drag {:button (.-button event)
                                       :shift? (.-shiftKey event)
                                       :start [(.-clientX event) (.-clientY event)]
                                       :last [(.-clientX event) (.-clientY event)]
                                       :last-move (js/performance.now)
                                       :moved? false}))))

(defn- on-pointer-move [app ^js event]
  (update-pointer! app event)
  (cond
    (:selecting @(:frame app)) (extend-selection! app)
    (:carried @(:frame app)) (carry-to-pointer! app)
    :else
    (when-let [{:keys [button shift? start] previous :last :as drag} (:drag @(:frame app))]
      (let [[start-x start-y] start
            [last-x last-y] previous
            delta-x (- (.-clientX event) last-x)
            delta-y (- (.-clientY event) last-y)
            drag-threshold 4
            moved? (or (:moved? drag)
                       (> (js/Math.hypot (- (.-clientX event) start-x) (- (.-clientY event) start-y)) drag-threshold))]
        (when moved?
          (if (or (= 2 button) (= 1 button) shift?)
            (navigation/pan! (:controller app) delta-x delta-y (camera/viewport-height app))
            (navigation/orbit! (:controller app) delta-x delta-y (camera/viewport-height app))))
        (swap! (:frame app) assoc
               :last-interaction (if moved? (js/performance.now) (:last-interaction @(:frame app)))
               :user-moved? (or (:user-moved? @(:frame app)) moved?)
               :drag (assoc drag :last [(.-clientX event) (.-clientY event)]
                            :last-move (js/performance.now)
                            :moved? moved?))))))

(defn- menu-for [hit {:keys [x y]}]
  (case (:kind hit)
    (:code :namespace) {:kind :page :id (:id hit) :x x :y y}
    :folder {:kind :folder :path (:path hit) :x x :y y}
    {:kind :sky :x x :y y}))

(defn- click-event [hit]
  (case (:kind hit)
    (:code :namespace) [:click-namespace (:id hit)]
    :folder [:click-folder (:path hit)]
    [:click-empty]))

(defn- on-pointer-up
  "Ends a selection, a drag, or a carry, or clicks. A click on readable
  code that can be edited puts the caret there at once. Other left clicks
  wait a moment, so a double-click's first half lifts nothing."
  [app ^js _event]
  (cond
    (:selecting @(:frame app)) (swap! (:frame app) dissoc :selecting)
    (:carried @(:frame app)) (set-down-carried! app)
    :else
    (when-let [{:keys [moved? last-move button]} (:drag @(:frame app))]
      (swap! (:frame app) assoc :drag nil)
      (let [pause-limit 90]
        (navigation/release-drag! (:controller app) (> (- (js/performance.now) last-move) pause-limit)))
      (when-not moved?
        (let [{:keys [ndc-x ndc-y] :as pointer} (:pointer @(:frame app))
              hit (pick app ndc-x ndc-y)
              double-click-window 240]
          (case button
            0 (when-not (and (editable-code? app hit) (begin-editing-at! app hit nil))
                (let [click-id (js/performance.now)]
                  (swap! (:frame app) assoc :pending-click click-id)
                  (js/setTimeout (fn []
                                   (when (= click-id (:pending-click @(:frame app)))
                                     (swap! (:frame app) dissoc :pending-click)
                                     (dispatch! app (click-event hit))))
                                 double-click-window)))
            2 (dispatch! app [:open-menu (menu-for hit pointer)])
            nil))))))

(defn- on-double-click
  "Flies to the page, folder, or whole cloud under the pointer. On the
  page being edited, the presses themselves chose a word instead."
  [app ^js event]
  (update-pointer! app event)
  (swap! (:frame app) dissoc :pending-click)
  (let [{:keys [ndc-x ndc-y]} (:pointer @(:frame app))
        hit (pick app ndc-x ndc-y)]
    (when-not (and (contains? #{:code :namespace} (:kind hit)) (on-edited-page? app hit))
      (case (:kind hit)
        (:code :namespace) (dispatch! app [:double-click-namespace (:id hit)])
        :folder (dispatch! app [:frame-directory (:path hit)])
        (dispatch! app [:frame-all])))))

;; Keyboard

(defn- typing? [^js event]
  (contains? #{"INPUT" "TEXTAREA"} (.. event -target -tagName)))

(defn- on-key-down
  "Answers the cloud's keys, unless the viewer is typing, holds a
  modifier, or has just opened search and is about to type into it."
  [app ^js event]
  (when-not (or (typing? event) (.-ctrlKey event) (.-metaKey event) (.-altKey event)
                (and (get-in @(:state app) [:search :open?]) (not= "Escape" (.-key event))))
    (let [controller (:controller app)
          zoom-in-factor 0.8
          zoom-out-factor 1.25
          handled (case (.-key event)
                    "/" (dispatch! app [:open-search])
                    "Escape" (dispatch! app [:escape])
                    "Home" (dispatch! app [:frame-all])
                    "ArrowLeft" (dispatch! app [:step-history -1])
                    "ArrowRight" (dispatch! app [:step-history 1])
                    " " (dispatch! app [:toggle-playback])
                    ("n" "N") (dispatch! app [:toggle-display-mode])
                    ("a" "A") (dispatch! app [:toggle-arrange-panel])
                    "?" (dispatch! app [:toggle-legend])
                    "[" (dispatch! app [:step-spread -1])
                    "]" (dispatch! app [:step-spread 1])
                    ("+" "=") (navigation/zoom-about! controller (navigation/target-position controller) zoom-in-factor)
                    "-" (navigation/zoom-about! controller (navigation/target-position controller) zoom-out-factor)
                    ::unhandled)]
      (when-not (= ::unhandled handled)
        (.preventDefault event)))))

(defn attach!
  "Listens for input on the cloud's canvas and the window."
  [app]
  (let [^js canvas (.-domElement ^js (:renderer app))]
    (.addEventListener canvas "wheel" #(on-wheel app %) #js {:passive false})
    (.addEventListener canvas "pointerdown" #(on-pointer-down app %))
    (.addEventListener canvas "pointermove" #(on-pointer-move app %))
    (.addEventListener canvas "pointerup" #(on-pointer-up app %))
    (.addEventListener canvas "pointercancel" #(swap! (:frame app) dissoc :drag :carried))
    (.addEventListener canvas "pointerleave" #(swap! (:frame app) assoc-in [:pointer :inside?] false))
    (.addEventListener canvas "pointerenter" #(update-pointer! app %))
    (.addEventListener canvas "dblclick" #(on-double-click app %))
    (.addEventListener canvas "contextmenu" #(.preventDefault ^js %))
    (.addEventListener js/window "keydown" #(on-key-down app %))))

(ns codecloud.render.gestures
  "Pointer and keyboard input, for picking, orbiting, panning, zooming
  toward the pointer, diving into the editor, carrying pages, and
  pointing into code to edit it. Keys reach the state through :dispatch!."
  (:require ["three" :as three]
            [codecloud.presentation.editing-state :as editing-state]
            [codecloud.presentation.page :as page]
            [codecloud.presentation.ui-state :as ui-state]
            [codecloud.presentation.zoom :as zoom]
            [codecloud.render.camera :as camera]
            [codecloud.render.editing-view :as editing-view]
            [codecloud.render.flights :as flights]
            [codecloud.render.grid :as grid]
            [codecloud.render.navigation :as navigation]
            [codecloud.render.pages :as pages]
            [codecloud.render.placement :as placement]
            [codecloud.render.typing :as typing]
            [codecloud.render.windows :as windows]))

(defn- dispatch! [app event]
  ((:dispatch! app) event))

;; Picking

(defn- body-hit
  "The nearest of the page bodies in `pairs` that the ray meets, with the
  code line there."
  [app ^js raycaster pairs]
  (let [page-of-mesh (js/Map.)]
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

(defn page-hit
  "The nearest page in the cloud under the pointer, with the code line
  there. Pages in the grid's cells stand on layers of their own, which
  the ray leaves out."
  [app ^js raycaster]
  (body-hit app raycaster (pages/body-meshes (:page-layer app))))

(defn- cell-hit
  "The page in grid cell `placed` under the pointer, with the code line
  there, met on the cell's own layer."
  [app ^js raycaster {:keys [cell layer]}]
  (when (= :page (:kind cell))
    (.set (.-layers raycaster) layer)
    (body-hit app raycaster (filter (fn [[_mesh page-object]] (= (:id cell) (:id page-object)))
                                    (pages/body-meshes (:page-layer app))))))

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

(defn- screen-of [app ndc-x ndc-y]
  (let [{:keys [width height]} (camera/viewport app)]
    [(* 0.5 (+ 1 ndc-x) width) (* 0.5 (- 1 ndc-y) height)]))

(defn pick
  "What lies under a pointer position: in the grid, the code of the page
  in the cell there; in the cloud, a line of code, a page, a window, or
  a folder cloud, the nearest of pages and windows first."
  [app ndc-x ndc-y]
  (let [[x y] (screen-of app ndc-x ndc-y)
        raycaster (camera/pointer-ray app ndc-x ndc-y)]
    (if-let [placed (grid/cell-at app x y)]
      (cell-hit app raycaster placed)
      (when-not (grid/covers? app x y)
        (let [page (page-hit app raycaster)
              window (windows/hit (:windows app) raycaster)]
          (cond
            (and window (or (nil? page) (< (:distance window) (:distance page)))) window
            page page
            :else (folder-hit app)))))))

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

(defn- release-follow! [app]
  (when (get-in @(:state app) [:presence :following])
    (dispatch! app [:stop-following])))

(defn- note-interaction!
  "Records that the viewer took the camera, which ends an arrival from
  the editor, any pending reframing, and following an agent."
  [app]
  (swap! (:frame app) #(-> %
                           (assoc :last-interaction (js/performance.now) :user-moved? true)
                           (dissoc :arrival)
                           (assoc-in [:pointer :moved?] true)))
  (release-follow! app))

(defn- zoom-or-dive!
  "Zooms toward the pointer. Over code, zooming stops where the code
  reaches the editor's size, and further scrolling dives into the file."
  [app ^js event]
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

(defn- on-wheel
  "Scrolls a page in the grid, does nothing elsewhere over the grid, and
  zooms or dives over the cloud."
  [app ^js event]
  (.preventDefault event)
  (let [{:keys [x y]} (:pointer @(:frame app))
        placed (grid/cell-at app x y)]
    (cond
      (= :page (get-in placed [:cell :kind])) (grid/scroll-page! app (get-in placed [:cell :id]) (wheel-pixels event))
      (grid/covers? app x y) nil
      :else (zoom-or-dive! app event))))

;; Pointing into code

(defn- readable-code?
  "True when `hit` lands on code drawn as real text, large enough to point
  into."
  [{:keys [kind page]}]
  (let [least-line-pixels 8
        text-showing 0.5]
    (and (= :code kind)
         (>= (:pixels-per-unit page 0) least-line-pixels)
         (> (:text-share page 0) text-showing))))

(defn editable-code?
  "True when `hit` lands on readable code in a file the cloud can edit."
  [app {:keys [node origin] :as hit}]
  (and (readable-code? hit) (ui-state/editable? @(:state app) node (:path origin))))

(defn- editing-buffer [app]
  (let [{:keys [editing buffers]} @(:state app)]
    (get buffers (:path editing))))

(defn- buffer-under
  "The buffer of the file under `hit`, open or read from its source text,
  or nil while the text has not arrived."
  [app {:keys [id origin]}]
  (let [state @(:state app)
        path (:path origin)]
    (or (get-in state [:buffers path])
        (when-let [source-text (some #(when (= path (:path %)) (:text %)) (ui-state/source-of state id))]
          (assoc (editing-state/buffer source-text) :path path)))))

(defn- begin-editing-at!
  "Puts the caret into the code under `hit`, selecting a word or line as
  `select` says, and stops any camera flight so the code stays the size
  it was clicked at. True when editing began."
  [app {:keys [id page origin local] :as hit} select]
  (let [path (:path origin)
        buffer (buffer-under app hit)
        position (when buffer (editing-view/text-position page local buffer false))
        {:keys [columns rows]} (get-in page [:visual :geometry])]
    (when position
      (navigation/cancel-flight! (:controller app))
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

(defn- on-edited-page? [app {:keys [id page local]}]
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
  "Places the caret where the pointer went down, extending the selection
  with Shift, or picks a word on the second press and a line on the
  third; dragging extends it."
  [app {:keys [page local]} ^js event presses]
  (navigation/cancel-flight! (:controller app))
  (let [buffer (editing-buffer app)
        position (editing-view/text-position page local buffer true)]
    (case presses
      1 (dispatch! app [:edit :place position (.-shiftKey event)])
      2 (dispatch! app [:edit :select-word position])
      (dispatch! app [:edit :select-line position]))
    (swap! (:frame app) assoc :selecting true)
    (typing/focus! (:input app))))

(defn text-cursor?
  "True when the pointer over `hit` points into text, as it does over code
  a click can edit and over the page being edited."
  [app hit]
  (boolean (and hit (or (editable-code? app hit) (on-edited-page? app hit)))))

(defn- extend-selection! [app]
  (let [{:keys [ndc-x ndc-y]} (:pointer @(:frame app))
        {:keys [editing]} @(:state app)
        buffer (editing-buffer app)]
    (when-let [page-object (when buffer (pages/page-of (:page-layer app) (:namespace-id editing)))]
      (when-let [local (page-plane-local app page-object ndc-x ndc-y)]
        (dispatch! app [:edit :place (editing-view/text-position page-object local buffer true) true])))))

;; Carrying a page

(defn- carry-modifier?
  "True when the pointer goes down with the key that picks pages up, Ctrl,
  Cmd on macOS, or Alt."
  [^js event]
  (or (.-ctrlKey event) (.-metaKey event) (.-altKey event)))

(defn- jump-modifier?
  "True when the pointer goes down with Ctrl or Cmd, whose click on a
  name goes to its definition, as in the editor."
  [^js event]
  (and (or (.-ctrlKey event) (.-metaKey event)) (not (.-altKey event))))

(defn- jump-to-name!
  "Asks for the definition of the name under `hit`, a point in readable
  code, returning true when there was a name to ask about."
  [app {:keys [id node page origin local] :as hit}]
  (when-let [buffer (buffer-under app hit)]
    (let [position (editing-view/text-position page local buffer false)
          language (some #(when (= (:path origin) (:path %)) (:language %)) (:files node))
          [[line start] [_ end]] (editing-state/word-range buffer position language)
          written (subs (nth (:lines buffer) line) start end)
          {:keys [x y]} (:pointer @(:frame app))]
      (when (re-find #"[A-Za-z_]" written)
        (dispatch! app [:jump-to-name {:namespace-id id :name written :x x :y y}])
        true))))

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
  whether it did. With Ctrl or Cmd over readable code, a click that never
  moves goes to the definition of the name there instead."
  [app {:keys [id point] :as hit} ^js event]
  (let [lifted? (contains? (flights/lifted-ids app) id)]
    (when-let [center (and (not lifted?) (:position (pages/page-of (:page-layer app) id)))]
      (let [[point-x point-y point-z] point
            [center-x center-y center-z] center]
        (swap! (:frame app) assoc :carried
               {:id id
                :grab [(- point-x center-x) (- point-y center-y) (- point-z center-z)]
                :plane-anchor point
                :offset (get-in @(:state app) [:moved-pages id] [0 0 0])
                :start [(.-clientX event) (.-clientY event)]
                :jump (when (and (jump-modifier? event) (readable-code? hit)) hit)})
        true))))

(defn- carry-to-pointer!
  "Keeps the carried page's grab point under the pointer, measuring the
  offset from its layout position, and notes once the pointer has moved
  far enough to make the press a carry."
  [app ^js event]
  (let [{:keys [carried pointer]} @(:frame app)
        {:keys [id grab plane-anchor start]} carried
        [start-x start-y] start
        carry-threshold 4]
    (when (> (js/Math.hypot (- (.-clientX event) start-x) (- (.-clientY event) start-y)) carry-threshold)
      (swap! (:frame app) assoc-in [:carried :moved?] true))
    (when-let [[meeting-x meeting-y meeting-z] (plane-point app plane-anchor (:ndc-x pointer) (:ndc-y pointer))]
      (when-let [[anchor-x anchor-y anchor-z] (or (:layout-position (pages/page-of (:page-layer app) id))
                                                  (placement/anchor-of app id))]
        (let [[grab-x grab-y grab-z] grab]
          (swap! (:frame app) assoc-in [:carried :offset]
                 [(- meeting-x grab-x anchor-x) (- meeting-y grab-y anchor-y) (- meeting-z grab-z anchor-z)]))))))

(defn- set-down-carried! [app]
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
  or a drag that orbits or pans, or over a page in the grid, scrolls it.
  A press on the grid's gaps does nothing."
  [app ^js event]
  (update-pointer! app event)
  (let [{:keys [x y ndc-x ndc-y]} (:pointer @(:frame app))
        placed (grid/cell-at app x y)
        left? (zero? (.-button event))
        presses (when left? (count-press! app event))
        hit (when left?
              (if placed
                (cell-hit app (camera/pointer-ray app ndc-x ndc-y) placed)
                (page-hit app (camera/pointer-ray app ndc-x ndc-y))))]
    (swap! (:frame app) dissoc :arrival)
    (when-not (and (nil? placed) (grid/covers? app x y))
      (.setPointerCapture ^js (.-target event) (.-pointerId event))
      (cond
        (and hit (not (carry-modifier? event)) (on-edited-page? app hit))
        (start-selecting! app hit event presses)

        (and hit (nil? placed) (carry-modifier? event) (start-carry! app hit event))
        nil

        :else
        (swap! (:frame app) assoc :drag {:button (.-button event)
                                         :shift? (.-shiftKey event)
                                         :start [(.-clientX event) (.-clientY event)]
                                         :last [(.-clientX event) (.-clientY event)]
                                         :last-move (js/performance.now)
                                         :moved? false
                                         :scrolls (get-in placed [:cell :id])
                                         :jump (when (and hit (jump-modifier? event) (readable-code? hit)) hit)})))))

(defn- pass-to-window-cell!
  "Lets the window in the grid cell under the pointer take it, as an editor
  group takes the pointer in VS Code."
  [app]
  (let [{:keys [x y]} (:pointer @(:frame app))
        {:keys [cell body]} (grid/cell-at app x y)]
    (when (and (= :window (:kind cell))
               (<= (:top body) y))
      (windows/activate! (:windows app) (:id cell)))))

(defn- on-pointer-move [app ^js event]
  (update-pointer! app event)
  (cond
    (:selecting @(:frame app)) (extend-selection! app)
    (:carried @(:frame app)) (carry-to-pointer! app event)
    (nil? (:drag @(:frame app))) (pass-to-window-cell! app)
    :else
    (when-let [{:keys [button shift? start scrolls] previous :last :as drag} (:drag @(:frame app))]
      (let [[start-x start-y] start
            [last-x last-y] previous
            delta-x (- (.-clientX event) last-x)
            delta-y (- (.-clientY event) last-y)
            drag-threshold 4
            moved? (or (:moved? drag)
                       (> (js/Math.hypot (- (.-clientX event) start-x) (- (.-clientY event) start-y)) drag-threshold))]
        (when moved?
          (release-follow! app)
          (cond
            scrolls (grid/scroll-page! app scrolls (- delta-y))
            (or (= 2 button) (= 1 button) shift?) (navigation/pan! (:controller app) delta-x delta-y (camera/viewport-height app))
            :else (navigation/orbit! (:controller app) delta-x delta-y (camera/viewport-height app))))
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
    :window {:kind :window :id (:id hit) :x x :y y}
    {:kind :sky :x x :y y}))

(defn- click-event [hit]
  (case (:kind hit)
    (:code :namespace) [:click-namespace (:id hit)]
    :folder [:click-folder (:path hit)]
    [:click-empty]))

(defn- on-pointer-up
  "Ends a selection, drag, or carry, or clicks. A click on editable code
  places the caret at once, or with Ctrl or Cmd goes to a definition;
  other clicks wait so a double-click's first half lifts nothing."
  [app ^js _event]
  (cond
    (:selecting @(:frame app)) (swap! (:frame app) dissoc :selecting)
    (and (get-in @(:frame app) [:carried :jump]) (not (get-in @(:frame app) [:carried :moved?])))
    (let [hit (get-in @(:frame app) [:carried :jump])]
      (swap! (:frame app) dissoc :carried)
      (jump-to-name! app hit))
    (:carried @(:frame app)) (set-down-carried! app)
    :else
    (when-let [{:keys [moved? last-move button jump]} (:drag @(:frame app))]
      (swap! (:frame app) assoc :drag nil)
      (let [pause-limit 90]
        (navigation/release-drag! (:controller app) (> (- (js/performance.now) last-move) pause-limit)))
      (when-not moved?
        (let [{:keys [ndc-x ndc-y] :as pointer} (:pointer @(:frame app))
              hit (pick app ndc-x ndc-y)
              double-click-window 240]
          (case button
            0 (when-not (or (and jump (jump-to-name! app jump))
                            (and (editable-code? app hit) (begin-editing-at! app hit nil)))
                (let [click-id (js/performance.now)]
                  (swap! (:frame app) assoc :pending-click click-id)
                  (js/setTimeout (fn []
                                   (when (= click-id (:pending-click @(:frame app)))
                                     (swap! (:frame app) dissoc :pending-click)
                                     ;; A click on a window lets it take the pointer.
                                     (if (= :window (:kind hit))
                                       (windows/activate! (:windows app) (:id hit))
                                       (dispatch! app (click-event hit)))))
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
    (when-not (or (and (contains? #{:code :namespace} (:kind hit)) (on-edited-page? app hit))
                  (let [{:keys [x y]} (:pointer @(:frame app))] (grid/covers? app x y)))
      (case (:kind hit)
        (:code :namespace) (dispatch! app [:double-click-namespace (:id hit)])
        :folder (dispatch! app [:frame-directory (:path hit)])
        :window (do (dispatch! app [:fly-to-window (:id hit)])
                    (windows/activate! (:windows app) (:id hit)))
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
                    "Escape" (do (windows/deactivate! (:windows app))
                                 (dispatch! app [:escape]))
                    "Home" (dispatch! app [:frame-all])
                    "ArrowLeft" (dispatch! app [:step-history -1])
                    "ArrowRight" (dispatch! app [:step-history 1])
                    " " (dispatch! app [:toggle-playback])
                    ("n" "N") (dispatch! app [:toggle-display-mode])
                    ("a" "A") (dispatch! app [:toggle-arrange-panel])
                    ("r" "R") (dispatch! app [:toggle-repl])
                    ("q" "Q") (dispatch! app [:toggle-assistant])
                    ("g" "G") (dispatch! app [:toggle-grid])
                    ("t" "T") (dispatch! app [:toggle-auto-hide-bars])
                    ("c" "C") (dispatch! app [:pick-command-for-page-in-hand])
                    "?" (dispatch! app [:toggle-legend])
                    "[" (dispatch! app [:step-spread -1])
                    "]" (dispatch! app [:step-spread 1])
                    ("+" "=") (navigation/zoom-about! controller (navigation/target-position controller) zoom-in-factor)
                    "-" (navigation/zoom-about! controller (navigation/target-position controller) zoom-out-factor)
                    ::unhandled)]
      (when-not (= ::unhandled handled)
        (.preventDefault event)))))

(defn- page-command-keys?
  "True for Ctrl+Alt+R, or Cmd+Option+R on macOS, which offer the commands
  to run for the page in hand."
  [^js event]
  (and (= "KeyR" (.-code event)) (.-altKey event) (not (.-shiftKey event)) (not (.-repeat event))
       (if (typing/mac?)
         (and (.-metaKey event) (not (.-ctrlKey event)))
         (and (.-ctrlKey event) (not (.-metaKey event))))))

(defn- on-page-command-keys
  "Takes the page command keys before anything else in the cloud sees
  them, VS Code included, so neither a pane holding the keyboard nor a
  keybinding another extension starts with the same keys keeps them from
  the cloud."
  [app ^js event]
  (when (page-command-keys? event)
    (.preventDefault event)
    (.stopPropagation event)
    (dispatch! app [:pick-command-for-page-in-hand])))

(defn attach!
  "Listens for input on the cloud's canvas and the window."
  [app]
  (let [^js canvas (.-domElement ^js (:renderer app))]
    (.addEventListener canvas "wheel" #(on-wheel app %) #js {:passive false})
    (.addEventListener canvas "pointerdown" #(on-pointer-down app %))
    ;; While a file is edited, a press anywhere on the canvas, whether it
    ;; places the caret, orbits, or lands off the code, leaves the keyboard
    ;; with the text area, which the canvas would otherwise take as the
    ;; press's default.
    (.addEventListener canvas "mousedown" (fn [^js event]
                                           (when (:editing @(:state app))
                                             (.preventDefault event))))
    (.addEventListener canvas "pointermove" #(on-pointer-move app %))
    (.addEventListener canvas "pointerup" #(on-pointer-up app %))
    (.addEventListener canvas "pointercancel" #(swap! (:frame app) dissoc :drag :carried))
    (.addEventListener canvas "pointerleave" #(swap! (:frame app) assoc-in [:pointer :inside?] false))
    (.addEventListener canvas "pointerenter" #(update-pointer! app %))
    (.addEventListener canvas "dblclick" #(on-double-click app %))
    (.addEventListener canvas "contextmenu" #(.preventDefault ^js %))
    (.addEventListener js/window "keydown" #(on-page-command-keys app %) true)
    (.addEventListener js/window "keydown" #(on-key-down app %))))

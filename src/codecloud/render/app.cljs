(ns codecloud.render.app
  "Wires the interface state to the three.js scene, overlay, input, and
  frame loop. The transport offers :send!, :subscribe!, :view-state, and
  :save-view-state!."
  (:require ["three" :as three]
            [codecloud.presentation.arrangement :as arrangement]
            [codecloud.presentation.layers :as layers]
            [codecloud.presentation.palette :as palette]
            [codecloud.presentation.scene :as scene]
            [codecloud.presentation.sky :as sky]
            [codecloud.presentation.ui-state :as ui-state]
            [codecloud.presentation.zoom :as zoom]
            [codecloud.protocol :as protocol]
            [codecloud.render.camera :as camera]
            [codecloud.render.edges :as edges]
            [codecloud.render.editing :as editing]
            [codecloud.render.flights :as flights]
            [codecloud.render.folders :as folders]
            [codecloud.render.gestures :as gestures]
            [codecloud.render.hud :as hud]
            [codecloud.render.labels :as labels]
            [codecloud.render.navigation :as navigation]
            [codecloud.render.overlay :as overlay]
            [codecloud.render.pages :as pages]
            [codecloud.render.placement :as placement]
            [codecloud.render.sky :as sky-layer]
            [codecloud.render.text :as text]
            [codecloud.render.typing :as typing]))

(defn- reduced-motion? []
  (.-matches (js/window.matchMedia "(prefers-reduced-motion: reduce)")))

(defn- local-hour []
  (let [now (js/Date.)]
    (+ (.getHours now) (/ (.getMinutes now) 60))))

;; Keeping the scene in line with the state

(declare run-effect!)

(def ^:private look-inputs
  "State that changes how pages look without moving them."
  (juxt :focused-id :lifted-folder :hovered-id (comp (juxt :open? :query) :search)
        :page-colors :folder-hues :active-layers :dirty-paths))

(defn- editing-moved?
  "True when the caret, the selection, or the text being edited changed."
  [before after]
  (let [path (get-in after [:editing :path])
        before-buffer (get-in before [:buffers path])
        after-buffer (get-in after [:buffers path])]
    (and after-buffer
         (or (not= path (get-in before [:editing :path]))
             (not (identical? (:lines before-buffer) (:lines after-buffer)))
             (not= (:caret before-buffer) (:caret after-buffer))
             (not= (:anchor before-buffer) (:anchor after-buffer))))))

(defn- sync-view!
  "Brings the scene and overlay in line with a state change, rebuilding
  page bases only when the shown pages, their arrangement, or the font
  change. Pages with edited files take their buffers' shape. While a file
  is edited, a new snapshot of the same pages leaves the layout alone
  until editing stops, so pages hold still under the caret."
  [app before after]
  (let [frame (:frame app)
        shown (:visible-snapshot after)
        character-width (:character-width @frame)
        width-changed? (not= character-width (:visuals-character-width @frame))
        shown-changed? (not (identical? (:visible-snapshot before) shown))
        arrangement-changed? (not= (:arrangement before) (:arrangement after))
        layout-changed? (or shown-changed? width-changed? arrangement-changed? (nil? (:bases @frame)))
        look-changed? (or layout-changed? (not= (look-inputs before) (look-inputs after)))
        buffers-changed? (or (not (identical? (:buffers before) (:buffers after)))
                             (not= (:editing before) (:editing after)))]
    (when (and (or look-changed? buffers-changed?) shown)
      (let [previous-visuals (:visuals @frame)
            visible-ids (ui-state/visible-ids after)
            bases (if layout-changed?
                    (scene/page-bases shown (:arrangement after) visible-ids character-width)
                    (:bases @frame))
            plain-visuals (if look-changed?
                            (scene/page-visuals shown after bases)
                            (:plain-visuals @frame))
            visuals (scene/shaped-by-buffers plain-visuals after character-width)
            held? (and (:editing after) shown-changed? (not width-changed?) (not arrangement-changed?)
                       (= visible-ids (:placed-ids @frame)))
            place! (fn [rebuild?]
                     (placement/sync! app {:visuals visuals
                                           :previous-visuals previous-visuals
                                           :snapshot shown
                                           :visible-ids visible-ids
                                           :arrangement-key (:arrangement after)
                                           :rebuild? rebuild?})
                     (swap! frame assoc :placed-ids visible-ids :placement-held? false))]
        (swap! frame assoc
               :bases bases
               :plain-visuals plain-visuals
               :visuals visuals
               :directory-visuals (scene/directory-visuals visuals (:cloud-name after))
               :visuals-character-width character-width)
        (cond
          held? (swap! frame assoc :placement-held? true)
          layout-changed? (place! (or shown-changed? width-changed?))
          (and (:placement-held? @frame) (not (:editing after))) (place! true))
        (pages/sync-pages! (:page-layer app) visuals)
        (when look-changed?
          (edges/sync-edges! (:edge-layer app) (scene/edge-visuals shown after visible-ids)))))
    (when (not= (:spread before) (:spread after))
      (placement/aim-spacing! app (:spread after)))
    (when (editing-moved? before after)
      (swap! frame assoc :caret-changed-at (js/performance.now)))
    (when-not (identical? before after)
      (typing/sync-input! (:input app) after)
      (editing/forget-settled! app (:buffers after))
      (hud/update-hud! (:hud app) after))))

(defn dispatch!
  "Applies an event to the interface state, updates the view, and carries
  out the resulting effects."
  [app event]
  (let [before @(:state app)
        {:keys [state effects]} (ui-state/handle before event)]
    (reset! (:state app) state)
    (sync-view! app before state)
    (doseq [effect effects]
      (run-effect! app effect))))

(defn- run-effect! [app [effect-kind payload]]
  (when (contains? #{:fly-to :fly-to-line :frame-all :frame-directory :arrive} effect-kind)
    (swap! (:frame app) assoc :user-moved? true))
  (case effect-kind
    :send ((get-in app [:transport :send!]) payload)
    :lift (flights/lift-page! app payload)
    :set-down (flights/set-down! app :page)
    :lift-folder (flights/lift-folder! app payload)
    :set-down-folder (flights/set-down! app :folder)
    :fly-to (flights/fly-to-page! app payload)
    :fly-to-line (flights/fly-to-line! app payload 0.85 1.1)
    :frame-all (do (flights/set-down-everything! app)
                   (flights/frame-everything! app 1.0))
    :frame-directory (flights/frame-directory! app payload)
    :refit (flights/refit-later! app)
    :arrive (flights/arrive! app payload)
    :return-from-dive (flights/return-from-dive! app)
    nil))

;; Source text

(defn- source-keys-of
  "Source keys of every namespace in `snapshot`, kept while the snapshot
  shows, so frames look text up without building keys again."
  [app snapshot]
  (let [{:keys [source-keys-for source-keys]} @(:frame app)]
    (if (identical? source-keys-for snapshot)
      source-keys
      (let [computed (into {}
                           (map (fn [namespace-id] [namespace-id (ui-state/source-key snapshot namespace-id)]))
                           (keys (:namespaces snapshot)))]
        (swap! (:frame app) assoc :source-keys-for snapshot :source-keys computed)
        computed))))

(defn- source-lookup
  "Function from namespace id to its source in the shown snapshot when the
  text is here, as {:source-key :files}, or for a page with edited files
  in the working tree, {:source-key :lines} holding the edited text."
  [app]
  (let [{:keys [snapshot sources buffers]} @(:state app)
        source-keys (source-keys-of app snapshot)
        edited? (and (seq buffers) (= :working-tree (:id snapshot)))]
    (fn [namespace-id]
      (when-let [source-key (get source-keys namespace-id)]
        (let [files (get sources source-key)]
          (or (when edited?
                (editing/edited-source app namespace-id (get-in snapshot [:namespaces namespace-id]) files buffers))
              (when files
                {:source-key source-key :files files})))))))

(defn- request-wanted-sources!
  "Asks for the text of pages that want it, a batch at a time."
  [app now wanted-ids]
  (let [request-interval 150]
    (when (and (seq wanted-ids) (> (- now (:last-source-request @(:frame app) 0)) request-interval))
      (swap! (:frame app) assoc :last-source-request now)
      (dispatch! app [:need-sources wanted-ids]))))

;; Each frame

(defn- folder-cloud-presence!
  "Eases the folder clouds in while the arrangement gathers folders and
  out while it does not, returning how present they are this frame."
  [app shown? elapsed]
  (let [wanted (if shown? 1 0)
        current (:folder-presence @(:frame app) wanted)
        fade-rate 4
        presence (+ current (* (- wanted current) (- 1 (js/Math.exp (- (* fade-rate elapsed))))))]
    (swap! (:frame app) assoc :folder-presence presence)
    presence))

(defn- folder-members
  "Folders with their pages as drawn this frame. Moved or carried pages
  float outside their folder's cloud."
  [app]
  (let [{:keys [directory-visuals visuals carried]} @(:frame app)
        moved-ids (cond-> (set (keys (:moved-pages @(:state app))))
                    carried (conj (:id carried)))
        page-layer (:page-layer app)]
    (into {}
          (map (fn [[path {:keys [member-ids emphasis hue]}]]
                 [path {:emphasis emphasis
                        :hue hue
                        :members (keep (fn [id]
                                         (when-let [page-object (when-not (contains? moved-ids id)
                                                                  (pages/page-of page-layer id))]
                                           (when-let [position (:position page-object)]
                                             {:id id
                                              :position position
                                              :radius (scene/layout-radius (get-in visuals [id :geometry]))
                                              :presence (:appearance page-object)})))
                                       member-ids)}]))
          directory-visuals)))

(defn- edge-ends
  "Function from namespace id to where its page's edges start this frame:
  {:position :radius :presence :color}, worked out once per page."
  [app display-mode]
  (let [found (volatile! {})]
    (fn [namespace-id]
      (if-let [[_ ends] (find @found namespace-id)]
        ends
        (let [ends (when-let [page-object (pages/page-of (:page-layer app) namespace-id)]
                     (when-let [position (:position page-object)]
                       (let [{:keys [width height]} (pages/shape page-object display-mode)]
                         {:position position
                          :radius (* 0.5 (min width height))
                          :presence (min 1 (* 1.4 (:opacity page-object 0)))
                          :color (:accent page-object)})))]
          (vswap! found assoc namespace-id ends)
          ends)))))

(defn- maybe-tick-playback! [app now]
  (let [state @(:state app)
        tick-interval 900]
    (when (and (:playing? state) (not (ui-state/awaiting-snapshot? state)))
      (let [due (:playback-due @(:frame app))]
        (cond
          (nil? due) (swap! (:frame app) assoc :playback-due (+ now tick-interval))
          (>= now due) (do (swap! (:frame app) assoc :playback-due nil)
                           (dispatch! app [:playback-tick])))))))

(defn- save-view-state!
  "Remembers the camera every few seconds when it has moved, so reopening
  the cloud returns to the same view."
  [app now]
  (let [save-interval 2500
        {:keys [last-view-save saved-view]} @(:frame app)]
    (when (> (- now (or last-view-save 0)) save-interval)
      (swap! (:frame app) assoc :last-view-save now)
      (when-let [save! (get-in app [:transport :save-view-state!])]
        (let [{:keys [^js spherical] :as controller} (:controller app)
              view {:target (navigation/target-position controller)
                    :distance (.-radius spherical)
                    :polar (.-phi spherical)
                    :azimuth (.-theta spherical)}]
          (when (not= view saved-view)
            (swap! (:frame app) assoc :saved-view view)
            (save! view)))))))

(defn- leak-dive! [app now]
  (let [{:keys [pressure last-push]} (:dive @(:frame app))]
    (when (pos? (or pressure 0))
      (swap! (:frame app) update :dive assoc
             :pressure (zoom/leak-dive pressure (/ (- now (or last-push now)) 1000))
             :last-push now))))

(defn- advance-clock!
  "Advances the clock and sky, returning {:elapsed :time :motion :night
  :ink :theme-key}: the sky's night amount, the ink amount pages and panels
  follow, and the theme text is drawn in. Reduced motion stops time and
  turns the sky at once."
  [app now]
  (let [frame @(:frame app)
        state @(:state app)
        longest-step 0.1
        elapsed (min longest-step (/ (- now (or (:last-time frame) now)) 1000))
        motion (if (:reduced-motion? frame) 0 1)
        time (+ (:time frame 0) (* elapsed motion))
        target-night (sky/night-amount (:sky-mode state) (:theme-kind state) (local-hour))
        night (if (:reduced-motion? frame) target-night (sky/approach (:night frame) target-night elapsed))
        ink (sky/ink-amount night)
        theme-key (palette/theme-key ink)]
    (swap! (:frame app) assoc :last-time now :time time :night night)
    (when (not= theme-key (:theme-key frame))
      (swap! (:frame app) assoc :theme-key theme-key)
      (hud/apply-theme! (:hud app) theme-key))
    {:elapsed elapsed :time time :motion motion :night night :ink ink :theme-key theme-key}))

(defn- draw-pages!
  "Places, fades, and draws every page, and asks the host for the source
  text that pages want."
  [app now {:keys [elapsed time motion ink theme-key]}]
  (let [state @(:state app)
        {:keys [forward right up]} (camera/basis app)
        wanted-ids (pages/update-pages! (:page-layer app)
                                        {:camera (:camera app)
                                         :forward forward
                                         :right right
                                         :up up
                                         :positions (placement/targets app)
                                         :offset-of (flights/offset-function app)
                                         :carried-id (get-in @(:frame app) [:carried :id])
                                         :viewport (camera/viewport app)
                                         :field-of-view navigation/field-of-view
                                         :ink ink
                                         :time time
                                         :elapsed elapsed
                                         :motion motion
                                         :theme-name theme-key
                                         :display-mode (:display-mode state)
                                         :editor-font (camera/editor-font state)
                                         :code-colors (ui-state/shown-code-colors state)
                                         :device-pixel-ratio (.getPixelRatio ^js (:renderer app))
                                         :source-of (source-lookup app)
                                         :hovered-id (:hovered-id state)
                                         :focus-distance (navigation/distance (:controller app))
                                         :lift-depth (flights/lift-depth app)
                                         :lifted-ids (flights/lifted-ids app)
                                         :examined-id (:examined-id @(:frame app))})]
    (request-wanted-sources! app now wanted-ids)))

(defn- draw-folder-clouds!
  "Gathers each folder's pages in a cloud while the arrangement keeps
  folders together, and remembers where the clouds fall on screen."
  [app {:keys [elapsed time motion night]}]
  (let [{:keys [arrangement active-layers tint-clouds?]} @(:state app)
        folder-clouds? (arrangement/shows-folder-clouds? arrangement)
        presence (folder-cloud-presence! app folder-clouds? elapsed)
        least-presence 0.01
        extents (folders/update-folders! (:folder-layer app)
                                         {:folders (if (> presence least-presence) (folder-members app) {})
                                          :tint (layers/cloud-tint active-layers tint-clouds?)
                                          :presence presence
                                          :night night
                                          :time time
                                          :motion motion})]
    (swap! (:frame app) assoc :folder-screens (if folder-clouds? (overlay/folder-screens app extents) {}))))

(defn- draw-edges-and-sky!
  "Moves the dependency edges with their pages and turns the sky with the
  camera."
  [app {:keys [elapsed time motion night ink]}]
  (let [{:keys [width height]} (camera/viewport app)
        ^js camera (:camera app)]
    (edges/update-edges! (:edge-layer app)
                         {:ends-of (edge-ends app (:display-mode @(:state app)))
                          :ink ink
                          :camera camera
                          :viewport-width width
                          :viewport-height height
                          :pixel-ratio (.getPixelRatio ^js (:renderer app))
                          :time time
                          :motion motion})
    (sky-layer/update-sky! (:sky app) camera {:time time :night night :dusk (sky/dusk-amount night)
                                              :motion motion :elapsed elapsed})))

(defn- update-editing!
  "Draws the caret and the selection on the page being edited and keeps
  the keyboard's text area beside the caret."
  [app now theme-key]
  (let [{:keys [editing buffers]} @(:state app)
        buffer (get buffers (:path editing))
        page-object (when editing (pages/page-of (:page-layer app) (:namespace-id editing)))]
    (editing/update-caret! (:caret-layer app)
                           {:page-object page-object
                            :buffer buffer
                            :theme-name theme-key
                            :code-colors (ui-state/shown-code-colors @(:state app))
                            :focused? (typing/focused? (:input app))
                            :since-change (- now (:caret-changed-at @(:frame app) 0))
                            :viewport (camera/viewport app)})
    (when-let [point (and page-object buffer (editing/caret-screen-point page-object buffer))]
      (typing/place! (:input app) point (:pixels-per-unit page-object)))))

(defn- update-overlay!
  "Refreshes what floats over the scene, which is the hovered thing, the
  labels, the caret, the tooltip, and the dive ring."
  [app now theme-key]
  (when (or (navigation/flying? (:controller app)) (placement/spacing-moving? app))
    (swap! (:frame app) assoc-in [:pointer :moved?] true))
  (overlay/refresh-hover! app)
  (labels/update-labels! (:labels app) (overlay/label-candidates app))
  (update-editing! app now theme-key)
  (let [{:keys [editing menu]} @(:state app)
        {:keys [pointer dive]} @(:frame app)]
    (hud/update-pointer-overlays! (:hud app) {:pointer [(:x pointer 0) (:y pointer 0)]
                                              :hover (overlay/shown-hover app now)
                                              :dive-pressure (:pressure dive 0)
                                              :editing? (some? editing)
                                              :menu-open? (some? menu)})))

(defn- render! [app]
  (let [^js renderer (:renderer app)
        {sky-scene :scene sky-camera :camera} (:sky app)]
    (.clear renderer)
    (.render renderer sky-scene sky-camera)
    (.render renderer (:scene app) (:camera app))))

(defn- draw-frame! [app now]
  (let [clock (advance-clock! app now)]
    (placement/advance! app (:elapsed clock))
    (flights/first-framing! app)
    (navigation/update! (:controller app) (:elapsed clock))
    (draw-pages! app now clock)
    (draw-folder-clouds! app clock)
    (draw-edges-and-sky! app clock)
    (leak-dive! app now)
    (flights/frame-when-settled! app)
    (flights/advance-arrival! app now)
    (maybe-tick-playback! app now)
    (update-overlay! app now (:theme-key clock))
    (save-view-state! app now)
    (render! app)))

(defn- report-frame-error!
  "Logs a failed frame to the console, and to the host once per distinct
  message."
  [app error]
  (let [message (str "A frame of the cloud failed: " (or (.-message error) error))]
    (js/console.error error)
    (when-not (contains? (:reported-errors @(:frame app)) message)
      (swap! (:frame app) update :reported-errors (fnil conj #{}) message)
      ((get-in app [:transport :send!]) (protocol/message :view/log {:message message})))))

(defn- frame!
  "Draws one frame and asks for the next, even when this one failed, so
  one bad frame never stops the cloud."
  [app now]
  (try
    (draw-frame! app now)
    (catch :default error
      (report-frame-error! app error)))
  (js/requestAnimationFrame #(frame! app %)))

;; Starting

(defn- resize! [app]
  (let [^js container (:container app)
        width (max 1 (.-clientWidth container))
        height (max 1 (.-clientHeight container))
        ^js camera (:camera app)
        sharpest-pixel-ratio 2]
    (.setPixelRatio ^js (:renderer app) (min sharpest-pixel-ratio (or js/window.devicePixelRatio 1)))
    (.setSize ^js (:renderer app) width height false)
    (set! (.-aspect camera) (/ width height))
    (.updateProjectionMatrix camera)
    (swap! (:frame app) assoc :viewport {:width width :height height})))

(defn- restore-view! [app view-state]
  (when-let [{:keys [target distance polar azimuth]} view-state]
    (let [{:keys [^js spherical] :as controller} (:controller app)]
      (navigation/jump-to! controller target distance)
      (set! (.-phi spherical) polar)
      (set! (.-theta spherical) azimuth)
      (swap! (:frame app) assoc :restored-view? true :saved-view view-state))))

(defn- measure-editor-font!
  "Measures the editor font's character width once the font is ready, and
  relays out the pages when it changed."
  [app]
  (let [width (text/character-width (camera/editor-font @(:state app)))]
    (when (not= width (:character-width @(:frame app)))
      (swap! (:frame app) assoc :character-width width)
      (let [state @(:state app)]
        (sync-view! app state state)))))

(defn- settle-sky!
  "Shows the sky that the host's first settings ask for at once, with no
  easing from the sky shown before they arrived."
  [app]
  (let [state @(:state app)
        night (sky/night-amount (:sky-mode state) (:theme-kind state) (local-hour))
        theme-key (palette/theme-key (sky/ink-amount night))]
    (swap! (:frame app) assoc :night night :theme-key theme-key :sky-settled? true)
    (hud/apply-theme! (:hud app) theme-key)))

(defn- on-host-message! [app message]
  (dispatch! app [:host message])
  (when (= :host/init (:type message))
    (when-not (:sky-settled? @(:frame app))
      (settle-sky! app))
    (let [{:keys [size family]} (camera/editor-font @(:state app))
          measure! #(measure-editor-font! app)]
      (-> (.load (.-fonts js/document) (str size "px " (text/code-font-family family)))
          (.then measure! measure!)))))

(defn- first-guess-night
  "Night amount to show before the host says anything. VS Code marks a
  webview's body with the kind of the editor's theme."
  []
  (if (some-> js/document .-body .-classList (.contains "vscode-light")) 0.0 1.0))

(defn- create-renderer
  "A WebGL renderer drawing into `container`, which clears only when told
  to, since the sky and the cloud are drawn in two passes."
  [^js container]
  (let [^js renderer (three/WebGLRenderer. #js {:antialias true :alpha false :powerPreference "high-performance"})]
    (set! (.-autoClear renderer) false)
    (set! (.-outputColorSpace renderer) three/SRGBColorSpace)
    (set! (.-tabIndex (.-domElement renderer)) 0)
    (.appendChild container (.-domElement renderer))
    renderer))

(defn- initial-frame
  "What the frame loop knows before the first frame, showing the sky for
  `night`."
  [night]
  {:night night
   :theme-key (palette/theme-key (sky/ink-amount night))
   :character-width 0.5
   :lifts {}
   :pointer {:x 0 :y 0 :ndc-x 0 :ndc-y 0 :inside? false :moved? false}
   :dive {:pressure 0}
   :reduced-motion? (reduced-motion?)})

(defn- start-frames!
  "Starts the frame loop once the labels' font has loaded, or failed to,
  so the first labels are measured in the right font."
  [app]
  (let [begin! #(js/requestAnimationFrame (fn [now] (frame! app now)))]
    (-> (.load (.-fonts js/document) (str "700 16px " text/label-font-family))
        (.then begin! begin!))))

(defn start!
  "Starts the cloud view in `container`, talking to the host through
  `transport`. Returns the app map."
  [^js container transport]
  (let [renderer (create-renderer container)
        near-plane 0.5
        far-plane 2000000
        ^js camera (three/PerspectiveCamera. navigation/field-of-view 1 near-plane far-plane)
        ^js scene (three/Scene.)
        app-holder (atom nil)
        dispatch-to-app! (fn [event] (dispatch! @app-holder event))
        hud-instance (hud/create-hud container dispatch-to-app!)
        frame (initial-frame (first-guess-night))
        app {:container container
             :transport transport
             :renderer renderer
             :camera camera
             :scene scene
             :dispatch! dispatch-to-app!
             :sky (sky-layer/create-sky)
             :folder-layer (folders/create-layer scene)
             :edge-layer (edges/create-layer scene)
             :page-layer (pages/create-layer scene)
             :labels (labels/create-labels (:root hud-instance))
             :caret-layer (editing/create-layer)
             :input (typing/create-input container dispatch-to-app!
                                         #(let [{:keys [editing buffers]} @(:state @app-holder)]
                                            (get buffers (:path editing))))
             :controller (navigation/create-controller camera)
             :hud hud-instance
             :state (atom ui-state/initial-state)
             :frame (atom frame)}]
    (reset! app-holder app)
    (hud/apply-theme! hud-instance (:theme-key frame))
    (resize! app)
    (.observe (js/ResizeObserver. #(resize! app)) container)
    (gestures/attach! app)
    (restore-view! app (:view-state transport))
    ((:subscribe! transport) #(on-host-message! app %))
    (hud/update-hud! hud-instance @(:state app))
    (start-frames! app)
    (dispatch! app [:ready])
    app))

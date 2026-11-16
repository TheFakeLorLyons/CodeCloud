(ns codecloud.render.windows
  "Windows beside the cloud. Each is a live element in a layer below the
  canvas, placed in 3D by three.js's CSS3DRenderer and turned to face the
  camera as pages are, and seen through a hole the canvas leaves where it
  stands, so pages in front of it still cover it. A window takes the
  pointer once clicked and gives it back when the pointer leaves it. In
  a grid cell, a window stands flat over the cell's body."
  (:require ["three" :as three]
            ["three/examples/jsm/renderers/CSS3DRenderer.js" :refer [CSS3DObject CSS3DRenderer]]
            [codecloud.presentation.markdown :as markdown]
            [codecloud.presentation.windows-state :as windows-state]
            [codecloud.presentation.zoom :as zoom]
            [codecloud.render.camera :as camera]
            [codecloud.render.dom :as dom]
            [codecloud.render.grid :as grid]
            [codecloud.render.icons :as icons]
            [codecloud.render.navigation :as navigation]
            [codecloud.render.pdf-view :as pdf-view]
            [codecloud.render.placement :as placement]
            [codecloud.render.planes :as planes]))

;; Taking the pointer

(defn- pass-pointer! [{:keys [^js canvas]} through?]
  (set! (.. canvas -style -pointerEvents) (if through? "none" "")))

(defn deactivate!
  "Gives the pointer back to the cloud."
  [{:keys [windows active] :as layer}]
  (when-let [{:keys [element]} (get @windows @active)]
    (dom/toggle-class! element "is-active" false))
  (reset! active nil)
  (pass-pointer! layer false))

(defn activate!
  "Lets window `window-id` take the pointer until the pointer moves away
  from it."
  [{:keys [windows active] :as layer} window-id]
  (when-let [{:keys [element]} (get @windows window-id)]
    (when (not= window-id @active)
      (deactivate! layer)
      (reset! active window-id)
      (dom/toggle-class! element "is-active" true))
    (pass-pointer! layer true)))

(defn- follow-pointer-away!
  "Gives the pointer back to the cloud once it moves over anything of the
  page outside the active window. Moves inside a window's frame stay in
  the frame's own document, so the page hears only moves outside it. The
  canvas hears moves only until a window takes the pointer, so the move
  that hands the pointer over leaves the window active."
  [{:keys [windows active drag ^js canvas] :as layer}]
  (.addEventListener js/document "pointermove"
                     (fn [^js event]
                       (when-let [{:keys [^js element]} (get @windows @active)]
                         (when (and (nil? @drag)
                                    (not (identical? canvas (.-target event)))
                                    (not (.contains element (.-target event))))
                           (deactivate! layer))))))

(defn create-layer
  "The layer of windows, its elements put in `container` below `canvas`,
  and the holes the canvas leaves for them in `scene`."
  [^js container ^js scene ^js canvas dispatch]
  (let [^js css-renderer (CSS3DRenderer.)
        ^js root (.-domElement css-renderer)
        layer {:css-renderer css-renderer
               :css-scene (three/Scene.)
               :scene scene
               :canvas canvas
               :dispatch dispatch
               :app (atom nil)
               :windows (atom {})
               :active (atom nil)
               :drag (atom nil)
               :homes (atom nil)
               :pending-flight (atom nil)}]
    (.add (.-classList root) "window-layer")
    (.insertBefore container root canvas)
    (follow-pointer-away! layer)
    layer))

(defn resize! [{:keys [^js css-renderer]} width height]
  (.setSize css-renderer width height))

;; Moving a window by its bar

(defn- world-per-pixel-at
  "World units a CSS pixel spans at world point `position`."
  [app position]
  (let [{:keys [^js forward]} (camera/basis app)
        depth (max 1 (camera/depth-along (:camera app) forward position))]
    (/ 1 (zoom/pixels-per-unit depth navigation/field-of-view (camera/viewport-height app)))))

(defn- dragged-offset
  "The offset a drag from `start` with `offset` reaches at screen point
  [x y], moving the window across the screen as the pointer moves."
  [app {:keys [start offset position]} x y]
  (let [{:keys [^js right ^js up]} (camera/basis app)
        [start-x start-y] start
        span (world-per-pixel-at app position)
        across (* span (- x start-x))
        rise (* span (- start-y y))]
    (mapv + (or offset [0 0 0])
          [(+ (* across (.-x right)) (* rise (.-x up)))
           (+ (* across (.-y right)) (* rise (.-y up)))
           (+ (* across (.-z right)) (* rise (.-z up)))])))

(defn- follow-bar-drags!
  "Lets the window's bar carry the window while the window is in the cloud."
  [{:keys [app drag dispatch windows]} window-id ^js bar]
  (.addEventListener bar "pointerdown"
                     (fn [^js event]
                       (let [{:keys [pinned? position offset]} (get @windows window-id)
                             [start-x start-y :as start] [(.-clientX event) (.-clientY event)]]
                         (when (and (zero? (.-button event)) (not pinned?) position
                                    (not (.closest (.-target event) "button, input, form")))
                           (reset! drag {:id window-id :start start :offset offset :position position})
                           (dom/follow-drag! event
                                             (fn [dx dy]
                                               (when-let [shown-app @app]
                                                 (swap! drag assoc :moved-to
                                                        (dragged-offset shown-app @drag (+ start-x dx) (+ start-y dy)))))
                                             (fn []
                                               (let [{:keys [moved-to]} @drag]
                                                 (reset! drag nil)
                                                 (when moved-to
                                                   (dispatch [:move-window window-id moved-to]))))))))))

;; Elements

(defn- kind-icon [kind]
  (case kind
    (:web :html) icons/web
    :image icons/image
    icons/document))

(defn- window-element [dispatch {:keys [id kind]}]
  (dom/element
   [:div.cloud-window {:data-window id :data-kind (name kind)}
    [:header.window-bar
     [:span.window-icon (kind-icon kind)]
     (if (= :web kind)
       [:form.window-address-form {:on-submit (fn [^js event]
                                                (.preventDefault event)
                                                (dispatch [:set-window-address id (.-value (.querySelector (.-currentTarget event) "input"))]))}
        [:input.window-address {:type "text" :spellcheck "false" :autocomplete "off" :aria-label "Web address"}]]
       [:span.window-title ""])
     [:button.window-button {:type "button" :title "Reload" :aria-label "Reload" :on-click #(dispatch [:reload-window id])}
      icons/reload]
     [:button.window-button {:type "button" :title "Pin to the grid" :aria-label "Pin to the grid"
                             :on-click #(dispatch [:pin {:kind :window :id id}])}
      icons/grid]
     [:button.window-button {:type "button" :title (if (= :web kind) "Open in the browser" "Open in VS Code")
                             :aria-label (if (= :web kind) "Open in the browser" "Open in VS Code")
                             :on-click #(dispatch [:reveal-window id])}
      icons/open-file]
     [:button.window-button {:type "button" :title "Close" :aria-label "Close" :on-click #(dispatch [:close-window id])}
      icons/close]]
    [:div.window-body [:p.window-message "Loading"]]]))

(defn- hole-mesh
  "A rectangle writing clear pixels into the canvas wherever it is the
  nearest thing drawn, sorted among the pages by depth."
  []
  (let [^js mesh (three/Mesh. planes/unit-plane (three/MeshBasicMaterial. #js {:color 0x000000
                                                                                :transparent true
                                                                                :opacity 0
                                                                                :blending three/NoBlending
                                                                                :depthWrite false}))]
    (set! (.-renderOrder mesh) 2)
    (set! (.-frustumCulled mesh) false)
    mesh))

(defn- add-window! [{:keys [windows css-scene ^js scene dispatch] :as layer} {:keys [id] :as window}]
  (let [^js element (window-element dispatch window)
        ^js window-object (CSS3DObject. element)
        hole (hole-mesh)]
    ;; The renderer forbids selecting text in its elements, and a window's
    ;; text should select as a browser's does.
    (set! (.. element -style -userSelect) "")
    (follow-bar-drags! layer id (dom/find-in element ".window-bar"))
    (.add ^js css-scene window-object)
    (.add scene hole)
    (swap! windows assoc id (assoc window :element element :object window-object :hole hole))))

(defn- remove-window! [{:keys [windows ^js css-scene ^js scene active] :as layer} window-id]
  (when-let [{:keys [^js object ^js hole ^js element]} (get @windows window-id)]
    (when (= window-id @active) (deactivate! layer))
    (some-> (dom/find-in element ".window-body") .-pdfDocument .destroy)
    (.remove css-scene object)
    (.remove scene hole)
    (.dispose ^js (.-material hole))
    (swap! windows dissoc window-id)))

;; Content

(defn- image-source
  "Function from an image's address in Markdown to where it loads from,
  relative addresses taken from `base-url`."
  [base-url]
  (fn [address]
    (try
      (.-href (js/URL. address (js/URL. (str base-url "/") (.-href js/location))))
      (catch :default _error nil))))

(defn- show-image! [^js body address title]
  (-> (js/fetch address #js {:cache "no-store"})
      (.then (fn [^js response]
               (if (.-ok response)
                 (.blob response)
                 (throw (js/Error. (str "The image could not be read (" (.-status response) ")"))))))
      (.then (fn [blob]
               (let [^js image (or (dom/find-in body "img")
                                   (let [built (dom/element [:img.window-image {:alt title}])]
                                     (dom/replace-children! body built)
                                     built))
                     blob-address (js/URL.createObjectURL blob)]
                 (some-> (.-blobAddress image) js/URL.revokeObjectURL)
                 (set! (.-blobAddress image) blob-address)
                 (set! (.-src image) blob-address))))
      (.catch (fn [error] (dom/replace-children! body [:p.window-message (or (.-message error) (str error))])))))

(defn- frame-in [^js body title]
  (or (dom/find-in body "iframe")
      (let [built (dom/element [:iframe.window-frame {:title title :allow "clipboard-read; clipboard-write; fullscreen"}])]
        (dom/replace-children! body built)
        built)))

(defn- show-content!
  "Shows what the host sent for a window: a page to load, Markdown to read,
  a PDF, an image, what the window waits for, or why there is nothing."
  [{:keys [^js element kind] :as window} {:keys [url text base-url error waiting]}]
  (let [^js body (dom/find-in element ".window-body")
        title (windows-state/window-title window)]
    (cond
      (or error waiting) (dom/replace-children! body [:p.window-message (or error waiting)])
      (contains? #{:web :html} kind) (set! (.-src ^js (frame-in body title)) url)
      (= :markdown kind) (let [scroll (.-scrollTop body)]
                           (dom/replace-children! body (into [:article.window-markdown]
                                                             (markdown/hiccup (or text "") {:document? true
                                                                                            :image-source (image-source base-url)})))
                           (set! (.-scrollTop body) scroll))
      (= :pdf kind) (pdf-view/show! body url)
      (= :image kind) (show-image! body url title))))

(defn- sync-windows!
  "Adds elements for new windows and takes away those of closed ones,
  and shows content that arrived since the last frame."
  [{:keys [windows] :as layer} {window-states :windows contents :window-contents}]
  (let [wanted (into {} (map (juxt :id identity)) window-states)]
    (doseq [window-id (keys @windows)
            :when (not (contains? wanted window-id))]
      (remove-window! layer window-id))
    (doseq [{:keys [id] :as window} window-states]
      (if-let [existing (get @windows id)]
        (when (not= (select-keys existing [:source :offset]) (select-keys window [:source :offset]))
          (swap! windows update id merge (select-keys window [:source :offset])))
        (add-window! layer window))
      (let [{:keys [shown-content element] :as entry} (get @windows id)
            content (get contents id)]
        (when (and content (not= content shown-content))
          (swap! windows assoc-in [id :shown-content] content)
          (show-content! entry content))
        (if (= :web (:kind window))
          (let [^js address (dom/find-in element ".window-address")]
            (when (and (not= address (.-activeElement js/document)) (not= (.-value address) (:source window)))
              (set! (.-value address) (:source window))))
          (dom/set-text! (dom/find-in element ".window-title") (windows-state/window-title window)))))))

;; Places

(defn- cloud-box
  "Least and most corners of the box holding the cloud's pages where they
  belong, or a point at the origin for an empty cloud."
  [app]
  (let [extents (placement/extents app)]
    (if (empty? extents)
      [[0 0 0] [0 0 0]]
      (reduce (fn [[[least-x least-y least-z] [most-x most-y most-z]] {[x y z] :position :keys [width height]}]
                (let [half-width (* 0.5 width)
                      half-height (* 0.5 height)]
                  [[(min least-x (- x half-width)) (min least-y (- y half-height)) (min least-z (- z half-width))]
                   [(max most-x (+ x half-width)) (max most-y (+ y half-height)) (max most-z (+ z half-width))]]))
              [[##Inf ##Inf ##Inf] [##-Inf ##-Inf ##-Inf]]
              extents))))

(defn- homes
  "Where each window floats until moved, worked out again twice a second
  as the cloud settles, and at once when windows come or go."
  [{:keys [homes]} app windows now]
  (let [{:keys [at positions for-windows]} @homes
        window-keys (mapv (juxt :id :kind) windows)
        refresh-milliseconds 500
        ;; World units between the cloud and the first window, and
        ;; between windows.
        gap 40]
    (if (and at (< (- now at) refresh-milliseconds) (= window-keys for-windows))
      positions
      (let [computed (windows-state/home-positions windows (cloud-box app) (/ 1 (camera/reading-pixels app)) gap)]
        (reset! homes {:at now :positions computed :for-windows window-keys})
        computed))))

(defn- place-in-cloud!
  "Stands a window at `position` facing the camera at the reading scale,
  with its hole behind it in the canvas."
  [{:keys [^js object ^js hole ^js element kind]} position ^js camera units-per-pixel]
  (let [[width height] (get windows-state/window-sizes kind)
        [x y z] position]
    (set! (.. element -style -width) (str width "px"))
    (set! (.. element -style -height) (str height "px"))
    (.set (.-position object) x y z)
    (.copy (.-quaternion object) (.-quaternion camera))
    (.setScalar (.-scale object) units-per-pixel)
    (set! (.-visible hole) true)
    (.set (.-position hole) x y z)
    (.copy (.-quaternion hole) (.-quaternion camera))
    (.set (.-scale hole) (* width units-per-pixel) (* height units-per-pixel) 1)))

(defn- place-in-cell!
  "Lays a window flat over a grid cell's body, a little ahead of the
  camera so nothing in the cloud comes before it. The canvas leaves the
  body clear, so the window needs no hole."
  [app {:keys [^js object ^js hole ^js element]} {:keys [left top width height]}]
  (let [{viewport-width :width viewport-height :height} (camera/viewport app)
        {:keys [^js right ^js up ^js forward]} (camera/basis app)
        ^js eye (.-position ^js (:camera app))
        depth 2
        span (/ 1 (zoom/pixels-per-unit depth navigation/field-of-view viewport-height))
        across (* span (- (+ left (* 0.5 width)) (* 0.5 viewport-width)))
        rise (* span (- (* 0.5 viewport-height) (+ top (* 0.5 height))))]
    (set! (.. element -style -width) (str width "px"))
    (set! (.. element -style -height) (str height "px"))
    (.set (.-position object)
          (+ (.-x eye) (* depth (.-x forward)) (* across (.-x right)) (* rise (.-x up)))
          (+ (.-y eye) (* depth (.-y forward)) (* across (.-y right)) (* rise (.-y up)))
          (+ (.-z eye) (* depth (.-z forward)) (* across (.-z right)) (* rise (.-z up))))
    (.copy (.-quaternion object) (.-quaternion ^js (:camera app)))
    (.setScalar (.-scale object) span)
    (set! (.-visible hole) false)))

(defn world-size
  "A window's width and height in world units in the cloud."
  [app kind]
  (let [units-per-pixel (/ 1 (camera/reading-pixels app))]
    (mapv #(* units-per-pixel %) (get windows-state/window-sizes kind))))

(defn- shifted [[x y z] ^js direction distance]
  [(+ x (* distance (.-x direction))) (+ y (* distance (.-y direction))) (+ z (* distance (.-z direction)))])

(defn- fly-now!
  "Flies to a window until it fills the part of the view no pane covers,
  stopping at one CSS pixel to a pixel. True when the window has a place."
  [{:keys [windows]} app window-id]
  (boolean
   (when-let [{:keys [position kind]} (get @windows window-id)]
     (when position
       (let [[width height] (world-size app kind)
             pixels-per-unit (min (camera/reading-pixels app) (camera/open-fitting-pixels-per-unit app width height))
             distance (camera/distance-for-pixels-per-unit app pixels-per-unit)
             {:keys [right]} (camera/basis app)]
         (navigation/fly-to! (:controller app) (shifted position right (camera/panel-shift app distance)) distance 1.0)
         true)))))

(defn fly-to!
  "Flies to window `window-id`, now or once it has a place."
  [layer app window-id]
  (when-not (fly-now! layer app window-id)
    (reset! (:pending-flight layer) window-id)))

(defn update-windows!
  "Keeps every window's element, content, and place in line with the state
  this frame, and starts a flight that waited for a window's place. A
  window hidden from the cloud keeps its home, so it returns where it
  was, and still shows in a grid cell."
  [{:keys [windows drag pending-flight] :as layer} app state now]
  (sync-windows! layer state)
  (let [home-of (homes layer app (:windows state) now)
        units-per-pixel (/ 1 (camera/reading-pixels app))
        ^js camera (:camera app)]
    (doseq [[window-id {:keys [offset element] :as entry}] @windows]
      (let [cell (grid/cell-of-window app window-id)
            hidden? (and (nil? cell) (:hidden? (windows-state/window-of state window-id)))
            dragged (when (= window-id (:id @drag)) (:moved-to @drag))
            home (get home-of window-id)
            position (when (and home (not hidden?)) (mapv + home (or dragged offset [0 0 0])))]
        (dom/toggle-class! element "is-pinned" (some? cell))
        (dom/toggle-class! element "is-hidden" hidden?)
        (swap! windows update window-id assoc :position position :pinned? (some? cell))
        (cond
          cell (place-in-cell! app entry (:body cell))
          position (place-in-cloud! entry position camera units-per-pixel)
          :else (set! (.-visible ^js (:hole entry)) false))))
    (when-let [window-id @pending-flight]
      (when (or (fly-now! layer app window-id) (not (contains? @windows window-id)))
        (reset! pending-flight nil)))))

(defn extents
  "Every window floating in the cloud as {:position :width :height}, for
  framing the cloud with its windows."
  [{:keys [windows]} app]
  (keep (fn [[_window-id {:keys [position kind pinned?]}]]
          (when (and position (not pinned?))
            (let [[width height] (world-size app kind)]
              {:position position :width width :height height})))
        @windows))

(defn label-candidates
  "Labels naming the windows too far off to read their bars, as the
  labels of distant pages name them."
  [{:keys [windows]} app]
  (let [{:keys [^js forward]} (camera/basis app)
        ^js camera (:camera app)
        {:keys [height]} (camera/viewport app)
        readable-bar-pixels 24
        bar-pixels 40
        label-gap 3]
    (for [[window-id {:keys [position kind pinned?] :as window}] @windows
          :when (and position (not pinned?))
          :let [depth (camera/depth-along camera forward position)]
          :when (pos? depth)
          :let [scale (/ (zoom/pixels-per-unit depth navigation/field-of-view height) (camera/reading-pixels app))
                [x y] (camera/screen-point app position)
                half-height (* 0.5 scale (second (get windows-state/window-sizes kind)))]
          :when (< (* scale bar-pixels) readable-bar-pixels)]
      {:key [:window window-id]
       :kind :page
       :prefix ""
       :leaf (windows-state/window-label window)
       :x x
       :y (- y half-height label-gap)
       :depth depth
       :priority 1
       :classes {}})))

(defn hit
  "The nearest window whose hole a ray meets, as {:kind :window :id
  :distance}, or nil."
  [{:keys [windows]} ^js raycaster]
  (let [hole-ids (into {} (keep (fn [[window-id {:keys [^js hole]}]] (when (.-visible hole) [hole window-id]))) @windows)]
    (when (seq hole-ids)
      (when-let [^js found (first (array-seq (.intersectObjects raycaster (into-array (keys hole-ids)) false)))]
        {:kind :window :id (get hole-ids (.-object found)) :distance (.-distance found)}))))

(defn render! [{:keys [^js css-renderer css-scene]} ^js camera]
  (.render css-renderer css-scene camera))

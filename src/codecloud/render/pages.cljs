(ns codecloud.render.pages
  "Pages as three.js objects facing the camera: a glass body whose shader
  draws a minimap of the code, and real text once it can be read."
  (:require ["three" :as three]
            [codecloud.languages.catalog :as catalog]
            [codecloud.presentation.code-colors :as code-colors]
            [codecloud.presentation.color :as color]
            [codecloud.presentation.highlight :as highlight]
            [codecloud.presentation.opacity :as opacity]
            [codecloud.presentation.page :as page]
            [codecloud.presentation.palette :as palette]
            [codecloud.presentation.zoom :as zoom]
            [codecloud.render.lru :as lru]
            [codecloud.render.minimap :as minimap]
            [codecloud.render.page-text :as page-text]
            [codecloud.render.shaders :as shaders]
            [codecloud.render.text :as text]))

(defonce ^:private unit-plane (three/PlaneGeometry. 1 1))

(defn create-layer
  "Every page by namespace id, with highlighted lines cached for the pages
  being read and the uniforms all pages share."
  [^js scene]
  (let [^js root (three/Object3D.)]
    (.add scene root)
    {:root root
     :pages (atom {})
     :highlighted (lru/create)
     :shared {:syntax-colors #js {:value (into-array (repeatedly (count highlight/minimap-kinds) #(three/Vector3.)))}
              :ink-color #js {:value (three/Vector3.)}
              :empty-texture (minimap/empty-texture)}}))

;; Source text

(defn panel-lines
  "Every line a page shows as {:tokens :number :bracket-depth :family},
  with an unnumbered comment naming each file of a page of several, the
  depth being the brackets open where the line starts and the family the
  file's language family. `tokens-of` gives a file's token lines, by
  default by reading its :text."
  ([files] (panel-lines files (fn [{:keys [text language]}]
                                (text/tokenize-source (or language :clojure) (or text "")))))
  ([files tokens-of]
   (let [several? (> (count files) 1)]
     (into []
           (mapcat (fn [{:keys [path language] :as file}]
                     (let [comment-start (or (:line-comment (catalog/language (or language :clojure))) "//")
                           family (catalog/language-family (or language :clojure))
                           token-lines (tokens-of file)]
                       (concat (when several? [{:tokens [[:comment (str comment-start " " path)]]
                                                :number nil :bracket-depth 0 :family family}])
                               (map (fn [number tokens depth]
                                      {:tokens tokens :number number :bracket-depth depth :family family})
                                    (iterate inc 1)
                                    token-lines
                                    (highlight/line-bracket-depths token-lines))))))
           files))))

(defn- highlighted-lines
  "Highlighted panel lines of a source: those it carries, as a page being
  edited does, or else lines cached by its key."
  [{:keys [highlighted]} {:keys [source-key files lines]}]
  (let [capacity 48]
    (or lines
        (lru/recall highlighted source-key)
        (lru/remember! highlighted source-key (panel-lines files) capacity))))

(defn- with-minimap
  "The page with a minimap of its source, rebuilt when the source or shape
  changes. The page owns the table and frees the one it replaces. While
  a page's edited text shows, its minimap waits, since typing would
  rebuild it at every key."
  [page-object source layer]
  (let [{:keys [line-count code-characters]} (get-in page-object [:visual :geometry])
        minimap-key [(:source-key source) line-count code-characters]
        text-showing 0.5]
    (if (or (= minimap-key (:minimap-key page-object))
            (and (:lines source) (:minimap page-object) (> (:text-share page-object) text-showing)))
      page-object
      (do (some-> (:minimap page-object) minimap/release!)
          (assoc page-object
                 :minimap (minimap/table (highlighted-lines layer source) line-count code-characters)
                 :minimap-key minimap-key)))))

;; Building pages

(defn- page-material [{:keys [syntax-colors ink-color empty-texture]}]
  (three/ShaderMaterial.
   #js {:vertexShader shaders/page-vertex
        :fragmentShader shaders/page-fragment
        :transparent true
        :depthWrite false
        :depthTest false
        :uniforms #js {:size #js {:value (three/Vector2. 1 1)}
                       :panelColor #js {:value (three/Vector3. 1 1 1)}
                       :accentColor #js {:value (three/Vector3. 0.5 0.5 0.5)}
                       :inkColor ink-color
                       :opacity #js {:value 0.0}
                       :headerHeight #js {:value page/header-height}
                       :padding #js {:value page/padding}
                       :columnWidth #js {:value 1.0}
                       :columnGap #js {:value page/column-gap}
                       :gutterWidth #js {:value 1.0}
                       :characterWidth #js {:value 0.5}
                       :rows #js {:value 1.0}
                       :columns #js {:value 1.0}
                       :lineCount #js {:value 1.0}
                       :codeCharacters #js {:value 80.0}
                       :minimap #js {:value empty-texture}
                       :minimapSize #js {:value (three/Vector2. 1 1)}
                       :minimapOpacity #js {:value 1.0}
                       :hasMinimap #js {:value 0.0}
                       :syntaxColors syntax-colors
                       :highlight #js {:value 0.0}
                       :pulse #js {:value 0.0}
                       :pill #js {:value 0.0}
                       :cornerRadius #js {:value 1.2}
                       :borderWidth #js {:value 0.32}
                       :borderPixels #js {:value 1.5}}}))

(defn- create-page [id visual shared]
  (let [^js group (three/Object3D.)
        ^js body (three/Mesh. unit-plane (page-material shared))
        ^js overlay (three/Object3D.)]
    (set! (.-renderOrder body) 2)
    (set! (.-frustumCulled body) false)
    ;; Text sits a hair in front of the glass.
    (set! (.. overlay -position -z) 0.02)
    (.add group body)
    (.add group overlay)
    {:id id
     :group group
     :body body
     :overlay overlay
     :visual visual
     :colors (palette/page-rgb (:color-spec visual))
     :appearance 0.0
     :leaving? false
     :offset [0 0 0]
     :text-share 0.0
     :tiles {}}))

(defn- dispose-page! [page-object]
  (page-text/dispose-text! page-object)
  (page-text/dispose-name! page-object)
  (some-> (:minimap page-object) minimap/release!)
  (when-let [^js parent (.-parent ^js (:group page-object))]
    (.remove parent (:group page-object)))
  (.dispose ^js (.-material ^js (:body page-object))))

(defn sync-pages!
  "Matches the layer to `visuals`: new pages fade in, missing ones fade
  out, and recolored ones take their new colors."
  [{:keys [root pages shared]} visuals]
  (let [updated (reduce-kv (fn [by-id id visual]
                             (if-let [existing (get by-id id)]
                               (assoc by-id id (cond-> (assoc existing :visual visual :leaving? false)
                                                 (not= (:color-spec (:visual existing)) (:color-spec visual))
                                                 (assoc :colors (palette/page-rgb (:color-spec visual)))))
                               (let [created (create-page id visual shared)]
                                 (.add ^js root (:group created))
                                 (assoc by-id id created))))
                           @pages
                           visuals)]
    (reset! pages (reduce-kv (fn [by-id id page-object]
                               (if (contains? visuals id)
                                 by-id
                                 (assoc by-id id (assoc page-object :leaving? true))))
                             updated
                             updated))))

;; Measuring pages each frame

(defn- approach
  "`current` eased toward `target` at `rate` over `elapsed` seconds."
  [current target rate elapsed]
  (+ current (* (- target current) (- 1 (js/Math.exp (- (* rate elapsed)))))))

(defn- approach-vector [current target rate elapsed]
  (mapv (fn [current-part target-part] (approach current-part target-part rate elapsed)) current target))

(defn- add-vectors [[first-x first-y first-z] [second-x second-y second-z]]
  [(+ first-x second-x) (+ first-y second-y) (+ first-z second-z)])

(defn pill-size
  "Size of a page drawn as its name in the names-only view, growing with
  the page."
  [{:keys [radius]} name-aspect]
  (let [name-height (max 2.2 (min 60 (* 0.2 radius)))
        text-height (* 1.4 name-height)]
    {:name-height name-height
     :text-width (* name-aspect text-height)
     :text-height text-height
     :width (+ (* name-aspect text-height) (* 1.3 name-height))
     :height (* 1.75 name-height)}))

(defn shape
  "{:width :height} a page covers in the current display mode."
  [page-object display-mode]
  (let [geometry (get-in page-object [:visual :geometry])]
    (if (and (= :names display-mode) (:name page-object))
      (pill-size geometry (get-in page-object [:name :aspect]))
      geometry)))

(defn- glide
  "Where a page stands this frame, gliding toward `destination`, or there
  at once without an earlier place or with reduced motion."
  [current destination elapsed motion]
  (let [glide-rate 6]
    (if (and current destination (pos? motion))
      (approach-vector current destination glide-rate elapsed)
      (or destination current))))

(defonce ^:private projection-scratch (three/Vector3.))

(defn- grown-shift
  "Page-local [x y] that keeps a page's top left corner where it was
  before an edit reshaped it: half of its growth right and down."
  [{:keys [geometry base-size]}]
  (if-let [[base-width base-height] base-size]
    [(* 0.5 (- (:width geometry) base-width)) (* -0.5 (- (:height geometry) base-height))]
    [0 0]))

(defn- local-to-world
  "World vector of page-local [x y], whose axes follow the camera."
  [[local-x local-y] ^js right ^js up]
  [(+ (* local-x (.-x right)) (* local-y (.-x up)))
   (+ (* local-x (.-y right)) (* local-y (.-y up)))
   (+ (* local-x (.-z right)) (* local-y (.-z up)))])

(defn- measure-page
  "First pass of a frame: eases the page's fade and position, places it,
  and measures it on screen. A carried page follows the pointer exactly.
  A page reshaped by an edit keeps its top left corner in place, and once
  the reshaping ends the page eases back instead of jumping."
  [page-object {:keys [^js camera ^js forward ^js right ^js up positions offset-of carried-id elapsed motion viewport
                       field-of-view display-mode]}]
  (let [{:keys [id leaving? visual]} page-object
        fade-rate 3.5
        offset-rate 4.5
        nearest-depth 0.05
        appearance (approach (:appearance page-object) (if leaving? 0 1) fade-rate elapsed)
        layout-position (glide (:layout-position page-object) (get positions id) elapsed motion)
        anchor (or layout-position [0 0 0])
        wanted-offset (offset-of id anchor)
        local-shift (grown-shift visual)
        previous-shift (:local-shift page-object [0 0])
        settling-offset (if (and (not= local-shift previous-shift) (not (:edited? visual)))
                          (add-vectors (:offset page-object)
                                       (local-to-world (mapv - previous-shift local-shift) right up))
                          (:offset page-object))
        offset (if (= id carried-id)
                 wanted-offset
                 (approach-vector settling-offset wanted-offset offset-rate elapsed))
        [x y z] (add-vectors (add-vectors anchor offset) (local-to-world local-shift right up))
        ^js group (:group page-object)
        _ (.set (.-position group) x y z)
        _ (.copy (.-quaternion group) (.-quaternion camera))
        ^js camera-position (.-position camera)
        depth (+ (* (- x (.-x camera-position)) (.-x forward))
                 (* (- y (.-y camera-position)) (.-y forward))
                 (* (- z (.-z camera-position)) (.-z forward)))
        {:keys [width height]} viewport
        pixels-per-unit (if (> depth nearest-depth) (zoom/pixels-per-unit depth field-of-view height) 0)
        ^js projected (.project (.set ^js projection-scratch x y z) camera)
        screen-x (* (+ 1 (.-x projected)) 0.5 width)
        screen-y (* (- 1 (.-y projected)) 0.5 height)
        {page-width :width page-height :height} (shape page-object display-mode)
        half-width-pixels (* 0.5 page-width pixels-per-unit)
        half-height-pixels (* 0.5 page-height pixels-per-unit)]
    (-> (transient page-object)
        (assoc! :appearance appearance)
        (assoc! :offset offset)
        (assoc! :local-shift local-shift)
        (assoc! :layout-position layout-position)
        (assoc! :position [x y z])
        (assoc! :depth depth)
        (assoc! :pixels-per-unit pixels-per-unit)
        (assoc! :screen-x screen-x)
        (assoc! :screen-y screen-y)
        (assoc! :half-width-pixels half-width-pixels)
        (assoc! :half-height-pixels half-height-pixels)
        (assoc! :on-screen? (and (> depth nearest-depth)
                                 (< (- screen-x half-width-pixels) width)
                                 (> (+ screen-x half-width-pixels) 0)
                                 (< (- screen-y half-height-pixels) height)
                                 (> (+ screen-y half-height-pixels) 0)))
        persistent!)))

;; Drawing pages each frame

(defn- with-text
  "The page with its text tiles and name band, drawn while the frame has
  time, eased in once ready and dropped once faded out."
  [page-object source wants-text? layer {:keys [device-pixel-ratio elapsed] :as frame}]
  (let [text-rate 6
        faded 0.01
        drawn (if (and wants-text? source (page-text/time-left? frame))
                (let [level (page-text/tile-level (:pixels-per-unit page-object) device-pixel-ratio)]
                  (-> page-object
                      (page-text/sync-tiles (highlighted-lines layer source) level frame)
                      (page-text/sync-title level frame)))
                page-object)
        text-share (approach (:text-share drawn) (if (and wants-text? (:text-ready? drawn)) 1 0) text-rate elapsed)]
    (if (and (not wants-text?) (< text-share faded) (or (seq (:tiles drawn)) (:title drawn)))
      (page-text/dispose-text! drawn)
      (assoc drawn :text-share text-share))))

(defn- set-geometry-uniforms!
  "Shader values that follow the layout of the page's code."
  [^js uniforms {:keys [column-width gutter-characters character-width rows columns line-count code-characters]}]
  (set! (.-value (.-columnWidth uniforms)) column-width)
  (set! (.-value (.-gutterWidth uniforms)) (* gutter-characters character-width))
  (set! (.-value (.-characterWidth uniforms)) character-width)
  (set! (.-value (.-rows uniforms)) rows)
  (set! (.-value (.-columns uniforms)) columns)
  (set! (.-value (.-lineCount uniforms)) line-count)
  (set! (.-value (.-codeCharacters uniforms)) code-characters))

(defn- set-look-uniforms!
  "Shader values that change each frame: size, colors, opacity, minimap,
  hover, a cycle's pulse, and the names-only pill."
  [^js uniforms {:keys [id visual minimap text-share]} {:keys [width height]} {:keys [panel accent opacity pill]}
   {:keys [time motion hovered-id]}]
  (let [pulse-rate 2.4]
    (.set (.-value (.-size uniforms)) width height)
    (.set (.-value (.-panelColor uniforms)) (nth panel 0) (nth panel 1) (nth panel 2))
    (.set (.-value (.-accentColor uniforms)) (nth accent 0) (nth accent 1) (nth accent 2))
    (set! (.-value (.-opacity uniforms)) opacity)
    (set! (.-value (.-hasMinimap uniforms)) (if minimap 1.0 0.0))
    (when minimap
      (set! (.-value (.-minimap uniforms)) (:texture minimap))
      (.set (.-value (.-minimapSize uniforms)) (nth (:size minimap) 0) (nth (:size minimap) 1)))
    (set! (.-value (.-minimapOpacity uniforms)) (- 1 text-share))
    (set! (.-value (.-highlight uniforms)) (if (= id hovered-id) 1.0 0.0))
    (set! (.-value (.-pulse uniforms)) (if (:cyclic? visual)
                                         (* motion (+ 0.5 (* 0.5 (js/Math.sin (* pulse-rate time)))))
                                         0.0))
    (set! (.-value (.-pill uniforms)) (if pill 1.0 0.0))
    (set! (.-value (.-cornerRadius uniforms)) (if pill (* 0.5 (:height pill)) 1.2))
    ;; The frame is at least a pixel and a half wide however far the page
    ;; is, and a parse error's at least three, so a broken file reads from
    ;; afar without a heavy frame up close.
    (set! (.-value (.-borderWidth uniforms)) (cond pill 0.12 (:parse-error? visual) 0.45 :else 0.32))
    (set! (.-value (.-borderPixels uniforms)) (if (:parse-error? visual) 3.0 1.5))))

(defn- apply-page!
  "Second pass of a frame: decides what the page shows at its size,
  builds that while time allows, and draws it."
  [page-object {:keys [ink display-mode source-of wanted-sources] :as frame} layer]
  (let [{:keys [id visual pixels-per-unit on-screen? colors]} page-object
        geometry (:geometry visual)
        names? (= :names display-mode)
        {:keys [opacity see-through?]} (opacity/page-opacity page-object frame)
        ;; A minimap shows once a page is a few lines tall on screen, and
        ;; text once a line is a few pixels tall.
        least-minimap-pixels 14
        least-text-pixels-per-line 4.5
        least-text-opacity 0.05
        least-visible 0.003
        page-pixels (* (:height geometry) pixels-per-unit)
        wants-minimap? (and (not names?) on-screen? (>= page-pixels least-minimap-pixels))
        wants-text? (and (not names?) on-screen?
                         (>= pixels-per-unit least-text-pixels-per-line)
                         (> opacity least-text-opacity))
        source (when (or wants-minimap? wants-text?) (source-of id))
        shown (-> (if (and wants-minimap? source (page-text/time-left? frame))
                    (with-minimap page-object source layer)
                    page-object)
                  (with-text source wants-text? layer frame)
                  (cond-> names? (page-text/sync-name frame)))
        pill (when (and names? (:name shown)) (pill-size geometry (get-in shown [:name :aspect])))
        accent (palette/mix-rgb (get-in colors [:day :accent]) (get-in colors [:night :accent]) ink)
        panel (palette/mix-rgb (get-in colors [:day :panel]) (get-in colors [:night :panel]) ink)
        ^js body (:body shown)
        ^js uniforms (.-uniforms ^js (.-material body))
        size (shape shown display-mode)]
    (when (and (or wants-minimap? wants-text?) (nil? source))
      (vswap! wanted-sources conj [id page-pixels]))
    (.set (.-scale body) (:width size) (:height size) 1)
    (set! (.-visible body) (> opacity least-visible))
    (set-geometry-uniforms! uniforms geometry)
    (set-look-uniforms! uniforms shown size {:panel panel :accent accent :opacity opacity :pill pill} frame)
    (page-text/set-text-opacity! shown opacity pill)
    (assoc shown :opacity opacity :accent accent :see-through? see-through?)))

(def ^:private minimap-rgb
  "Day and night RGB of each minimap token kind, plain text among them,
  in the code colors last asked for."
  (let [last-asked (atom nil)]
    (fn [code-colors]
      (let [[asked table] @last-asked]
        (if (identical? asked code-colors)
          table
          (let [rgb-of (fn [sky token-kind] (color/hex->rgb (:color (code-colors/style code-colors sky token-kind))))
                computed (into {}
                               (map (fn [token-kind]
                                      [token-kind {:day (rgb-of :day token-kind) :night (rgb-of :night token-kind)}]))
                               highlight/minimap-kinds)]
            (reset! last-asked [code-colors computed])
            computed))))))

(defn- update-shared-colors!
  "Minimap colors for this moment of the day, shared by every page."
  [{:keys [syntax-colors ink-color]} code-colors ink]
  (let [kind-rgb (minimap-rgb code-colors)
        blended (fn [token-kind]
                  (let [{:keys [day] night-rgb :night} (get kind-rgb token-kind)]
                    (palette/mix-rgb day night-rgb ink)))]
    (doseq [[kind-index token-kind] (map-indexed vector highlight/minimap-kinds)]
      (let [[red green blue] (blended token-kind)]
        (.set ^js (aget (.-value syntax-colors) kind-index) red green blue)))
    (let [[red green blue] (blended :text)]
      (.set ^js (.-value ink-color) red green blue))))

(defn update-pages!
  "Measures and draws every page for this frame. Returns the ids of pages
  that want source text, largest on screen first."
  [{:keys [pages shared] :as layer} frame]
  (let [wanted-sources (volatile! [])
        budget-milliseconds 7
        gone 0.01
        least-holding 0.001
        measured (persistent!
                  (reduce-kv (fn [by-id id page-object]
                               (if (and (:leaving? page-object) (< (:appearance page-object) gone))
                                 (do (dispose-page! page-object) by-id)
                                 (assoc! by-id id (measure-page page-object frame))))
                             (transient {})
                             @pages))
        examined (when-let [page-object (get measured (:examined-id frame))]
                   (let [holding (opacity/holding-pivot page-object frame)]
                     (when (> holding least-holding)
                       (assoc page-object :holding holding))))
        frame (assoc frame
                     :deadline (+ (js/performance.now) budget-milliseconds)
                     :wanted-sources wanted-sources
                     :examined examined)
        largest-on-screen-first (sort-by (fn [[_id {:keys [half-height-pixels on-screen?]}]]
                                           (- (if on-screen? half-height-pixels 0)))
                                         measured)]
    (update-shared-colors! shared (:code-colors frame) (:ink frame))
    (reset! pages (persistent!
                   (reduce (fn [by-id [id page-object]] (assoc! by-id id (apply-page! page-object frame layer)))
                           (transient {})
                           largest-on-screen-first)))
    (into [] (comp (map first) (distinct)) (sort-by (comp - second) @wanted-sources))))

;; Questions about pages

(defn page-of
  "The page object for a namespace id, or nil."
  [{:keys [pages]} id]
  (get @pages id))

(defn all-pages [{:keys [pages]}]
  (vals @pages))

(defn body-meshes
  "[mesh page] for every page solid enough to point at."
  [{:keys [pages]}]
  (let [least-pointable 0.08]
    (for [page-object (vals @pages)
          :when (and (not (:leaving? page-object))
                     (not (:see-through? page-object))
                     (> (:opacity page-object 0) least-pointable))]
      [(:body page-object) page-object])))

(defn local-point
  "Page-local [x y] of a texture coordinate on a page's body."
  [page-object ^js texture-point display-mode]
  (let [{:keys [width height]} (shape page-object display-mode)]
    [(* width (- (.-x texture-point) 0.5)) (* height (- (.-y texture-point) 0.5))]))

(defn line-at-point
  "{:panel-line :origin} of the code under a page-local point, or nil."
  [page-object local]
  (let [{:keys [geometry files]} (:visual page-object)]
    (when-let [panel-line (page/line-at geometry local)]
      (when-let [origin (page/panel-line-origin files panel-line)]
        {:panel-line panel-line :origin (dissoc origin :heading?)}))))

(defn line-local-point
  "Page-local [x y] of line `line` of `path`, or of the first line when
  the page lacks it."
  [page-object path line]
  (let [{:keys [geometry files]} (:visual page-object)
        panel-line (or (page/origin-panel-line files path line) 0)]
    (page/line-center geometry panel-line)))

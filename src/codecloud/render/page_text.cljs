(ns codecloud.render.page-text
  "Readable text on near pages: code tiles at the sharpness the screen
  needs, the name band, and the names-only view's large name."
  (:require ["three" :as three]
            [codecloud.presentation.page :as page]
            [codecloud.render.text :as text]))

(def ^:private tile-levels
  "Canvas pixels per line at which text tiles are drawn. A tile uses the
  smallest level that is at least as sharp as the screen shows it."
  [8 12 18 26 38 52])

(defonce ^:private unit-plane (three/PlaneGeometry. 1 1))

(defn time-left?
  "True while the frame's budget for drawing text and tables lasts."
  [{:keys [deadline]}]
  (< (js/performance.now) deadline))

(defn tile-level
  "The sharpness, in canvas pixels per line, that text drawn at
  `pixels-per-unit` on screen needs."
  [pixels-per-unit device-pixel-ratio]
  (let [needed (* pixels-per-unit device-pixel-ratio)]
    (or (first (filter #(>= % needed) tile-levels)) (peek tile-levels))))

(defn- flat-mesh
  "A unit plane showing `texture`, drawn after page bodies at its depth."
  [^js texture]
  (let [^js material (three/MeshBasicMaterial. #js {:map texture
                                                    :transparent true
                                                    :depthWrite false
                                                    :depthTest false
                                                    :opacity 0})
        ^js mesh (three/Mesh. unit-plane material)]
    (set! (.-renderOrder mesh) 2)
    mesh))

(defn- dispose-flat-mesh! [^js mesh]
  (when mesh
    (when-let [^js parent (.-parent mesh)] (.remove parent mesh))
    (let [^js material (.-material mesh)]
      (when-let [^js texture (.-map material)] (.dispose texture))
      (.dispose material))))

(defn dispose-text!
  "The page without its text tiles and name band."
  [page-object]
  (doseq [{:keys [mesh]} (vals (:tiles page-object))]
    (dispose-flat-mesh! mesh))
  (dispose-flat-mesh! (get-in page-object [:title :mesh]))
  (assoc page-object :tiles {} :title nil :text-share 0.0))

(defn dispose-name!
  "Frees the large name of the names-only view."
  [page-object]
  (dispose-flat-mesh! (get-in page-object [:name :mesh])))

(defn- tile-span
  "Panel lines and page-local rectangle of tile `chunk` of `column`."
  [{:keys [rows line-count column-width] :as geometry} column chunk]
  (let [first-row (* chunk text/tile-lines)
        first-line (+ (* column rows) first-row)
        end-line (min (+ first-line text/tile-lines) (* (inc column) rows) line-count)
        left (page/column-left geometry column)
        top (- (page/code-top geometry) first-row)]
    {:first-line first-line
     :end-line end-line
     :center-x (+ left (* 0.5 column-width))
     :center-y (- top (* 0.5 (- end-line first-line)))
     :width column-width
     :height (- end-line first-line)}))

(defn- wanted-tiles
  "[column chunk] keys of the tiles of a page that fall on screen, with a
  margin."
  [{:keys [visual screen-x screen-y pixels-per-unit]} {:keys [viewport]}]
  (let [{:keys [columns rows] :as geometry} (:geometry visual)
        {:keys [width height]} viewport
        margin (* 0.2 height)]
    (into #{}
          (for [column (range columns)
                chunk (range (js/Math.ceil (/ rows text/tile-lines)))
                :let [{:keys [center-x center-y first-line end-line] tile-width :width tile-height :height}
                      (tile-span geometry column chunk)
                      left (+ screen-x (* pixels-per-unit (- center-x (* 0.5 tile-width))))
                      right (+ screen-x (* pixels-per-unit (+ center-x (* 0.5 tile-width))))
                      top (- screen-y (* pixels-per-unit (+ center-y (* 0.5 tile-height))))
                      bottom (- screen-y (* pixels-per-unit (- center-y (* 0.5 tile-height))))]
                :when (and (< first-line end-line)
                           (< left (+ width margin)) (> right (- margin))
                           (< top (+ height margin)) (> bottom (- margin)))]
            [column chunk]))))

(defn sync-tiles
  "Keeps text tiles for what is on screen at a sharpness matching the
  screen, drawing new ones while the frame has time, and drops the rest."
  [page-object lines level frame]
  (let [{:keys [tiles overlay visual]} page-object
        {:keys [theme-name editor-font code-colors]} frame
        geometry (:geometry visual)
        wanted (wanted-tiles page-object frame)
        ;; A tile stays while it is no blurrier than the screen needs and
        ;; not far sharper than it, and while the lines, sky, colors,
        ;; font, and page shape it was drawn for last, so an edit redraws
        ;; only the tiles whose lines changed.
        content-of (fn [[column chunk]]
                     (let [{:keys [first-line end-line]} (tile-span geometry column chunk)]
                       (subvec lines (min first-line (count lines)) (min end-line (count lines)))))
        current? (fn [tile tile-key]
                   (and (>= (:level tile) (* 0.9 level))
                        (<= (:level tile) (* 2.5 level))
                        (= (:theme tile) theme-name)
                        (= (:colors tile) code-colors)
                        (= (:font tile) editor-font)
                        (= (:geometry tile) geometry)
                        (= (:content tile) (content-of tile-key))))
        kept (reduce-kv (fn [kept tile-key tile]
                          (if (contains? wanted tile-key)
                            (assoc kept tile-key tile)
                            (do (dispose-flat-mesh! (:mesh tile)) kept)))
                        {}
                        tiles)
        refreshed (reduce (fn [all-tiles [column chunk :as tile-key]]
                            (let [existing (get all-tiles tile-key)]
                              (if (or (and existing (current? existing tile-key)) (not (time-left? frame)))
                                all-tiles
                                (let [{:keys [first-line end-line center-x center-y width height]}
                                      (tile-span geometry column chunk)
                                      {:keys [texture]} (text/tile-texture editor-font geometry lines first-line end-line
                                                                           level code-colors theme-name)
                                      ^js mesh (flat-mesh texture)]
                                  (when existing (dispose-flat-mesh! (:mesh existing)))
                                  (.set (.-position mesh) center-x center-y 0)
                                  (.set (.-scale mesh) width height 1)
                                  (.add ^js overlay mesh)
                                  (assoc all-tiles tile-key {:mesh mesh :level level :theme theme-name :colors code-colors
                                                             :content (content-of tile-key)
                                                             :font editor-font :geometry geometry})))))
                          kept
                          (sort wanted))]
    (assoc page-object
           :tiles refreshed
           :text-ready? (every? #(and (contains? refreshed %) (current? (get refreshed %) %)) wanted))))

(defn sync-title
  "The namespace's name in the band at the top of the page, drawn at the
  sharpness of the page's text."
  [page-object level {:keys [theme-name editor-font] :as frame}]
  (let [{:keys [visual title overlay]} page-object
        label (select-keys visual [:rank :tag :prefix :leaf :dirty?])
        title-key [label level theme-name editor-font (select-keys (:geometry visual) [:width :height])]]
    (if (or (= title-key (:key title)) (not (time-left? frame)))
      page-object
      (let [{:keys [texture aspect]} (text/title-texture editor-font label level theme-name)
            ^js mesh (flat-mesh texture)
            {:keys [width height]} (:geometry visual)
            text-height 1.4
            natural-width (* aspect text-height)
            fitted-width (min natural-width (- width (* 2 page/padding)))
            fitted-height (* text-height (/ fitted-width natural-width))]
        (dispose-flat-mesh! (:mesh title))
        (.set (.-position mesh)
              (+ (- (/ width 2)) page/padding (* 0.5 fitted-width))
              (- (/ height 2) page/padding (* 0.5 page/header-height))
              0)
        (.set (.-scale mesh) fitted-width fitted-height 1)
        (.add ^js overlay mesh)
        (assoc page-object :title {:mesh mesh :key title-key})))))

(defn sync-name
  "The large name of the names-only view."
  [page-object {:keys [theme-name editor-font] :as frame}]
  (let [{:keys [visual overlay] large-name :name} page-object
        label (select-keys visual [:rank :tag :prefix :leaf])
        name-key [label theme-name editor-font]]
    (if (or (= name-key (:key large-name)) (and large-name (not (time-left? frame))))
      page-object
      (let [{:keys [texture aspect]} (text/name-texture editor-font label theme-name)
            ^js mesh (flat-mesh texture)]
        (dispose-flat-mesh! (:mesh large-name))
        (.add ^js overlay mesh)
        (assoc page-object :name {:mesh mesh :key name-key :aspect aspect})))))

(defn- set-material-opacity! [^js mesh opacity]
  (let [least-visible 0.003]
    (when mesh
      (set! (.-opacity ^js (.-material mesh)) opacity)
      (set! (.-visible mesh) (> opacity least-visible)))))

(defn set-text-opacity!
  "Fades the page's text tiles and name band, and the large name of the
  names-only view, with the page."
  [{:keys [tiles title text-share] large-name :name} opacity pill]
  (let [text-opacity (* text-share (min 1 (* 1.15 opacity)))]
    (doseq [{:keys [mesh]} (vals tiles)]
      (set-material-opacity! mesh text-opacity))
    (set-material-opacity! (:mesh title) text-opacity))
  (when-let [^js name-mesh (:mesh large-name)]
    (if pill
      (do (.set (.-position name-mesh) 0 0 0)
          (.set (.-scale name-mesh) (:text-width pill) (:text-height pill) 1)
          (set-material-opacity! name-mesh (min 1 (* 1.3 opacity))))
      (set-material-opacity! name-mesh 0))))

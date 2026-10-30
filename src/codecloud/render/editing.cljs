(ns codecloud.render.editing
  "Editing on a page in the cloud: the text of pages whose files have
  buffers, lexed again only where an edit changed it; the caret and the
  selection drawn on the page; and the text position under the pointer."
  (:require ["three" :as three]
            [codecloud.languages.lexer :as lexer]
            [codecloud.languages.lexicons :as lexicons]
            [codecloud.presentation.code-colors :as code-colors]
            [codecloud.presentation.color :as color]
            [codecloud.presentation.editing :as editing]
            [codecloud.presentation.page :as page]
            [codecloud.presentation.palette :as palette]
            [codecloud.render.pages :as pages]
            [codecloud.render.text :as text]))

;; The text of edited pages

(defn- buffer-tokens
  "Token lines of a buffer's lines, lexed again from the last lexing only
  where the lines changed. `cache` maps paths to lexing results."
  [cache path language-id lines]
  (let [previous (get @cache path)
        lexed (cond
                (and previous (identical? lines (:lines previous))) previous
                (and previous (= language-id (:language-id previous))) (lexer/retokenized previous lines)
                :else (assoc (lexer/tokenized (lexicons/lexicon-for language-id) lines) :language-id language-id))]
    (when-not (identical? lexed previous)
      (swap! cache assoc path (assoc lexed :language-id language-id)))
    (:tokens lexed)))

(defn edited-source
  "The source of page `node` with the text of files that have buffers in
  place of `files`, as {:source-key :lines}, or nil when none of its files
  has a buffer. Built once for each change of its inputs."
  [{:keys [frame]} namespace-id node files buffers]
  (let [node-files (:files node)
        page-buffers (map #(get buffers (:path %)) node-files)]
    (when (some some? page-buffers)
      (let [inputs (into [files] (map :lines page-buffers))
            {cached-inputs :inputs cached-source :source} (get-in @frame [:edited-sources namespace-id])]
        (if (and cached-source (= (count inputs) (count cached-inputs)) (every? true? (map identical? inputs cached-inputs)))
          cached-source
          (let [cache (or (:edit-lexing @frame) (let [created (atom {})] (swap! frame assoc :edit-lexing created) created))
                text-of (fn [path] (some #(when (= path (:path %)) (:text %)) files))
                lines (pages/panel-lines node-files
                                         (fn [{:keys [path language]}]
                                           (let [language-id (or language :clojure)]
                                             (if-let [{buffer-lines :lines} (get buffers path)]
                                               (buffer-tokens cache path language-id buffer-lines)
                                               (text/tokenize-source language-id (or (text-of path) ""))))))
                source {:source-key [:edited namespace-id (mapv #(vector (:epoch %) (:revision %)) (remove nil? page-buffers))]
                        :lines lines}]
            (swap! frame assoc-in [:edited-sources namespace-id] {:inputs inputs :source source})
            source))))))

(defn forget-settled!
  "Drops what the frame keeps for pages and files that no longer have
  buffers."
  [{:keys [frame]} buffers]
  (when-let [cache (:edit-lexing @frame)]
    (swap! cache #(select-keys % (keys buffers))))
  (when (empty? buffers)
    (swap! frame dissoc :edited-sources)))

;; Positions on a page

(defn- line-text
  "Text of zero-based `line` of the buffer, or the empty string."
  [{:keys [lines]} line]
  (get lines line ""))

(defn text-position
  "Zero-based [line column] in the buffer at the page-local point
  `local`, or nil off the buffer's file. With `nearest?`, a point beside
  the lines or over another file of the page lands on the nearest end of
  the buffer's file."
  [page-object local buffer nearest?]
  (let [{:keys [geometry files]} (:visual page-object)
        panel-line (if nearest? (page/line-near geometry local) (page/line-at geometry local))
        origin (when panel-line (page/panel-line-origin files panel-line))
        path (:path buffer)]
    (cond
      (and origin (= path (:path origin)) (not (:heading? origin)))
      (let [line (dec (:line origin))
            column (quot panel-line (:rows geometry))
            code-left (+ (page/column-left geometry column) (* (:gutter-characters geometry) (:character-width geometry)))
            visual (js/Math.round (/ (- (first local) code-left) (:character-width geometry)))]
        [line (editing/column-at-visual (line-text buffer line) (max 0 visual) page/tab-size)])

      nearest?
      (let [first-panel-line (page/origin-panel-line files path 1)]
        (if (and first-panel-line panel-line (< panel-line first-panel-line))
          [0 0]
          (let [last-line (dec (count (:lines buffer)))]
            [last-line (count (line-text buffer last-line))])))

      :else nil)))

(defn- local-line-point
  "Page-local [x y] of the start of zero-based screen column `visual` on
  line `line` of `path`, halfway up the line, and the page column's code
  width."
  [{:keys [geometry files]} path line visual]
  (let [panel-line (page/origin-panel-line files path (inc line))
        [line-x line-y] (page/line-center geometry panel-line)
        {:keys [character-width code-characters]} geometry]
    [(+ line-x (* character-width (min visual code-characters))) line-y]))

;; Drawing the caret and the selection

(defonce ^:private unit-plane (three/PlaneGeometry. 1 1))

(defn- flat-material []
  (three/MeshBasicMaterial. #js {:transparent true :depthTest false :depthWrite false :opacity 0}))

(defn create-layer
  "Meshes for the caret and the selection, kept while editing goes on."
  []
  (let [^js caret (three/Mesh. unit-plane (flat-material))]
    (set! (.-renderOrder caret) 2)
    {:caret caret
     :selection (atom [])}))

(defn- selection-mesh! [{:keys [selection]} index]
  (or (get @selection index)
      (let [^js mesh (three/Mesh. unit-plane (flat-material))]
        (set! (.-renderOrder mesh) 2)
        (swap! selection conj mesh)
        mesh)))

(defn- set-color! [^js mesh [red green blue] opacity]
  (let [^js material (.-material mesh)]
    (.setRGB (.-color material) red green blue three/SRGBColorSpace)
    (set! (.-opacity material) opacity)
    (set! (.-visible mesh) (> opacity 0.003))))

(defn- adopt! [^js group ^js mesh]
  (when-not (identical? group (.-parent mesh))
    (when-let [^js parent (.-parent mesh)] (.remove parent mesh))
    (.add group mesh)))

(defn- hide-all! [{:keys [^js caret selection]}]
  (set! (.-visible caret) false)
  (doseq [^js mesh @selection] (set! (.-visible mesh) false)))

(defn- selection-spans
  "[line from-visual to-visual] for each line the selection covers, the
  newline at a line's end shown as half a column."
  [{:keys [lines] :as buffer}]
  (let [[[start-line start-column] [end-line end-column]] (editing/ordered buffer)]
    (for [line (range start-line (inc end-line))
          :let [text (get lines line "")
                from (editing/visual-column text (if (= line start-line) start-column 0) page/tab-size)
                to (editing/visual-column text (if (= line end-line) end-column (count text)) page/tab-size)]]
      [line from (if (< line end-line) (+ to 0.5) to)])))

(defn update-caret!
  "Draws the caret and the selection on the page being edited, the caret
  blinking while the keyboard is in the page and resting steady for a
  moment after each change. Hides both when nothing is being edited."
  [layer {:keys [page-object buffer theme-name code-colors focused? since-change viewport]}]
  (if-not (and page-object buffer (> (:text-share page-object) 0.4))
    (hide-all! layer)
    (let [{:keys [^js group visual pixels-per-unit opacity screen-y]} page-object
          {:keys [geometry]} visual
          {:keys [character-width code-characters]} geometry
          path (:path buffer)
          blink-period 1060
          steady-after-change 500
          most-spans 400
          ;; The caret is at least a pixel and a half wide.
          caret-width (max 0.09 (/ 1.5 (max 1 pixels-per-unit)))
          shown? (and focused? (or (< since-change steady-after-change)
                                   (< (mod since-change blink-period) (* 0.5 blink-period))))
          [caret-line caret-column] (:caret buffer)
          caret-visual (editing/visual-column (line-text buffer caret-line) caret-column page/tab-size)
          [caret-x caret-y] (local-line-point visual path caret-line caret-visual)
          ^js caret (:caret layer)
          on-screen? (fn [[_ local-y]]
                       (let [screen (- screen-y (* pixels-per-unit local-y))
                             margin pixels-per-unit]
                         (< (- margin) screen (+ (:height viewport) margin))))
          spans (when (editing/selected? buffer)
                  (take most-spans (filter #(on-screen? (local-line-point visual path (first %) 0))
                                           (selection-spans buffer))))]
      (adopt! group caret)
      (.set (.-position caret) caret-x caret-y 0.04)
      (.set (.-scale caret) caret-width 0.92 1)
      (set-color! caret (color/hex->rgb (:color (code-colors/style code-colors theme-name :caret)))
                  (if shown? (min 1 (* 1.2 opacity)) 0))
      (doseq [[index [line from to]] (map-indexed vector spans)]
        (let [^js mesh (selection-mesh! layer index)
              [left y] (local-line-point visual path line from)
              width (* character-width (- (min to code-characters) (min from code-characters)))]
          (adopt! group mesh)
          (.set (.-position mesh) (+ left (* 0.5 width)) y 0.01)
          (.set (.-scale mesh) (max 0.001 width) 1 1)
          (set-color! mesh (color/oklch->rgb (palette/color-of theme-name :selection)) (* 0.55 opacity))))
      (doseq [^js mesh (drop (count spans) @(:selection layer))]
        (set! (.-visible mesh) false)))))

(defn caret-screen-point
  "CSS pixel position of the caret's line start on screen, for placing
  the keyboard's input beside it, or nil."
  [{:keys [visual screen-x screen-y pixels-per-unit]} buffer]
  (when (and visual buffer)
    (let [[line column] (:caret buffer)
          visual-column (editing/visual-column (line-text buffer line) column page/tab-size)
          [x y] (local-line-point visual (:path buffer) line visual-column)]
      [(+ screen-x (* pixels-per-unit x)) (- screen-y (* pixels-per-unit y))])))

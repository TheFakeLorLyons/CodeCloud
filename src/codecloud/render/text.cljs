(ns codecloud.render.text
  "Text drawn into canvases for three.js textures: tiles of code in the
  editor's font, name bands, and the large names of the names-only view."
  (:require ["three" :as three]
            [clojure.string :as string]
            [codecloud.model.source :as source]
            [codecloud.presentation.code-colors :as code-colors]
            [codecloud.presentation.editing :as editing]
            [codecloud.presentation.highlight :as highlight]
            [codecloud.presentation.page :as page]
            [codecloud.presentation.palette :as palette]))

(def label-font-family
  "Typeface of the overlay's labels, bundled with the extension, with
  system fallbacks."
  "\"Recursive\", \"Segoe UI Variable Text\", \"Segoe UI\", system-ui, sans-serif")

(def default-code-font-family
  "\"Cascadia Code\", \"Cascadia Mono\", Consolas, \"DejaVu Sans Mono\", \"Courier New\", monospace")

(def tile-lines
  "Code lines in one tile texture."
  32)

(defn code-font-family
  "The editor's font family as CSS, with monospace fallbacks."
  [font-family]
  (if (string/blank? font-family)
    default-code-font-family
    (str font-family ", " default-code-font-family)))

(defn- canvas [width height]
  (let [^js element (js/document.createElement "canvas")]
    (set! (.-width element) (max 1 (js/Math.ceil width)))
    (set! (.-height element) (max 1 (js/Math.ceil height)))
    element))

(defn- texture-from-canvas [^js source-canvas]
  (let [^js texture (three/CanvasTexture. source-canvas)]
    (set! (.-colorSpace texture) three/SRGBColorSpace)
    (set! (.-anisotropy texture) 4)
    texture))

(defonce ^:private measuring-context
  (delay (.getContext (canvas 1 1) "2d")))

(defn- text-width
  "Width of `text` set in `font`, in canvas pixels."
  [font text]
  (let [^js context @measuring-context]
    (set! (.-font context) font)
    (.-width (.measureText context text))))

(defonce ^:private advances (atom {}))

(defn- measured-advance
  "Width of one character of `font` in canvas pixels, kept once the font
  has loaded."
  [font]
  (or (get @advances font)
      (let [sample-length 40
            advance (/ (text-width font (apply str (repeat sample-length "M"))) sample-length)]
        (when (.check (.-fonts js/document) font)
          (swap! advances assoc font advance))
        advance)))

(defn character-width
  "Advance of one character of the editor font, in editor lines."
  [{:keys [family size line-pixels]}]
  (let [reference-size 64
        advance (measured-advance (str reference-size "px " (code-font-family family)))]
    (/ (* advance (/ size reference-size)) line-pixels)))

(defn- code-font [{:keys [family size line-pixels]} level weight]
  (str weight " " (* level (/ size line-pixels)) "px " (code-font-family family)))

(defn- visible-runs
  "[kind text column depth] for each token of a line, tabs turned into
  the spaces reaching their stops, cut to `code-characters` screen columns
  with an ellipsis. The depth counts the brackets open at the token, from
  `depth` where the line starts."
  [tokens code-characters depth]
  (loop [remaining tokens
         column 0
         depth depth
         kept []]
    (if-let [[kind text :as token] (first remaining)]
      (let [shown (editing/expand-tabs text column page/tab-size)
            room (- code-characters column)
            [token-depth next-depth] (highlight/bracket-step depth token)]
        (cond
          (<= (count shown) room) (recur (rest remaining) (+ column (count shown)) next-depth
                                         (conj kept [kind shown column token-depth]))
          (pos? room) (conj kept [kind (str (subs shown 0 (max 0 (dec room))) "…") column token-depth])
          :else kept))
      kept)))

(defn tile-texture
  "{:texture} of panel lines `first-line` up to `end-line`, drawn at
  `level` canvas pixels per line on a clear background, in the colors and
  font styles `code-colors` gives on sky `theme-name`."
  [editor-font {:keys [gutter-characters code-characters]} lines first-line end-line level code-colors theme-name]
  (let [font (code-font editor-font level "500")
        advance (measured-advance font)
        width (* advance (+ gutter-characters code-characters))
        height (* level (- end-line first-line))
        ^js tile-canvas (canvas width height)
        ^js context (.getContext tile-canvas "2d")
        number-right (* advance (- gutter-characters 1.5))
        code-left (* advance gutter-characters)
        styled-font (fn [{:keys [italic? bold?]}]
                      (str (when italic? "italic ") (code-font editor-font level (if bold? "700" "500"))))
        ;; The canvas may write back a font in its own form, so the font
        ;; last set is kept here to set it again only when it changes.
        current-font (volatile! font)
        use-font! (fn [wanted]
                    (when (not= wanted @current-font)
                      (vreset! current-font wanted)
                      (set! (.-font context) wanted)))]
    (set! (.-font context) font)
    (set! (.-textBaseline context) "middle")
    (doseq [line-index (range first-line end-line)]
      (let [{:keys [tokens number bracket-depth family]} (get lines line-index)
            y (* level (+ 0.5 (- line-index first-line)))]
        (when number
          (use-font! font)
          (set! (.-fillStyle context) (:color (code-colors/style code-colors theme-name :gutter)))
          (set! (.-textAlign context) "right")
          (.fillText context (str number) number-right y))
        (set! (.-textAlign context) "left")
        (doseq [[kind shown column depth] (visible-runs tokens code-characters (or bracket-depth 0))]
          (when-not (= :space kind)
            (let [{:keys [color] :as token-style} (code-colors/style code-colors theme-name kind family)]
              (use-font! (styled-font token-style))
              (set! (.-fillStyle context) (or (when (= :bracket kind) (code-colors/bracket-color code-colors theme-name depth))
                                              color))
              (.fillText context shown (+ code-left (* advance column)) y))))))
    {:texture (texture-from-canvas tile-canvas)}))

(defn title-texture
  "{:texture :aspect} of a page's name band: its place on a ranked wall,
  its language tag, a light prefix, and its own name, with a dot when a
  file of the page has unsaved changes."
  [editor-font {:keys [rank tag prefix dirty?] page-leaf :leaf} level theme-name]
  (let [leaf (if dirty? (str page-leaf " ●") page-leaf)
        prefix-font (code-font editor-font level "400")
        leaf-font (code-font editor-font level "700")
        rank-text (if rank (str "#" rank "  ") "")
        tag-text (if tag (str tag "  ") "")
        rank-width (text-width leaf-font rank-text)
        leaf-width (text-width leaf-font leaf)
        tag-width (text-width prefix-font tag-text)
        prefix-width (text-width prefix-font prefix)
        height (* 1.4 level)
        middle (/ height 2)
        ^js title-canvas (canvas (+ rank-width tag-width prefix-width leaf-width 4) height)
        ^js context (.getContext title-canvas "2d")]
    (set! (.-textBaseline context) "middle")
    (set! (.-font context) leaf-font)
    (set! (.-fillStyle context) (palette/hex-of theme-name :ink-soft))
    (.fillText context rank-text 0 middle)
    (set! (.-font context) prefix-font)
    (.fillText context tag-text rank-width middle)
    (.fillText context prefix (+ rank-width tag-width) middle)
    (set! (.-font context) leaf-font)
    (set! (.-fillStyle context) (palette/hex-of theme-name :ink))
    (.fillText context leaf (+ rank-width tag-width prefix-width) middle)
    {:texture (texture-from-canvas title-canvas)
     :aspect (/ (.-width title-canvas) height)}))

(defn name-texture
  "{:texture :aspect} of a page's name for the names-only view."
  [editor-font label-parts theme-name]
  (let [level 72]
    (title-texture editor-font label-parts level theme-name)))

(defn tokenize-source
  "Highlighted lines of a source text, capped so a huge file cannot stall
  the view."
  [language-id text]
  (let [line-cap 20000]
    (highlight/tokenize-lines language-id (vec (take line-cap (source/split-lines text))))))

(ns codecloud.render.problems
  "Problems VS Code reports in the cloud's files, such as clj-kondo's
  through Calva or clojure-lsp, marked on the pages showing them with a
  line under the code each problem covers, in the color of its
  severity."
  (:require ["three" :as three]
            [codecloud.presentation.color :as color]
            [codecloud.presentation.page :as page]
            [codecloud.presentation.palette :as palette]
            [codecloud.render.editing-view :as editing-view]
            [codecloud.render.pages :as pages]
            [codecloud.render.planes :as planes]))

(defn create-layer
  "The meshes marking problems, made as they are needed and kept."
  []
  {:marks (atom [])})

(def ^:private severity-colors
  {:error :problem-error :warning :problem-warning :information :problem-information})

(defn- spans
  "[line from to] for each line `problem` covers, at most three, in screen
  columns. A problem with no width takes one character."
  [{:keys [line column end-line end-column]} line-length]
  (let [most-lines 3]
    (for [covered (range line (inc (min end-line (+ line most-lines -1))))
          :let [from (if (= covered line) column 0)
                to (if (= covered end-line) end-column (max (inc from) (line-length covered)))]]
      [covered from (max to (inc from))])))

(defn update-marks!
  "Marks the problems in `diagnostics`, a map of each file's path to its
  problems, on every page on screen whose code shows as text, hints left
  unmarked. Lines being edited are read from their buffers."
  [layer page-layer {:keys [diagnostics buffers]} theme-name]
  (let [most-marks 400
        text-showing 0.4
        placed (when (seq diagnostics)
                 (take most-marks
                       (for [{:keys [visual] :as page-object} (pages/all-pages page-layer)
                             :when (and (:on-screen? page-object) (> (:text-share page-object 0) text-showing) visual)
                             {:keys [path]} (:files visual)
                             :let [problems (get diagnostics path)
                                   lines (get-in buffers [path :lines])
                                   line-length (fn [line] (count (get lines line "")))]
                             problem (remove #(= :hint (:severity %)) problems)
                             [line from to] (spans problem line-length)
                             :when (page/origin-panel-line (:files visual) path (inc line))]
                         [page-object path line from to (:severity problem)])))]
    (doseq [[index [{:keys [^js group visual pixels-per-unit opacity]} path line from to severity]] (map-indexed vector placed)]
      (let [^js mesh (planes/pooled-mesh! (:marks layer) index)
            {:keys [character-width code-characters]} (:geometry visual)
            [left y] (editing-view/local-line-point visual path line from)
            width (* character-width (- (min to code-characters) (min from code-characters)))
            ;; A third of a pixel short of two pixels thick, and just below
            ;; the letters.
            thickness (max 0.07 (/ 1.66 (max 1 pixels-per-unit)))
            ^js material (.-material mesh)]
        (when-not (identical? group (.-parent mesh))
          (when-let [^js parent (.-parent mesh)] (.remove parent mesh))
          (.add group mesh))
        (.set (.-position mesh) (+ left (* 0.5 width)) (- y 0.44) 0.03)
        (.set (.-scale mesh) (max 0.001 width) thickness 1)
        (let [[red green blue] (color/oklch->rgb (palette/color-of theme-name (get severity-colors severity :problem-information)))]
          (.setRGB (.-color material) red green blue three/SRGBColorSpace))
        (set! (.-opacity material) (* 0.9 opacity))
        (set! (.-visible mesh) (> width 0))))
    (doseq [^js mesh (drop (count placed) @(:marks layer))]
      (set! (.-visible mesh) false))))

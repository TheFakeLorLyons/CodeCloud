(ns codecloud.presentation.page
  "A page's shape in local coordinates, origin at its center, y up. One
  world unit is one editor line, and long files wrap into columns."
  (:require [codecloud.model.source :as source]))

(def padding
  "World units between the page's edge and its contents."
  1.4)

(def header-height
  "Height of the band at the top of a page that names the namespace."
  2.4)

(def column-gap
  "World units between columns of code."
  3.0)

(def tab-size
  "Columns between tab stops in code, the editor's usual width."
  4)

(defn panel-lines
  "Lines a page shows: every file's lines, plus a heading line before each
  file when the namespace spans several files."
  [files]
  (+ (reduce + 0 (map #(:line-count % 0) files))
     (if (> (count files) 1) (count files) 0)))

(defn- column-count
  "Columns for `line-count` lines of `column-width`, as many as keep the
  page no more than about one and a half times taller than wide."
  [line-count column-width]
  (let [tallest-ratio 1.6]
    (max 1 (Math/round (Math/sqrt (/ line-count (* tallest-ratio column-width)))))))

(defn geometry
  "Layout of a page for `files`, each with :line-count and :longest-line.
  `character-width` is the font's advance over its line height. A page
  being edited keeps `fixed-columns`, so its lines never jump between
  columns as it grows."
  ([files character-width] (geometry files character-width nil))
  ([files character-width fixed-columns]
   (let [;; Short files still read as pages, and longer lines end in an ellipsis.
         shortest-column 24
         widest-column 100
         line-count (max 1 (panel-lines files))
         longest-line (reduce max 0 (map #(:longest-line % 80) files))
         code-characters (max shortest-column (min widest-column longest-line))
         largest-line-number (reduce max 1 (map #(:line-count % 0) files))
         gutter-characters (+ 2 (max 3 (count (str largest-line-number))))
         column-width (* character-width (+ gutter-characters code-characters))
         columns (or fixed-columns (column-count line-count column-width))
         rows (int (Math/ceil (/ line-count columns)))
         width (+ (* 2 padding) (* columns column-width) (* (dec columns) column-gap))
         height (+ (* 2 padding) header-height rows)]
     {:line-count line-count
      :columns columns
      :rows rows
      :code-characters code-characters
      :gutter-characters gutter-characters
      :character-width character-width
      :column-width column-width
      :width width
      :height height
      :radius (* 0.5 (Math/sqrt (+ (* width width) (* height height))))})))

(defn code-top
  "Page-local y of the top edge of the first code row."
  [{:keys [height]}]
  (- (/ height 2) padding header-height))

(defn column-left
  "Page-local x of the left edge of column `column`, where its line
  numbers start."
  [{:keys [width column-width]} column]
  (+ (- (/ width 2)) padding (* column (+ column-width column-gap))))

(defn line-center
  "Page-local [x y] at the start of the code of zero-based panel line
  `panel-line`, halfway up the line."
  [{:keys [rows gutter-characters character-width] :as page-geometry} panel-line]
  (let [column (quot panel-line rows)
        row (mod panel-line rows)]
    [(+ (column-left page-geometry column) (* gutter-characters character-width))
     (- (code-top page-geometry) row 0.5)]))

(defn line-at
  "Zero-based panel line under the page-local point [x y], or nil when the
  point is outside every column of code."
  [{:keys [rows columns column-width line-count] :as page-geometry} [x y]]
  (let [row (int (Math/floor (- (code-top page-geometry) y)))
        column-offset (- x (column-left page-geometry 0))
        column (int (Math/floor (/ column-offset (+ column-width column-gap))))
        within-column? (< (- column-offset (* column (+ column-width column-gap))) column-width)
        panel-line (+ (* column rows) row)]
    (when (and (<= 0 row) (< row rows)
               (<= 0 column) (< column columns)
               (not (neg? column-offset))
               within-column?
               (< panel-line line-count))
      panel-line)))

(defn line-near
  "Zero-based panel line nearest the page-local point [x y], within the
  page's code, for a drag that leaves the lines."
  [{:keys [rows columns column-width line-count] :as page-geometry} [x y]]
  (let [row (max 0 (min (dec rows) (int (Math/floor (- (code-top page-geometry) y)))))
        column-offset (- x (column-left page-geometry 0))
        column (max 0 (min (dec columns) (int (Math/floor (/ (+ column-offset (* 0.5 column-gap))
                                                                  (+ column-width column-gap))))))]
    (max 0 (min (dec line-count) (+ (* column rows) row)))))

(defn panel-line-origin
  "{:path :line} that zero-based panel line `panel-line` shows, with
  :heading? true on the line naming a file, or nil past the end."
  [files panel-line]
  (let [headings? (> (count files) 1)]
    (loop [remaining files
           first-panel-line 0]
      (when-let [{:keys [path line-count]} (first remaining)]
        (let [heading-lines (if headings? 1 0)
              next-first (+ first-panel-line heading-lines (or line-count 0))]
          (cond
            (>= panel-line next-first) (recur (rest remaining) next-first)
            (< panel-line (+ first-panel-line heading-lines)) {:path path :line 1 :heading? true}
            :else {:path path :line (inc (- panel-line first-panel-line heading-lines))}))))))

(defn origin-panel-line
  "Zero-based panel line showing one-based `line` of the file at `path`,
  or nil when the page does not hold it."
  [files path line]
  (let [headings? (> (count files) 1)]
    (loop [remaining files
           first-panel-line 0]
      (when-let [file (first remaining)]
        (let [heading-lines (if headings? 1 0)
              line-count (or (:line-count file) 0)]
          (if (= path (:path file))
            (+ first-panel-line heading-lines (dec (max 1 (min line (max 1 line-count)))))
            (recur (rest remaining) (+ first-panel-line heading-lines line-count))))))))

(defn line-length-metrics
  "{:line-count :longest-line} of a file's lines, as the analysis measures
  them."
  [lines]
  {:line-count (count lines)
   :longest-line (reduce max 0 (map count lines))})

(defn named-by-path?
  "True for a page named by its path: a script without a namespace, or a
  file of another language."
  [{:keys [kind]}]
  (contains? #{:script :module} kind))

(defn language-of
  "A page's language, Clojure for a page that names none."
  [node]
  (:language node :clojure))

(defn primary-file
  "The file a page is named after, else its first: a C header's page is
  its source file's."
  [{:keys [id files]}]
  (or (some #(when (= id (:path %)) %) files) (first files)))

(defn directory
  "The folder of a page's primary file, relative to the cloud's root."
  [node]
  (source/directory-of (or (:path (primary-file node)) "")))

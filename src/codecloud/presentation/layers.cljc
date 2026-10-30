(ns codecloud.presentation.layers
  "Color layers. Each colors pages by one kind of information, and a page
  blends the colors of every layer turned on, mixed in Oklab. A page no
  layer colors stays gray.

  A layer has :key and :label; :prepare, from a snapshot to a context;
  :color, from the context and a page to {:day :night} Oklch or nil;
  :describe, from the same to a few words; and :legend, from the context
  to [{:label :day :night :target}] for :kind :category, or {:low :high
  :low-label :high-label :low-target :high-target :unclassified} for
  :heat. Optional keys: :weight; :recolor, from the context and chosen
  colors to the context; :entry, from the context and a page to what a
  chosen color for it replaces; and :entry-color, from the context and
  such an entry to its color. Any such map in `built-in` gains a
  checkbox."
  (:require [clojure.string :as string]
            [codecloud.collections :as collections]
            [codecloud.languages.catalog :as catalog]
            [codecloud.model.selection :as selection]
            [codecloud.presentation.color :as color]
            [codecloud.presentation.page :as page]
            [codecloud.presentation.palette :as palette]))

;; Blending

(def ^:private reserved-reds
  "The stretch of hues around the reds of cycles and parse errors, which
  no mix of other colors lands in."
  [15 45])

(defn- reserved-red? [hue]
  (let [[least most] reserved-reds]
    (< least hue most)))

(defn- clear-of-reserved-reds
  "`hue`, or the nearer edge of the reserved reds when a mix of `parts`,
  none red, lands among them."
  [hue parts]
  (let [[least most] reserved-reds]
    (if (and (reserved-red? hue) (not-any? (fn [[_ [_ _ part-hue]]] (reserved-red? part-hue)) parts))
      (if (< hue (/ (+ least most) 2)) least most)
      hue)))

(defn blend
  "[weight oklch] pairs mixed in Oklab, or nil without weight. Half the
  chroma opposing hues cancel is kept, and mixes of other colors stay out
  of the reserved reds."
  [weighted-colors]
  (let [present (filter (fn [[weight oklch]] (and oklch (pos? weight))) weighted-colors)
        total (reduce + 0 (map first present))]
    (when (pos? total)
      (let [labs (map (fn [[weight oklch]] [weight (color/oklch->oklab oklch)]) present)
            average (fn [index] (/ (reduce + 0 (map (fn [[weight lab]] (* weight (nth lab index))) labs)) total))
            [lightness green-red blue-yellow] (mapv average [0 1 2])
            mixed-chroma (Math/sqrt (+ (* green-red green-red) (* blue-yellow blue-yellow)))
            mean-chroma (/ (reduce + 0 (map (fn [[weight [_ chroma]]] (* weight chroma)) present)) total)
            kept-share 0.5
            canceled-below 0.0001]
        [lightness
         (+ mixed-chroma (* kept-share (- mean-chroma mixed-chroma)))
         (if (> mixed-chroma canceled-below)
           (clear-of-reserved-reds (nth (color/oklab->oklch [lightness green-red blue-yellow]) 2) present)
           (nth (second (apply max-key first present)) 2))]))))

(defn- blend-themes
  "{:day :night} blending [weight {:day :night}] pairs theme by theme."
  [weighted]
  (when (seq weighted)
    {:day (blend (map (fn [[weight colors]] [weight (:day colors)]) weighted))
     :night (blend (map (fn [[weight colors]] [weight (:night colors)]) weighted))}))

;; Folders

(defn folder-key
  "The folders a path names, outermost first."
  [path]
  (vec (remove string/blank? (string/split path #"/"))))

(defn folder-path
  "The folders holding a page's first file, outermost first."
  [node]
  (folder-key (page/directory node)))

(defn folder-name
  "A folder path written with slashes, or top level for the root."
  [folders]
  (if (empty? folders) "top level" (string/join "/" folders)))

(defn- tree-order
  "A key that sorts folder paths the way a file tree lists them, each
  folder just before the folders inside it."
  [folders]
  (string/join "\u0000" folders))

(defn- tone
  "{:day :night} for `hue` at the lightness step `step`, which alternates
  light and dark so neighbors differ in more than hue."
  [hue step]
  (let [day-lightness [0.54 0.68 0.46 0.61 0.74]
        night-lightness [0.72 0.86 0.64 0.79 0.92]
        index (mod step (count day-lightness))
        day-chroma 0.15
        night-chroma 0.14]
    {:day [(nth day-lightness index) day-chroma hue]
     :night [(nth night-lightness index) night-chroma hue]}))

(defn folder-colors
  "Folder path to {:day :night}. Hues spread evenly in file-tree order,
  subfolders following their parent, with alternating lightness. Reds
  stay free for cycles and parse errors."
  [snapshot]
  (let [ordered (sort-by tree-order (distinct (map folder-path (vals (:namespaces snapshot)))))
        folder-count (count ordered)
        first-hue 65
        hue-range 270]
    (into {}
          (map-indexed (fn [index folders]
                         [folders (tone (+ first-hue (* hue-range (/ (+ index 0.5) folder-count))) index)]))
          ordered)))

(defn- largest-folders
  "The `most` folders holding the most pages, as [folders page-count]."
  [snapshot most]
  (->> (frequencies (map folder-path (vals (:namespaces snapshot))))
       (sort-by (fn [[folders page-count]] [(- page-count) (tree-order folders)]))
       (take most)))

(defn- with-chosen-folders
  "A folder or dependents layer's context with the folder colors chosen
  in `overrides`."
  [context overrides]
  (update context :colors merge
          (into {} (map (fn [[path colors]] [(folder-key path) colors])) (get overrides :folder))))

(defn- folder-context
  "What the folder and dependents layers work out: every folder's color,
  each page's folders, and the largest folders, which their legends
  list."
  [snapshot]
  (let [listed 8]
    {:colors (folder-colors snapshot)
     :folders (into {} (map (fn [[id node]] [id (folder-path node)])) (:namespaces snapshot))
     :largest (largest-folders snapshot listed)}))

;; Languages

(defn- color-family
  "The Language layer entry for `language-id`: its family, or
  :unclassified for a language read only as text."
  [language-id]
  (if (catalog/analyzed? language-id) (catalog/language-family language-id) :unclassified))

(defn- language-families
  "The snapshot's Language layer entries, most pages first and
  unclassified languages last, as {:family :label}, :label naming each
  language present."
  [snapshot]
  (->> (frequencies (map page/language-of (vals (:namespaces snapshot))))
       (group-by (comp color-family key))
       (map (fn [[family members]]
              (let [ordered (sort-by (fn [[language-id page-count]] [(- page-count) (name language-id)]) members)
                    named (collections/spoken-list (map (comp :label catalog/language key) ordered))]
                {:family family
                 :label (if (= :unclassified family) (str "Unclassified: " named) named)
                 :page-count (reduce + (map val members))})))
       (sort-by (juxt #(= :unclassified (:family %)) (comp - :page-count) :label))
       (mapv #(dissoc % :page-count))))

(defn- family-color
  "{:day :night} of a Language layer entry: the color chosen for it, or
  its accent."
  [{:keys [chosen]} family]
  (or (get chosen family)
      {:day (palette/language-accent :day family) :night (palette/language-accent :night family)}))

;; Built-in layers

(def language-layer
  {:key :language
   :label "Language"
   :kind :category
   :prepare (fn [snapshot] {:families (language-families snapshot)})
   :recolor (fn [context overrides] (assoc context :chosen (get overrides :language)))
   :color (fn [context node] (family-color context (color-family (page/language-of node))))
   :entry (fn [_context node] (color-family (page/language-of node)))
   :entry-color family-color
   :describe (fn [_context node]
               (let [language-id (page/language-of node)
                     language-label (:label (catalog/language language-id))]
                 (if (catalog/analyzed? language-id) language-label (str language-label ", unclassified"))))
   :legend (fn [{:keys [families] :as context}]
             (for [{:keys [family label]} families]
               (merge {:label label :target [:language family]} (family-color context family))))})

(def folder-layer
  {:key :folder
   :label "Folder"
   :kind :category
   :prepare folder-context
   :recolor with-chosen-folders
   :color (fn [{:keys [colors]} node] (get colors (folder-path node)))
   :entry (fn [_context node] (string/join "/" (folder-path node)))
   :entry-color (fn [{:keys [colors]} entry] (get colors (folder-key entry)))
   :describe (fn [_context node] (folder-name (folder-path node)))
   :legend (fn [{:keys [colors largest]}]
             (for [[folders] largest]
               (merge {:label (folder-name folders) :target [:folder (string/join "/" folders)]}
                      (get colors folders))))})

(defn- linked-pages
  "The pages that use `node`, or when none does, the pages it uses, with
  the word that says which, as [word ids]."
  [{:keys [id required-by requires]}]
  (let [users (remove #(= id %) required-by)]
    (if (seq users)
      ["used by" users]
      ["uses" (remove #(= id %) requires)])))

(def dependents-layer
  {:key :dependents
   :label "Dependents"
   :kind :category
   :prepare folder-context
   :recolor with-chosen-folders
   :color (fn [{:keys [colors folders]} node]
            (let [[_ linked] (linked-pages node)]
              (blend-themes (keep #(some->> (get folders %) (get colors) (vector 1)) linked))))
   :describe (fn [{:keys [folders]} node]
               (let [[word linked] (linked-pages node)
                     listed 3
                     counts (sort-by (fn [[linked-folder page-count]] [(- page-count) linked-folder])
                                     (frequencies (keep #(some-> (get folders %) folder-name) linked)))]
                 (when (seq counts)
                   (str word " " (string/join ", " (map (fn [[linked-folder page-count]] (str linked-folder " " page-count))
                                                       (take listed counts)))))))
   :legend (fn [{:keys [colors largest]}]
             (for [[folders] largest]
               (merge {:label (str "used by " (folder-name folders)) :target [:folder (string/join "/" folders)]}
                      (get colors folders))))})

(defn- measurement
  "A page's `metric`: its importance, or one of its measurements."
  [metric node]
  (if (= :importance metric)
    (:importance node 0)
    (get-in node [:metrics metric] 0)))

(def ^:private text-metrics
  "Measurements every page has, whatever its language. The rest need an
  adapter to read the page."
  #{:lines :flags})

(defn- measured?
  "True when `metric` means something for `node`: a measurement of text,
  or any measurement of a page in an analyzed language."
  [metric node]
  (or (contains? text-metrics metric) (catalog/analyzed? (page/language-of node))))

(defn- rounded
  "`value` to two decimal places."
  [value]
  (let [hundredths 100.0]
    (/ (Math/round (* hundredths value)) hundredths)))

(defn heat-layer
  "A layer shading pages from `low` to `high` by where their `metric`
  falls in the snapshot's range, along the logarithm with :log-scale?.
  Pages the metric cannot measure take the unclassified color."
  [{layer-key :key :keys [label metric low high log-scale? unit low-label high-label]}]
  (let [scaled (fn [node]
                 (let [value (max 0 (measurement metric node))]
                   (if log-scale? (Math/log (inc value)) value)))]
    {:key layer-key
     :label label
     :kind :heat
     :prepare (fn [snapshot]
                (let [nodes (vals (:namespaces snapshot))
                      values (map scaled (filter #(measured? metric %) nodes))]
                  {:least (if (seq values) (reduce min values) 0)
                   :most (reduce max 0 values)
                   :low low
                   :high high
                   :unclassified? (not-every? #(measured? metric %) nodes)}))
     :recolor (fn [context overrides] (merge context (select-keys (get overrides layer-key) [:low :high])))
     :color (fn [{:keys [least most] ramp-low :low ramp-high :high} node]
              (if (measured? metric node)
                (let [share (if (> most least) (/ (- (scaled node) least) (- most least)) 0)]
                  {:day (color/mix-oklch (:day ramp-low) (:day ramp-high) share)
                   :night (color/mix-oklch (:night ramp-low) (:night ramp-high) share)})
                palette/unclassified))
     :entry-color (fn [context entry] (get context entry))
     :describe (fn [_context node]
                 (if (measured? metric node)
                   (let [value (rounded (measurement metric node))]
                     (str (if (== value (Math/round value)) (Math/round value) value) (when unit (str " " unit))))
                   "unclassified"))
     :legend (fn [{:keys [unclassified?] :as context}]
               (cond-> {:low (:low context) :high (:high context)
                        :low-label (or low-label "less") :high-label (or high-label "more")
                        :low-target [layer-key :low] :high-target [layer-key :high]}
                 unclassified? (assoc :unclassified palette/unclassified)))}))

(defn- theme-pair
  "[low high] ramp ends, each {:day :night}, from two palette roles."
  [low-key high-key]
  [{:day (palette/color-of :day low-key) :night (palette/color-of :night low-key)}
   {:day (palette/color-of :day high-key) :night (palette/color-of :night high-key)}])

(def importance-layer
  (let [[low high] (theme-pair :importance-low :importance-high)]
    (heat-layer {:key :importance :label "Importance" :metric :importance :low low :high high
                 :low-label "minor" :high-label "central"})))

(def complection-layer
  (let [[low high] (theme-pair :complection-low :complection-high)]
    (heat-layer {:key :complection :label "Complection" :metric :complection :low low :high high
                 :unit "points" :low-label "plain" :high-label "tangled"})))

(def length-layer
  (let [[low high] (theme-pair :length-low :length-high)]
    (heat-layer {:key :length :label "Length" :metric :lines :log-scale? true :low low :high high
                 :unit "lines" :low-label "short" :high-label "long"})))

(def built-in
  "The layers every cloud offers, in the order the panel lists them."
  [language-layer folder-layer dependents-layer importance-layer complection-layer length-layer])

;; Layers from the cloud file

(def named-colors
  "Colors a cloud file may name, each {:day :night}."
  {"red" {:day [0.58 0.19 28] :night [0.70 0.17 28]}
   "orange" {:day [0.66 0.17 52] :night [0.78 0.15 55]}
   "yellow" {:day [0.80 0.15 98] :night [0.90 0.14 100]}
   "green" {:day [0.58 0.16 145] :night [0.72 0.16 148]}
   "teal" {:day [0.64 0.12 185] :night [0.80 0.12 188]}
   "blue" {:day [0.52 0.16 258] :night [0.68 0.15 258]}
   "purple" {:day [0.55 0.17 300] :night [0.76 0.14 303]}
   "pink" {:day [0.62 0.17 345] :night [0.78 0.14 348]}
   "brown" {:day [0.48 0.10 60] :night [0.62 0.10 62]}
   "gray" {:day [0.60 0.01 250] :night [0.72 0.01 250]}})

(defn color-named
  "{:day :night} for a named color or #rrggbb, or nil. Hex colors brighten
  enough to read at night."
  [text]
  (when (string? text)
    (or (get named-colors (string/lower-case text))
        (when (re-matches #"#[0-9a-fA-F]{6}" text)
          (let [oklch (color/rgb->oklch (color/hex->rgb text))
                least-night-lightness 0.66]
            {:day oklch :night (update oklch 0 max least-night-lightness)})))))

(defn- pattern-layer
  "A layer that colors the pages with a file matching `patterns` in
  `color` and leaves the rest to other layers."
  [{layer-key :key :keys [label patterns color]}]
  (let [scope (selection/compile-selection {:include patterns :ignore []})
        matches? (fn [node] (some #(selection/selected? scope (:path %)) (:files node)))]
    {:key layer-key
     :label label
     :kind :category
     :prepare (fn [_snapshot] {:color color})
     :recolor (fn [context overrides] (merge context (select-keys (get overrides layer-key) [:color])))
     :color (fn [context node] (when (matches? node) (:color context)))
     :entry (fn [_context node] (when (matches? node) :color))
     :entry-color (fn [context _entry] (:color context))
     :describe (fn [_context node] (when (matches? node) "matches"))
     :legend (fn [context] [(merge {:label label :target [layer-key :color]} (:color context))])}))

(defn custom-layer
  "The layer a validated cloud file entry describes: a heat layer for
  {:metric :colors}, or a pattern layer for {:patterns :colors}."
  [{layer-key :key :keys [label metric colors patterns]}]
  (if metric
    (let [[low high] (map color-named colors)]
      (heat-layer {:key layer-key :label label :metric metric :low low :high high
                   :log-scale? (= :lines metric) :low-label "less" :high-label "more"}))
    (pattern-layer {:key layer-key :label label :patterns patterns :color (color-named (first colors))})))

(defn registry
  "Every layer a cloud offers: the built-in ones, then those its cloud
  file declares."
  [custom-specs]
  (into built-in (map custom-layer) custom-specs))

;; Using layers

(defn prepare
  "Map of each layer's key to what it works out for `snapshot`, taking the
  colors chosen in `overrides`, {layer-key {entry {:day :night}}}, over its
  own."
  ([layers snapshot] (prepare layers snapshot {}))
  ([layers snapshot overrides]
   (into {}
         (map (fn [{layer-key :key prepare-context :prepare recolor :recolor}]
                (let [context (prepare-context snapshot)]
                  [layer-key (if (and recolor (seq overrides)) (recolor context overrides) context)])))
         layers)))

(defn default-keys
  "Layers on by default: folder and dependents, plus language once a tenth
  of the pages are outside the most common language."
  [snapshot]
  (let [counts (vals (frequencies (map page/language-of (vals (:namespaces snapshot)))))
        page-count (reduce + 0 counts)
        other-pages (- page-count (reduce max 0 counts))
        least-other-share 0.1]
    (if (and (pos? other-pages) (>= other-pages (* least-other-share page-count)))
      [:language :folder :dependents]
      [:folder :dependents])))

(defn layer-color
  "The color one layer gives `node`, or nil."
  [{:keys [color]} context node]
  (color context node))

(defn page-color
  "A page's {:day :night} color blended from the layers whose keys are in
  `enabled`, or nil when none of them colors it."
  [layers contexts enabled node]
  (blend-themes (keep (fn [{layer-key :key :keys [weight] :as layer}]
                        (when (contains? enabled layer-key)
                          (some->> (layer-color layer (get contexts layer-key) node)
                                   (vector (or weight 1)))))
                      layers)))

(defn page-colors
  "Map of page id to its blended {:day :night} color, nil for a page no
  enabled layer colors."
  [layers contexts enabled snapshot]
  (into {} (map (fn [[id node]] [id (page-color layers contexts enabled node)])) (:namespaces snapshot)))

(defn folder-hues
  "Map of page id to the hue of its folder's color by day, which the
  folder's cloud takes."
  [contexts snapshot]
  (let [colors (get-in contexts [:folder :colors])]
    (into {}
          (keep (fn [[id node]] (when-let [day (:day (get colors (folder-path node)))] [id (nth day 2)])))
          (:namespaces snapshot))))

(defn cloud-tint
  "How strongly folder clouds take their folders' colors: :strong while
  the Folder layer colors the pages alone, :slight under other layers, or
  nil once the viewer turns tinting off."
  [active-layers tint-clouds?]
  (cond
    (not tint-clouds?) nil
    (= #{:folder} (set active-layers)) :strong
    :else :slight))

(defn breakdown
  "What each enabled layer says about `node`, in registry order, as
  {:key :label :color :description}, with the :target a chosen color for
  this page would take when the layer has one."
  [layers contexts enabled node]
  (for [{layer-key :key :keys [label describe entry] :as layer} layers
        :when (contains? enabled layer-key)]
    (let [context (get contexts layer-key)
          chosen-entry (when entry (entry context node))]
      (cond-> {:key layer-key
               :label label
               :color (layer-color layer context node)
               :description (describe context node)}
        (some? chosen-entry) (assoc :target [layer-key chosen-entry])))))

(defn target-color
  "The {:day :night} color a chooser target [layer-key entry] shows now, or
  nil."
  [layers contexts [layer-key entry]]
  (when-let [entry-color (some #(when (= layer-key (:key %)) (:entry-color %)) layers)]
    (entry-color (get contexts layer-key) entry)))

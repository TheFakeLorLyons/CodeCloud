(ns codecloud.presentation.code-colors
  "Colors and font styles of code on pages for both skies, CodeCloud's
  own or an editor theme's. A theme gives each kind of token a color and
  style through its TextMate rules and the editor's customizations, and
  each color moves in lightness until it reads on every page's glass."
  (:require [clojure.string :as string]
            [codecloud.presentation.color :as color]
            [codecloud.presentation.palette :as palette]))

;; Code colors map each sky, :day and :night, to a map from token kind to
;; {:color :italic? :bold?}, the color as hex. A sky also holds :gutter
;; for line numbers, :caret, :bracket-levels for bracket pair colors by
;; depth, and :languages for kinds a language family's grammar scopes its
;; own way.

(def own
  "CodeCloud's own code colors."
  (into {}
        (map (fn [sky]
               (let [kind-colors (update-vals (get palette/syntax-colors sky)
                                              (fn [oklch] {:color (color/oklch->hex oklch)}))]
                 [sky (assoc kind-colors :caret (:text kind-colors))])))
        [:day :night]))

(defn style
  "{:color :italic? :bold?} of `token-kind` on `sky`, as code of
  `language-family` shows it when given, plain text's for a kind without
  its own."
  ([code-colors sky token-kind] (style code-colors sky token-kind nil))
  ([code-colors sky token-kind language-family]
   (let [sky-colors (get code-colors sky)]
     (or (get-in sky-colors [:languages language-family token-kind])
         (get sky-colors token-kind)
         (:text sky-colors)))))

(defn bracket-color
  "Hex color of a bracket at nesting `depth` on `sky`, or nil when brackets
  share one color."
  [code-colors sky depth]
  (let [levels (get-in code-colors [sky :bracket-levels])]
    (when (seq levels)
      (nth levels (mod depth (count levels))))))

;; Reading a theme

(def ^:private kind-scopes
  "TextMate scopes for each token kind, tried in order until the theme
  colors one; a kind none colors takes the theme's foreground."
  {:comment ["comment.line"]
   :discard ["comment.block"]
   :string ["string.quoted"]
   :number ["constant.numeric"]
   :constant ["constant.language"]
   :keyword ["support.type" "storage.type"]
   :special ["keyword" "storage" "keyword.control"]
   :definition ["entity.name.function" "entity.name"]
   :reader ["keyword.control.directive" "meta.preprocessor" "keyword.other"]
   :bracket ["punctuation.section.brackets" "punctuation"]})

(def ^:private language-kind-scopes
  "Scopes for the kinds a family's grammar names its own way. Clojure's
  grammar scopes def forms and ns as keyword.control and its other special
  forms as storage.control, so pages use the first for both, and it
  leaves reader macros and discards unscoped."
  {:clojure {:keyword ["constant.keyword"]
             :special ["keyword.control" "storage.control"]
             :definition ["entity.global"]
             :reader []
             :discard []}
   :c {:keyword ["storage.type.built-in" "storage.type" "support.type"]}
   :shell {:keyword ["support.function.builtin" "support.function"]}
   :css {:keyword ["support.type.property-name" "support.type"]}})

(def ^:private shorthand-scopes
  "The scopes of each shorthand in editor.tokenColorCustomizations, in
  the order the editor applies them."
  [["comments" ["comment" "punctuation.definition.comment"]]
   ["strings" ["string" "meta.embedded.assembly"]]
   ["keywords" ["keyword - keyword.operator" "keyword.control" "storage" "storage.type"]]
   ["numbers" ["constant.numeric"]]
   ["types" ["entity.name.type" "entity.name.class" "support.type" "support.class"]]
   ["functions" ["entity.name.function" "support.function"]]
   ["variables" ["variable" "entity.name.variable"]]])

(def ^:private editor-defaults
  "The editor's own colors for a dark or light theme that leaves them
  out: code's foreground and background, and the first three bracket pair
  colors."
  {:dark {:foreground "#d4d4d4" :background "#1e1e1e" :brackets ["#ffd700" "#da70d6" "#179fff"]}
   :light {:foreground "#000000" :background "#ffffff" :brackets ["#0431fa" "#319331" "#7b3814"]}})

(defn- byte-value [two-digits]
  #?(:clj (Integer/parseInt two-digits 16)
     :cljs (js/parseInt two-digits 16)))

(defn- theme-rgb
  "RGB of a theme color written #rgb, #rgba, #rrggbb, or #rrggbbaa, laid
  over `background` by its alpha, or nil for no color or a transparent
  one."
  [text background]
  (when-let [[_ digits] (when (string? text)
                          (re-matches #"#([0-9a-fA-F]{8}|[0-9a-fA-F]{6}|[0-9a-fA-F]{3,4})" text))]
    (let [full-digits (if (<= (count digits) 4) (apply str (mapcat #(list % %) digits)) digits)
          rgb (color/hex->rgb (subs full-digits 0 6))
          opacity (if (= 8 (count full-digits)) (/ (byte-value (subs full-digits 6 8)) 255) 1)]
      (cond
        (zero? opacity) nil
        (and background (< opacity 1)) (palette/mix-rgb background rgb opacity)
        :else rgb))))

(defn- prefix-depth
  "How many dotted names `selector` has when it names `scope` or a scope
  `scope` refines, else nil."
  [selector scope]
  (when (or (= selector scope) (string/starts-with? scope (str selector ".")))
    (inc (count (re-seq #"\." selector)))))

(defn- selector-entry
  "{:scope :excluded} of one selector of a rule, or nil for one naming
  enclosing scopes, which tokens on a page do not carry."
  [selector]
  (let [[included & excluded] (map string/trim (string/split selector #"\s-\s"))]
    (when (and (seq included) (not (re-find #"\s" included)))
      {:scope included :excluded excluded})))

(defn- rule-entries
  "Rules as {:selectors :settings :order}, counted from the first, a
  rule's scope being a string of comma-separated selectors or a list."
  [rules]
  (map-indexed (fn [order rule]
                 (let [scope (when (map? rule) (get rule "scope"))
                       settings (when (map? rule) (get rule "settings"))]
                   {:selectors (keep selector-entry (filter string? (if (string? scope) (string/split scope #",") scope)))
                    :settings (if (map? settings) settings {})
                    :order order}))
               rules))

(defn- scope-settings
  "The foreground and font style the rules give `scope`, each from the
  most specific rule setting it, the later on a tie, or nil without a
  foreground."
  [entries scope]
  (let [matching (sort-by (juxt :depth :order)
                          (for [{:keys [selectors settings order]} entries
                                {selector :scope :keys [excluded]} selectors
                                :let [depth (prefix-depth selector scope)]
                                :when (and depth (not-any? #(prefix-depth % scope) excluded))]
                            {:depth depth :order order :settings settings}))
        merged (reduce (fn [found {:keys [settings]}]
                         (merge found (select-keys settings ["foreground" "fontStyle"])))
                       {}
                       matching)]
    (when (get merged "foreground") merged)))

(defn- font-flags
  "{:italic? :bold?} of a rule's fontStyle, a list of words."
  [font-style]
  (let [words (set (re-seq #"[a-z]+" (if (string? font-style) font-style "")))]
    {:italic? (contains? words "italic") :bold? (contains? words "bold")}))

(defn- regex-quoted [text]
  (string/replace text #"[\\^$.|?*+()\[\]{}]" #(str "\\" %)))

(defn- names-theme?
  "True when a customization's [Theme] block key names `theme-id`, alone
  or among several, where * stands for any text."
  [block-key theme-id]
  (boolean
   (and (string? theme-id)
        (some (fn [[_ pattern]]
                (re-matches (re-pattern (string/join ".*" (map regex-quoted (string/split pattern #"\*" -1))))
                            theme-id))
              (re-seq #"\[([^\]]*)\]" block-key)))))

(defn- for-theme
  "The parts of a customization for theme `theme-id`: its settings outside
  [Theme] blocks, then each block naming the theme."
  [customization theme-id]
  (let [block-key? #(string/starts-with? % "[")
        settings (if (map? customization) customization {})]
    (cons (into {} (remove (comp block-key? key)) settings)
          (keep (fn [[block-key block]]
                  (when (and (block-key? block-key) (map? block) (names-theme? block-key theme-id))
                    block))
                settings))))

(defn- customization-rules
  "TextMate rules from editor.tokenColorCustomizations for theme
  `theme-id`, each part giving its shorthands and then its textMateRules."
  [customization theme-id]
  (mapcat (fn [part]
            (concat (keep (fn [[shorthand scopes]]
                            (let [value (get part shorthand)]
                              (cond
                                (string? value) {"scope" scopes "settings" {"foreground" value}}
                                (map? value) {"scope" scopes "settings" value})))
                          shorthand-scopes)
                    (filter map? (get part "textMateRules"))))
          (for-theme customization theme-id)))

;; Fitting colors to the skies

(def ^:private glass-references
  "Per sky, the lightness of plain gray page glass, about which theme
  colors turn, and the page glass of least contrast with code: the
  darkest by day and the lightest by night, cycles and parse errors
  included."
  (into {}
        (map (fn [sky]
               (let [;; Glass is darkest and lightest at the ends of a blend's
                     ;; lightness with full chroma, sampled every 30 degrees of hue.
                     extreme-blends (for [hue (range 0 360 30)
                                          lightness [0.0 1.0]]
                                      {:blend {sky [lightness 0.2 hue]}})
                     glasses (concat [(palette/color-of sky :page-cycle) (palette/color-of sky :page-parse-error)]
                                     (map #(:panel (palette/page-oklch sky %)) extreme-blends))
                     luminance #(color/relative-luminance (color/oklch->rgb %))
                     nearest (apply (if (= :day sky) min-key max-key) luminance glasses)]
                 [sky {:gray-lightness (first (:panel (palette/page-oklch sky {})))
                       :nearest-rgb (color/oklch->rgb nearest)}])))
        [:day :night]))

(defn- least-contrast
  "The WCAG contrast a kind keeps against any page's glass: 4.5 for plain
  text, as for body text on the web, less for line numbers, brackets, and
  discards, and 3 for the rest."
  [token-kind]
  (get {:text 4.5 :gutter 2.2 :bracket 2.2 :discard 1.8} token-kind 3.0))

(defn- readable-lightness
  "The Oklch lightness nearest `lightness` that reaches `least` contrast
  with the sky's closest glass, darker by day and lighter by night."
  [[lightness chroma hue] sky least]
  (let [{:keys [gray-lightness nearest-rgb]} (get glass-references sky)
        day? (= :day sky)
        contrast-at #(color/contrast-ratio (color/oklch->rgb [% chroma hue]) nearest-rgb)
        start (if day? (min lightness gray-lightness) (max lightness gray-lightness))
        search-steps 24]
    (if (>= (contrast-at start) least)
      start
      (loop [near start
             far (if day? 0.0 1.0)
             remaining search-steps]
        (let [middle (/ (+ near far) 2)]
          (cond
            (zero? remaining) far
            (>= (contrast-at middle) least) (recur near middle (dec remaining))
            :else (recur middle far (dec remaining))))))))

(defn- fitted-hex
  "Hex of theme color `rgb` on `sky`: kept when the theme is as dark as
  the sky, else mirrored about the sky's glass at its distance from the
  theme's background, then moved until it reaches `least` contrast."
  [rgb {:keys [dark? background-lightness]} sky least]
  (let [[lightness chroma hue] (color/rgb->oklch rgb)
        placed (if (= dark? (= :night sky))
                 lightness
                 (max 0.0 (min 1.0 (- (+ (get-in glass-references [sky :gray-lightness]) background-lightness)
                                      lightness))))
        readable (readable-lightness [placed chroma hue] sky least)]
    (if (== readable lightness)
      (color/rgb->hex rgb)
      (color/oklch->hex [readable chroma hue]))))

(defn- kind-style
  "{:rgb :italic? :bold?} the rules give the first of `scopes` they color,
  or the theme's foreground."
  [rules scopes background foreground]
  (let [settings (some #(scope-settings rules %) scopes)]
    (assoc (font-flags (get settings "fontStyle"))
           :rgb (or (theme-rgb (get settings "foreground") background) foreground))))

(defn- kind-styles
  "{:rgb :italic? :bold?} of every token kind, line numbers, and the caret
  in a theme, with :languages and, when `bracket-pairs?`, :bracket-levels
  as RGB."
  [{:keys [dark? colors token-colors token-customizations color-customizations theme-id bracket-pairs?]}]
  (let [workbench-colors (apply merge (or colors {}) (for-theme color-customizations theme-id))
        rules (rule-entries (concat token-colors (customization-rules token-customizations theme-id)))
        global-settings (apply merge {} (keep (fn [rule]
                                                (when (and (map? rule) (nil? (get rule "scope")) (map? (get rule "settings")))
                                                  (get rule "settings")))
                                              token-colors))
        defaults (get editor-defaults (if dark? :dark :light))
        background (or (theme-rgb (get workbench-colors "editor.background") nil)
                       (theme-rgb (get global-settings "background") nil)
                       (color/hex->rgb (:background defaults)))
        workbench-rgb #(theme-rgb (get workbench-colors %) background)
        foreground (or (workbench-rgb "editor.foreground")
                       (theme-rgb (get global-settings "foreground") background)
                       (color/hex->rgb (:foreground defaults)))
        half-way 0.5
        bracket-pair-count 6]
    (into {:background background
           :languages (update-vals language-kind-scopes
                                   (fn [scopes-by-kind]
                                     (update-vals scopes-by-kind #(kind-style rules % background foreground))))
           :text {:rgb foreground}
           :gutter {:rgb (or (workbench-rgb "editorLineNumber.foreground")
                             (palette/mix-rgb background foreground half-way))}
           :caret {:rgb (or (workbench-rgb "editorCursor.foreground") foreground)}
           :bracket-levels (when bracket-pairs?
                             (vec (keep-indexed (fn [index fallback]
                                                  (let [setting (str "editorBracketHighlight.foreground" (inc index))]
                                                    (if (contains? workbench-colors setting)
                                                      (workbench-rgb setting)
                                                      (some-> fallback color/hex->rgb))))
                                                (take bracket-pair-count (concat (:brackets defaults) (repeat nil))))))}
          (map (fn [[token-kind scopes]] [token-kind (kind-style rules scopes background foreground)]))
          kind-scopes)))

(defn theme-colors
  "Code colors of an editor theme on both skies, from its workbench
  `colors` and `token-colors` rules with the editor's customizations over
  them. `theme-id` is the name [Theme] blocks of customizations use."
  [{:keys [dark?] :as theme}]
  (let [{:keys [background bracket-levels languages] :as styles} (kind-styles theme)
        placement {:dark? (boolean dark?) :background-lightness (first (color/rgb->oklch background))}]
    (into {}
          (map (fn [sky]
                 (let [fitted-styles (fn [styles-by-kind]
                                       (into {}
                                             (map (fn [[token-kind {:keys [rgb italic? bold?]}]]
                                                    [token-kind (cond-> {:color (fitted-hex rgb placement sky (least-contrast token-kind))}
                                                                  italic? (assoc :italic? true)
                                                                  bold? (assoc :bold? true))]))
                                             styles-by-kind))]
                   [sky (assoc (fitted-styles (dissoc styles :background :bracket-levels :languages))
                               :languages (update-vals languages fitted-styles)
                               :bracket-levels (mapv #(fitted-hex % placement sky (least-contrast :bracket-levels))
                                                     bracket-levels))])))
          [:day :night])))

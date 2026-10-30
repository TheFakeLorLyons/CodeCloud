(ns codecloud.presentation.palette
  "Every color of the day and night skies, in Oklch. Pages take glass and
  edge from their blend, gray without one. Bright red marks parse errors,
  a deeper red cycles, amber import loops, and black pages in languages
  CodeCloud reads only as text."
  (:require [codecloud.presentation.color :as color]))

(def themes
  "Oklch color of each role on each sky. Dusk holds only the sky and
  cloud colors that day and night pass through."
  {:day {:importance-low [0.80 0.03 150]
         :importance-high [0.60 0.165 148]
         :page-cycle [0.97 0.02 28]
         :page-parse-error [0.91 0.065 25]
         :cycle [0.52 0.17 27]
         :cycle-warning [0.70 0.16 70]
         :parse-error [0.58 0.235 28]
         :cloud [0.99 0.004 240]
         :cloud-shade [0.86 0.022 245]
         :ink [0.27 0.03 255]
         :ink-soft [0.46 0.025 255]
         :edge [0.42 0.05 255]
         :edge-strong [0.30 0.08 255]
         :panel [0.985 0.005 250]
         :panel-edge [0.86 0.02 250]
         :complection-low [0.80 0.02 80]
         :complection-high [0.66 0.15 72]
         :length-low [0.80 0.03 290]
         :length-high [0.52 0.17 290]
         :accent [0.50 0.15 268]
         :selection [0.84 0.08 250]
         :sky-zenith [0.63 0.12 245]
         :sky-horizon [0.93 0.035 215]
         :sun [0.97 0.06 90]}
   :night {:importance-low [0.46 0.025 150]
           :importance-high [0.82 0.16 150]
           :page-cycle [0.27 0.05 28]
           :page-parse-error [0.32 0.12 27]
           :cycle [0.76 0.13 28]
           :cycle-warning [0.82 0.15 75]
           :parse-error [0.65 0.24 28]
           :cloud [0.52 0.03 262]
           :cloud-shade [0.30 0.04 268]
           :ink [0.96 0.008 255]
           :ink-soft [0.80 0.02 255]
           :edge [0.84 0.04 280]
           :edge-strong [0.94 0.05 280]
           :panel [0.22 0.02 265]
           :panel-edge [0.36 0.03 265]
           :complection-low [0.46 0.02 80]
           :complection-high [0.84 0.14 78]
           :length-low [0.46 0.03 290]
           :length-high [0.78 0.14 290]
           :accent [0.80 0.12 268]
           :selection [0.48 0.1 265]
           :sky-zenith [0.16 0.05 272]
           :sky-horizon [0.30 0.065 262]
           :moon [0.95 0.02 85]
           :aurora [0.76 0.13 168]}
   :dusk {:sky-zenith [0.42 0.09 290]
          :sky-horizon [0.77 0.12 52]
          :cloud [0.88 0.06 45]
          :cloud-shade [0.6 0.07 335]}})

(def language-accents
  "Each analyzed language family's Language layer color per sky.
  Alternating lightness keeps every pair at least 16 apart in Oklab (Delta
  E times 100), and at least 6 under color blindness, where language tags
  tell the closest apart. Reds stay free for cycles and parse errors."
  {:day {:clojure [0.50 0.15 145] :python [0.50 0.16 258] :javascript [0.80 0.15 98] :css [0.65 0.15 305]
         :html [0.68 0.16 55] :shell [0.72 0.12 190] :c [0.42 0.20 330]}
   :night {:clojure [0.60 0.15 150] :python [0.66 0.15 258] :javascript [0.92 0.14 100] :css [0.82 0.13 305]
           :html [0.80 0.14 52] :shell [0.84 0.12 190] :c [0.58 0.20 330]}})

(def unclassified
  "{:day :night} of pages in a language CodeCloud reads only as text:
  black by day, and a dark gray by night, where black would vanish."
  {:day [0.25 0.01 260] :night [0.40 0.01 260]})

(def chooser-colors
  "Colors the color chooser offers: a deep and a light tone of twelve hues,
  at the lightness folder colors take on each sky, and all clear of the
  reds that mark cycles and parse errors."
  (let [hues [55 80 105 130 155 180 205 230 255 280 305 330]
        tones [{:day [0.54 0.15] :night [0.72 0.14]}
               {:day [0.70 0.13] :night [0.86 0.105]}]]
    (vec (for [{[day-lightness day-chroma] :day [night-lightness night-chroma] :night} tones
               hue hues]
           {:day [day-lightness day-chroma hue] :night [night-lightness night-chroma hue]}))))

(def ^:private other-language-accent
  "The warm gray of an analyzed language without a hue of its own."
  {:day [0.55 0.02 80] :night [0.72 0.02 80]})

(defn language-accent
  "Oklch color of a Language layer entry on one theme: a language family,
  or :unclassified."
  [theme-name family]
  (if (= :unclassified family)
    (get unclassified theme-name)
    (get-in language-accents [theme-name family] (get other-language-accent theme-name))))

(def syntax-colors
  "CodeCloud's own token colors for code, chosen to read on each theme's
  pages."
  {:day {:text [0.30 0.03 255]
         :comment [0.58 0.02 255]
         :string [0.50 0.12 150]
         :keyword [0.50 0.15 305]
         :number [0.55 0.14 45]
         :constant [0.55 0.14 45]
         :special [0.48 0.14 258]
         :definition [0.45 0.10 215]
         :reader [0.56 0.12 80]
         :bracket [0.62 0.02 255]
         :discard [0.70 0.01 255]
         :gutter [0.70 0.015 255]}
   :night {:text [0.90 0.015 255]
           :comment [0.64 0.02 255]
           :string [0.84 0.12 150]
           :keyword [0.80 0.12 305]
           :number [0.84 0.10 55]
           :constant [0.84 0.10 55]
           :special [0.80 0.12 258]
           :definition [0.86 0.09 215]
           :reader [0.86 0.10 85]
           :bracket [0.66 0.02 255]
           :discard [0.50 0.01 255]
           :gutter [0.52 0.02 255]}})

(defn theme-key
  "The theme drawn for a night amount between 0 (day) and 1 (night).
  Text is redrawn when the sky passes halfway."
  [night-amount]
  (if (< night-amount 0.5) :day :night))

(defn color-of
  "Oklch color `color-key` of theme `theme-name`."
  [theme-name color-key]
  (get-in themes [theme-name color-key]))

(defn- hex-table
  "The colors of each theme of `colors-by-theme` as hex, worked out once."
  [colors-by-theme]
  (into {} (map (fn [[theme-name colors]] [theme-name (update-vals colors color/oklch->hex)])) colors-by-theme))

(def ^:private theme-hexes (hex-table themes))

(defn hex-of [theme-name color-key]
  (get-in theme-hexes [theme-name color-key]))

(def ^:private blend-tones
  "How a blend becomes edge and glass per sky: the edge keeps its hue and
  chroma at a readable lightness, and the glass takes the hue at low
  chroma. Gray is the unblended look."
  {:day {:edge-lightness [0.42 0.80]
         :glass-lightness 0.955
         :glass-lightness-swing 0.08
         :glass-chroma-share 0.42
         :most-glass-chroma 0.072
         :gray-edge [0.62 0.012 250]
         :gray-glass [0.968 0.004 250]}
   :night {:edge-lightness [0.58 0.92]
           :glass-lightness 0.27
           :glass-lightness-swing 0.1
           :glass-chroma-share 0.5
           :most-glass-chroma 0.08
           :gray-edge [0.70 0.012 260]
           :gray-glass [0.245 0.008 262]}})

(defn- blended-oklch
  "{:panel :accent} on one theme for a page whose layers blend to
  `blended`, an Oklch color, or nil for a page none of them colors."
  [theme-name blended]
  (let [{[least-edge most-edge] :edge-lightness
         :keys [glass-lightness glass-lightness-swing glass-chroma-share most-glass-chroma gray-edge gray-glass]}
        (get blend-tones theme-name)
        middle-edge (/ (+ least-edge most-edge) 2)]
    (if-let [[lightness chroma hue] blended]
      (let [edge-lightness (max least-edge (min most-edge lightness))]
        {:accent [edge-lightness chroma hue]
         :panel [(+ glass-lightness (* glass-lightness-swing (- edge-lightness middle-edge)))
                 (min most-glass-chroma (* glass-chroma-share chroma))
                 hue]})
      {:accent gray-edge :panel gray-glass})))

(defn page-oklch
  "{:panel :accent} of a page on one theme from its spec's :blend, {:day
  :night} or nil, overridden by :parse-error?, :cyclic?, or
  :cycle-warning?."
  [theme-name {:keys [blend cyclic? cycle-warning? parse-error?]}]
  (cond
    parse-error? {:panel (color-of theme-name :page-parse-error) :accent (color-of theme-name :parse-error)}
    cyclic? {:panel (color-of theme-name :page-cycle) :accent (color-of theme-name :cycle)}
    :else (cond-> (blended-oklch theme-name (get blend theme-name))
            cycle-warning? (assoc :accent (color-of theme-name :cycle-warning)))))

(defn page-rgb
  "Page colors as gamma-encoded RGB for both themes, {:day {:panel
  :accent} :night {:panel :accent}}, blended while the sky turns."
  [color-spec]
  (into {}
        (map (fn [theme-name]
               [theme-name (update-vals (page-oklch theme-name color-spec) color/oklch->rgb)]))
        [:day :night]))

(defn- pulled-hue
  "Hue of `oklch` once a chroma of `pull` toward `hue` adds to it in Oklab."
  [oklch hue pull]
  (let [[_ green-red blue-yellow] (color/oklch->oklab oklch)
        [_ pull-green-red pull-blue-yellow] (color/oklch->oklab [0 pull hue])]
    (nth (color/oklab->oklch [0 (+ green-red pull-green-red) (+ blue-yellow pull-blue-yellow)]) 2)))

(def ^:private cloud-tints
  "Lightness and chroma of a tinted cloud's lit and shaded tones per sky
  and tint. By day a cloud takes the folder's hue outright. By night it
  keeps its moonlit blue, with the hue pulled toward the folder's by :lean
  of the blue's chroma, since turning it further passes through pink near
  the cycle reds, and graying it toward a warm hue looks khaki beside the
  sky."
  {:day {:slight {:light [0.978 0.014] :shade [0.86 0.03]}
         :strong {:light [0.968 0.03] :shade [0.835 0.06]}}
   :night {:slight {:light [0.536 0.03] :shade [0.316 0.038] :lean 0.3}
           :strong {:light [0.54 0.036] :shade [0.32 0.046] :lean 0.5}}})

(defn folder-cloud-oklch
  "{:light :shade} of a folder cloud on one theme, tinted toward `hue` by
  `tint`, :slight or :strong, or plain without a hue or a tint."
  [theme-name hue tint]
  (let [plain {:light (color-of theme-name :cloud)
               :shade (color-of theme-name :cloud-shade)}]
    (if-let [{:keys [lean] :as tones} (when hue (get-in cloud-tints [theme-name tint]))]
      (into {}
            (map (fn [tone]
                   (let [[lightness chroma] (get tones tone)
                         [_ plain-chroma :as plain-color] (get plain tone)]
                     [tone [lightness chroma (if lean
                                               (pulled-hue plain-color hue (* lean plain-chroma))
                                               hue)]])))
            [:light :shade])
      plain)))

(defn folder-cloud-rgb
  "Folder cloud colors for `hue` and `tint` as gamma-encoded RGB for both
  themes: {:day {:light :shade} :night {:light :shade}}."
  [hue tint]
  (into {}
        (map (fn [theme-name]
               [theme-name (update-vals (folder-cloud-oklch theme-name hue tint) color/oklch->rgb)]))
        [:day :night]))

(defn blended-rgb
  "A theme color between day and night."
  [color-key night-amount]
  (color/oklch->rgb (color/mix-oklch (color-of :day color-key) (color-of :night color-key) night-amount)))

(defn mix-rgb
  "Linear blend of two RGB colors."
  [[from-red from-green from-blue] [to-red to-green to-blue] share]
  [(+ from-red (* share (- to-red from-red)))
   (+ from-green (* share (- to-green from-green)))
   (+ from-blue (* share (- to-blue from-blue)))])

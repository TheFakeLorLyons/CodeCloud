(ns codecloud.presentation.environments
  "The spaces the cloud floats in. An environment names the backdrop drawn
  behind the cloud, the light deciding whether pages take their day or
  night look, how folders gather, and how many frames a second the view
  draws. The built-in environments come first, then the spaces a person
  describes in the codecloud.spaces setting, each a room or an endless
  maze of rooms with walls, floor, and ceiling of its own."
  (:require [clojure.string :as string]
            [codecloud.presentation.color :as color]))

(defn hex-rgb
  "Gamma-encoded sRGB components from 0 to 1 of a color written as #rgb or
  #rrggbb, or nil for anything else."
  [text]
  (when (string? text)
    (let [digits (string/replace (string/trim text) #"^#" "")
          expanded (cond
                     (re-matches #"[0-9a-fA-F]{6}" digits) digits
                     (re-matches #"[0-9a-fA-F]{3}" digits) (apply str (mapcat #(repeat 2 %) digits))
                     :else nil)
          channel (fn [start]
                    (let [pair (subs expanded start (+ start 2))]
                      (/ #?(:clj (Integer/parseInt pair 16) :cljs (js/parseInt pair 16)) 255.0)))]
      (when expanded
        (mapv channel [0 2 4])))))

(def patterns
  "The patterns a surface of a space can wear, in the order the room
  shader numbers them."
  [:plain :stripes :grid :tiles :checks :dots :carpet :wallpaper])

(def ^:private backrooms-space
  "Mono-yellow wallpaper, damp carpet, and ceiling tiles holding
  fluorescent panels, repeating in every direction."
  {:layout :maze
   :walls {:color (hex-rgb "#c9b56a") :accent (hex-rgb "#ad9a54") :pattern :wallpaper :scale 1.0}
   :floor {:color (hex-rgb "#94834f") :accent (hex-rgb "#77683c") :pattern :carpet :scale 1.0}
   :ceiling {:color (hex-rgb "#dcd4ad") :accent (hex-rgb "#b9b089") :pattern :tiles :scale 1.0}
   :lamps (hex-rgb "#fff6d6")
   :haze (hex-rgb "#5f5532")})

(def ^:private night-library-space
  "Dark wood paneling, red carpet, and warm lamps, aisle after aisle after
  closing time."
  {:layout :maze
   :walls {:color (hex-rgb "#3a2a22") :accent (hex-rgb "#2e221a") :pattern :stripes :scale 1.0}
   :floor {:color (hex-rgb "#5a1f24") :accent (hex-rgb "#47181c") :pattern :carpet :scale 1.0}
   :ceiling {:color (hex-rgb "#2b2420") :accent (hex-rgb "#221c19") :pattern :tiles :scale 1.0}
   :lamps (hex-rgb "#ffd9a0")
   :haze (hex-rgb "#140f0c")})

(def ^:private plain-space
  "The room a described space starts from, light walls on a tiled floor
  under a lit ceiling, each part replaced by what the description gives."
  {:layout :room
   :walls {:color (hex-rgb "#e6e1d6") :accent (hex-rgb "#cfc8b8") :pattern :plain :scale 1.0}
   :floor {:color (hex-rgb "#8d8478") :accent (hex-rgb "#77705f") :pattern :tiles :scale 1.0}
   :ceiling {:color (hex-rgb "#f2efe8") :accent (hex-rgb "#d9d4c8") :pattern :grid :scale 1.0}
   :lamps (hex-rgb "#fff8e8")
   :haze (hex-rgb "#d7d2c6")})

(def built-in
  "The environments CodeCloud offers, in the order the sky button cycles
  through them."
  [{:id :simple :name "Simple" :backdrop :plain :light :theme :folders :discs :frames-per-second 30
    :description "A plain backdrop, folders as flat discs, and fewer frames, easy on large codebases and modest computers"}
   {:id :auto :name "Sky with the editor theme" :backdrop :sky :light :theme :folders :clouds
    :description "A day sky with light themes and a night sky with dark ones"}
   {:id :day :name "Day" :backdrop :sky :light :day :folders :clouds
    :description "Always a day sky"}
   {:id :twilight :name "Twilight" :backdrop :sky :light :twilight :folders :clouds
    :description "Always a sunset sky, with night pages"}
   {:id :night :name "Night" :backdrop :sky :light :night :folders :clouds
    :description "Always a night sky"}
   {:id :clock :name "Time of day" :backdrop :sky :light :clock :folders :clouds
    :description "The sky of the local time, with dusk and dawn between"}
   {:id :backrooms :name "Backrooms" :backdrop :room :light :day :folders :clouds :space backrooms-space
    :description "Yellow wallpaper, damp carpet, and humming lights, room after room"}
   {:id :night-library :name "Night Library" :backdrop :room :light :night :folders :clouds :space night-library-space
    :description "Dark wood, red carpet, and warm lamps, aisle after aisle"}])

(def default-id
  "The environment a first look at the cloud gets, the lightest to draw."
  :simple)

(defn- choice [text allowed fallback]
  (let [chosen (when (string? text) (keyword (string/lower-case (string/trim text))))]
    (if (some #{chosen} allowed) chosen fallback)))

(defn- face-from-setting
  "One surface of a described space, its color, patternColor, pattern, and
  patternScale taken from `setting` where valid, else from `face`."
  [setting face]
  (let [setting (if (map? setting) setting {})
        scale (get setting "patternScale")
        smallest-scale 0.25
        largest-scale 8]
    {:color (or (hex-rgb (get setting "color")) (:color face))
     :accent (or (hex-rgb (get setting "patternColor")) (:accent face))
     :pattern (choice (get setting "pattern") patterns (:pattern face))
     :scale (if (and (number? scale) (pos? scale))
              (double (min largest-scale (max smallest-scale scale)))
              (:scale face))}))

(defn- lamps-from-setting
  "The lamps' color from the setting's lamps, none when it says false,
  none, or off, and the plain room's lamps when it says nothing usable."
  [setting]
  (let [lamps (get setting "lamps")]
    (cond
      (or (false? lamps) (contains? #{"none" "off"} (some-> lamps str string/lower-case))) nil
      :else (or (hex-rgb lamps) (:lamps plain-space)))))

(defn space-from-setting
  "The environment one entry of the codecloud.spaces setting describes,
  given as a map with string keys, or nil without a name."
  [setting]
  (when (map? setting)
    (let [space-name (some-> (get setting "name") str string/trim)
          layout (choice (get setting "layout") [:room :maze] (:layout plain-space))]
      (when (seq space-name)
        {:id space-name
         :name space-name
         :backdrop :room
         :light (choice (get setting "light") [:day :twilight :night :clock :theme] :day)
         :folders (choice (get setting "folders") [:clouds :discs :none] :clouds)
         :description (if (= :maze layout)
                        "Rooms in every direction, from the codecloud.spaces setting"
                        "One room, from the codecloud.spaces setting")
         :space {:layout layout
                 :walls (face-from-setting (get setting "walls") (:walls plain-space))
                 :floor (face-from-setting (get setting "floor") (:floor plain-space))
                 :ceiling (face-from-setting (get setting "ceiling") (:ceiling plain-space))
                 :lamps (lamps-from-setting setting)
                 :haze (or (hex-rgb (get setting "haze")) (:haze plain-space))}}))))

(defn spaces-from-setting
  "The spaces the codecloud.spaces setting describes, leaving out entries
  without a name, a name a built-in environment goes by, and a name used
  before."
  [setting]
  (let [taken (into #{} (map (comp name :id)) built-in)]
    (first (reduce (fn [[kept used] entry]
                     (let [space (space-from-setting entry)
                           folded (some-> space :id string/lower-case)]
                       (if (and space (not (contains? used folded)))
                         [(conj kept space) (conj used folded)]
                         [kept used])))
                   [[] taken]
                   (if (sequential? setting) setting [])))))

(defn all
  "Every environment to choose from, the built-in ones first."
  [spaces]
  (concat built-in spaces))

(defn chosen-color
  "The color `text` names as #rgb or #rrggbb, written as #rrggbb, or nil."
  [text]
  (some-> (hex-rgb text) color/rgb->hex))

(def ^:private simple-look
  "The gradient and light of Simple in the color `color-hex` names, the
  color overhead brightening toward the horizon, with pages in their day
  look on a light color and their night look on a dark one, or nil when
  it names none. Kept per color, since every frame asks."
  (memoize
   (fn [color-hex]
     (when-let [rgb (hex-rgb color-hex)]
       (let [[lightness chroma hue] (color/rgb->oklch rgb)
             horizon-rise 0.13
             brightest 0.98
             ;; Black text and white text read equally well at this luminance.
             even-contrast 0.179]
         {:gradient {:zenith rgb
                     :horizon (color/oklch->rgb [(min brightest (+ lightness horizon-rise)) chroma hue])}
          :light (if (> (color/relative-luminance rgb) even-contrast) :day :night)})))))

(defn with-simple-color
  "Simple in the color `color-hex` names. Any other environment, or Simple
  without a color, stays as it is."
  [environment color-hex]
  (if-let [look (when (= :simple (:id environment)) (simple-look color-hex))]
    (merge environment look)
    environment))

(defn environment-of
  "The environment `id` names, a built-in's keyword or a space's name, or
  the default one when no environment goes by it."
  [id spaces]
  (or (some #(when (= id (:id %)) %) (all spaces))
      (some #(when (= default-id (:id %)) %) built-in)))

(defn chosen-id
  "The environment id a setting's `text` names, the keyword of a built-in
  environment or the name of a space, or the default id when it names
  neither."
  [text spaces]
  (let [text (str text)
        built-in-id (keyword text)]
    (cond
      (some #(= built-in-id (:id %)) built-in) built-in-id
      (some #(= text (:id %)) spaces) text
      :else default-id)))

(defn next-id
  "The id after `id` in the order the sky button cycles through."
  [id spaces]
  (let [ids (mapv :id (all spaces))
        position (or (first (keep-indexed (fn [index each] (when (= each id) index)) ids)) -1)]
    (nth ids (mod (inc position) (count ids)))))

(ns codecloud.presentation.color
  "Color math in Oklab and Oklch, where equal steps look equal. Colors are
  [red green blue], gamma-encoded from 0 to 1, or [lightness chroma hue],
  hue in degrees.")

(defn- linear-from-encoded [component]
  (if (<= component 0.04045)
    (/ component 12.92)
    (Math/pow (/ (+ component 0.055) 1.055) 2.4)))

(defn- encoded-from-linear [component]
  (if (<= component 0.0031308)
    (* 12.92 component)
    (- (* 1.055 (Math/pow component (/ 1 2.4))) 0.055)))

(defn rgb->oklab
  "Björn Ottosson's Oklab from gamma-encoded sRGB."
  [[red green blue]]
  (let [linear-red (linear-from-encoded red)
        linear-green (linear-from-encoded green)
        linear-blue (linear-from-encoded blue)
        ;; Each row of Ottosson's matrix from linear sRGB to cone responses
        ;; sums to one, so white stirs every cone alike, and a row's weight
        ;; for red follows from its other two.
        cone-response (fn [green-weight blue-weight]
                        (Math/cbrt (+ (* (- 1 green-weight blue-weight) linear-red)
                                      (* green-weight linear-green)
                                      (* blue-weight linear-blue))))
        long-cone (cone-response 0.5363325363 0.0514459929)
        medium-cone (cone-response 0.6806995451 0.1073969566)
        short-cone (cone-response 0.2817188376 0.6299787005)]
    [(+ (* 0.2104542553 long-cone) (* 0.7936177850 medium-cone) (* -0.0040720468 short-cone))
     (+ (* 1.9779984951 long-cone) (* -2.4285922050 medium-cone) (* 0.4505937099 short-cone))
     (+ (* 0.0259040371 long-cone) (* 0.7827717662 medium-cone) (* -0.8086757660 short-cone))]))

(defn- oklab->linear-rgb [[lightness green-red blue-yellow]]
  (let [long-cone (Math/pow (+ lightness (* 0.3963377774 green-red) (* 0.2158037573 blue-yellow)) 3)
        medium-cone (Math/pow (+ lightness (* -0.1055613458 green-red) (* -0.0638541728 blue-yellow)) 3)
        short-cone (Math/pow (+ lightness (* -0.0894841775 green-red) (* -1.2914855480 blue-yellow)) 3)
        ;; Each row of the matrix back to linear sRGB sums to one, so cones
        ;; stirred alike give white, and a row's weight for the medium cone
        ;; follows from its other two.
        channel (fn [long-weight short-weight]
                  (+ (* long-weight long-cone)
                     (* (- 1 long-weight short-weight) medium-cone)
                     (* short-weight short-cone)))]
    [(channel 4.0767416621 0.2309699292)
     (channel -1.2684380046 -0.3413193965)
     (channel -0.0041960863 1.7076147010)]))

(defn- degrees->radians [degrees] (* degrees (/ Math/PI 180)))
(defn- radians->degrees [radians] (* radians (/ 180 Math/PI)))

(defn oklch->oklab [[lightness chroma hue]]
  [lightness
   (* chroma (Math/cos (degrees->radians hue)))
   (* chroma (Math/sin (degrees->radians hue)))])

(defn oklab->oklch [[lightness green-red blue-yellow]]
  (let [hue (radians->degrees (Math/atan2 blue-yellow green-red))]
    [lightness
     (Math/sqrt (+ (* green-red green-red) (* blue-yellow blue-yellow)))
     (if (neg? hue) (+ hue 360) hue)]))

(defn- in-gamut? [linear-components]
  (let [tolerance 0.000001]
    (every? #(<= (- tolerance) % (+ 1 tolerance)) linear-components)))

(defn oklch->rgb
  "Gamma-encoded sRGB for an Oklch color. Colors outside sRGB lose chroma
  and keep their lightness and hue."
  [[lightness chroma hue]]
  (let [search-steps 24
        fitted-chroma (if (in-gamut? (oklab->linear-rgb (oklch->oklab [lightness chroma hue])))
                        chroma
                        (loop [low 0.0
                               high chroma
                               remaining search-steps]
                          (if (zero? remaining)
                            low
                            (let [middle (/ (+ low high) 2)]
                              (if (in-gamut? (oklab->linear-rgb (oklch->oklab [lightness middle hue])))
                                (recur middle high (dec remaining))
                                (recur low middle (dec remaining)))))))]
    (mapv #(encoded-from-linear (max 0 (min 1 %)))
          (oklab->linear-rgb (oklch->oklab [lightness fitted-chroma hue])))))

(defn rgb->oklch [rgb]
  (oklab->oklch (rgb->oklab rgb)))

(def ^:private hex-digits "0123456789abcdef")

(defn- byte->hex [byte-value]
  (let [clamped (max 0 (min 255 (Math/round (double byte-value))))]
    (str (subs hex-digits (quot clamped 16) (inc (quot clamped 16)))
         (subs hex-digits (mod clamped 16) (inc (mod clamped 16))))))

(defn rgb->hex [[red green blue]]
  (str "#" (byte->hex (* 255 red)) (byte->hex (* 255 green)) (byte->hex (* 255 blue))))

(defn hex->rgb
  "[red green blue] from \"#rrggbb\"."
  [hex]
  (let [digits (if (= \# (first hex)) (subs hex 1) hex)
        channel (fn [start]
                  (/ #?(:clj (Integer/parseInt (subs digits start (+ start 2)) 16)
                        :cljs (js/parseInt (subs digits start (+ start 2)) 16))
                     255))]
    [(channel 0) (channel 2) (channel 4)]))

(defn oklch->hex [oklch]
  (rgb->hex (oklch->rgb oklch)))

(defn- interpolate [from to fraction]
  (+ from (* (- to from) fraction)))

(defn mix-oklch
  "Color `fraction` of the way from `from` to `to`, turning the hue along
  the shorter arc."
  [[from-lightness from-chroma from-hue] [to-lightness to-chroma to-hue] fraction]
  (let [hue-difference (let [raw (- to-hue from-hue)]
                         (cond (> raw 180) (- raw 360)
                               (< raw -180) (+ raw 360)
                               :else raw))
        hue (+ from-hue (* hue-difference fraction))]
    [(interpolate from-lightness to-lightness fraction)
     (interpolate from-chroma to-chroma fraction)
     (cond (neg? hue) (+ hue 360) (>= hue 360) (- hue 360) :else hue)]))
(defn relative-luminance
  "WCAG relative luminance of gamma-encoded sRGB."
  [rgb]
  (let [[red green blue] (map linear-from-encoded rgb)]
    (+ (* 0.2126 red) (* 0.7152 green) (* 0.0722 blue))))

(defn contrast-ratio
  "WCAG contrast ratio between two gamma-encoded sRGB colors."
  [first-rgb second-rgb]
  (let [[lighter darker] (sort > [(relative-luminance first-rgb) (relative-luminance second-rgb)])]
    (/ (+ lighter 0.05) (+ darker 0.05))))

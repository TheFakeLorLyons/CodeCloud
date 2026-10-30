(ns codecloud.render.minimap
  "Tables of a page's colored code runs for its minimap shader. Each panel
  line takes sixteen RGBA slots, its runs and then its code's extent, and
  each texture row holds sixteen lines."
  (:require ["three" :as three]
            [codecloud.presentation.highlight :as highlight]))

(def ^:private runs-per-line 15)

(def ^:private slots-per-line (inc runs-per-line))

(def ^:private lines-per-texture-row 16)

(defn- write-texel!
  "Writes one RGBA texel of the table, capping the first two values at a
  byte."
  [^js data index red green blue]
  (let [byte-cap 255]
    (aset data index (min byte-cap red))
    (aset data (+ index 1) (min byte-cap green))
    (aset data (+ index 2) blue)
    (aset data (+ index 3) byte-cap)))

(defn- write-line-runs!
  "Writes one panel line's colored runs, each [start length kind], and in
  the line's last slot where its code starts and ends."
  [data table-width line-index runs]
  (let [row (quot line-index lines-per-texture-row)
        slot-base (* slots-per-line (mod line-index lines-per-texture-row))
        texel-index (fn [slot] (* 4 (+ (* row table-width) slot-base slot)))]
    (doseq [[slot [start length kind-index]] (map-indexed vector runs)]
      (write-texel! data (texel-index slot) start length kind-index))
    (when (seq runs)
      (let [[first-start] (first runs)
            [last-start last-length] (peek runs)]
        (write-texel! data (texel-index runs-per-line) first-start (+ last-start last-length) 0)))))

(defn- data-texture
  "Texture of RGBA bytes that the shader reads texel by texel."
  [data width height]
  (let [^js texture (three/DataTexture. data width height three/RGBAFormat three/UnsignedByteType)]
    (set! (.-magFilter texture) three/NearestFilter)
    (set! (.-minFilter texture) three/NearestFilter)
    (set! (.-generateMipmaps texture) false)
    (set! (.-needsUpdate texture) true)
    texture))

(defn table
  "{:texture :size} for highlighted `lines`, cut at `code-characters`.
  `line-count` covers the page, whose lines past `lines` stay empty."
  [lines line-count code-characters]
  (let [line-count (max 1 line-count (count lines))
        width (* slots-per-line lines-per-texture-row)
        height (js/Math.ceil (/ line-count lines-per-texture-row))
        data (js/Uint8Array. (* 4 width height))]
    (doseq [[line-index {:keys [tokens]}] (map-indexed vector lines)]
      (write-line-runs! data width line-index (highlight/line-runs tokens code-characters runs-per-line)))
    {:texture (data-texture data width height) :size [width height]}))

(defn release!
  "Frees a table's texture."
  [{:keys [^js texture]}]
  (when texture (.dispose texture)))

(defn empty-texture
  "A one-texel texture for pages without a table yet."
  []
  (let [^js texture (three/DataTexture. (js/Uint8Array. 4) 1 1 three/RGBAFormat three/UnsignedByteType)]
    (set! (.-needsUpdate texture) true)
    texture))

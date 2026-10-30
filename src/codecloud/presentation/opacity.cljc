(ns codecloud.presentation.opacity
  "How solid each page is drawn. Measured pages carry :depth,
  :pixels-per-unit, and the screen rectangle :screen-x :screen-y
  :half-width-pixels :half-height-pixels."
  (:require [codecloud.presentation.zoom :as zoom]))

(defn- depth-fade
  "Pages fade beyond the pivot, so the near cloud reads first, and well
  before it, so they never hide what the camera zoomed toward."
  [depth focus-distance]
  (let [ratio (/ depth (max 0.001 focus-distance))
        ;; From 0.8 of the focus distance on, a page loses half its
        ;; solidity for each further focus distance, keeping at least 0.3.
        far-fade (max 0.3 (min 1 (- 1.4 (* 0.5 ratio))))
        ;; Nearer than a fifth of the focus distance a page all but
        ;; vanishes, and it returns by about half of it.
        [vanished-ratio returned-ratio] [0.22 0.55]
        least-near 0.06
        near-fade (+ least-near (* (- 1 least-near) (zoom/smoothstep vanished-ratio returned-ratio ratio)))]
    (* far-fade near-fade)))

(defn holding-pivot
  "How surely a page holds the pivot at the screen's middle and focus
  distance: 1 while the camera rests on it, falling to 0 as it leaves."
  [{:keys [depth screen-x screen-y half-width-pixels half-height-pixels on-screen?]}
   {:keys [focus-distance viewport]}]
  (if (and on-screen? (> half-width-pixels 0) (> half-height-pixels 0))
    (let [{:keys [width height]} viewport
          depth-offset (Math/abs (- (/ depth (max 0.001 focus-distance)) 1))
          reach (max (/ (Math/abs (- (* 0.5 width) screen-x)) half-width-pixels)
                     (/ (Math/abs (- (* 0.5 height) screen-y)) half-height-pixels))
          ;; Holding fades as the pivot leaves the page's plane by 4 to 12
          ;; percent of the focus distance, or passes its edge by up to 30
          ;; percent of its half size.
          [plane-near plane-far] [0.04 0.12]
          edge-reach 1.3]
      (* (- 1 (zoom/smoothstep plane-near plane-far depth-offset))
         (- 1 (zoom/smoothstep 1 edge-reach reach))))
    0))

(defn covered-share
  "Share of the smaller of two pages that their overlap on screen covers."
  [first-page second-page]
  (let [extent (fn [{:keys [screen-x screen-y half-width-pixels half-height-pixels]}]
                 [(- screen-x half-width-pixels) (+ screen-x half-width-pixels)
                  (- screen-y half-height-pixels) (+ screen-y half-height-pixels)])
        [first-left first-right first-top first-bottom] (extent first-page)
        [second-left second-right second-top second-bottom] (extent second-page)
        overlap-width (- (min first-right second-right) (max first-left second-left))
        overlap-height (- (min first-bottom second-bottom) (max first-top second-top))
        smaller-area (min (* (- first-right first-left) (- first-bottom first-top))
                          (* (- second-right second-left) (- second-bottom second-top)))]
    (if (and (> overlap-width 0) (> overlap-height 0) (> smaller-area 0))
      (/ (* overlap-width overlap-height) smaller-area)
      0)))

(defn- clearing
  "How far a page steps aside for the examined page: fully when nearer and
  covering it, not at all once the camera moves on."
  [{:keys [id depth] :as measured-page} examined]
  (let [nearer-share 0.97
        full-cover 0.2]
    (if (and examined (not= id (:id examined)) (< depth (* nearer-share (:depth examined))))
      (* (:holding examined) (zoom/smoothstep 0 full-cover (covered-share measured-page examined)))
      0)))

(defn page-opacity
  "{:opacity :see-through?} of a page this frame. Pages fade from the
  pivot, dim when unrelated to the lift, and step aside for a lifted or
  examined page behind them; past halfway aside, the pointer passes
  through."
  [{:keys [id visual appearance depth pixels-per-unit] :as measured-page}
   {:keys [focus-distance lift-depth lifted-ids examined]}]
  (let [lifted? (contains? lifted-ids id)
        ;; Lines grow readable from about half a pixel to eight pixels tall,
        ;; and a page firms from 0.62 to 0.92 solid as they do.
        reading (zoom/smoothstep 0.6 8 pixels-per-unit)
        [distant-solidity reading-solidity] [0.62 0.3]
        nearer-share 0.97
        in-front-of-lift? (and lift-depth (not lifted?) (< depth (* nearer-share lift-depth)))
        clearing-share (clearing measured-page examined)
        stepped-aside 0.15
        half-cleared 0.5]
    {:opacity (* appearance
                 (if lifted? 1 (* (:emphasis visual) (depth-fade depth focus-distance)))
                 (if in-front-of-lift? stepped-aside 1)
                 (- 1 (* (- 1 stepped-aside) clearing-share))
                 (+ distant-solidity (* reading-solidity reading)))
     :see-through? (boolean (or in-front-of-lift? (> clearing-share half-cleared)))}))

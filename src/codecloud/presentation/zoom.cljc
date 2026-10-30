(ns codecloud.presentation.zoom
  "Zoom math: apparent size on screen, when zooming in hands over to the
  editor, and how zooming out of the editor maps onto its font steps.")

(defn- half-angle-tangent [vertical-field-of-view-degrees]
  (Math/tan (/ (* vertical-field-of-view-degrees Math/PI) 360)))

(defn pixels-per-unit
  "Screen pixels covered by one world unit at `distance` from a
  perspective camera."
  [distance vertical-field-of-view-degrees viewport-height]
  (if (<= distance 0)
    ##Inf
    (/ viewport-height (* 2 distance (half-angle-tangent vertical-field-of-view-degrees)))))

(defn distance-for-scale
  "Camera distance at which one world unit covers `target-pixels` pixels."
  [target-pixels vertical-field-of-view-degrees viewport-height]
  (/ viewport-height (* 2 target-pixels (half-angle-tangent vertical-field-of-view-degrees))))

(defn smoothstep
  "0 below `edge-start`, 1 above `edge-end`, and an S-curve between."
  [edge-start edge-end value]
  (let [fraction (max 0.0 (min 1.0 (double (/ (- value edge-start) (- edge-end edge-start)))))]
    (* fraction fraction (- 3 (* 2 fraction)))))

(def font-zoom-step
  "Share of the base font size that one VS Code editor font zoom step adds
  or removes."
  0.1)

(defn editor-zoom-steps
  "Font zoom-out steps until editor text looks `threshold-percent` zoomed
  out: 150, two thirds size, takes four. Thresholds run from 100 to 200."
  [threshold-percent]
  (let [;; VS Code stops font zoom out at half size, five steps down.
        deepest-zoom-out 5
        clamped (max 100 (min 200 threshold-percent))
        shrink (- 1 (/ 100 clamped))
        tolerance 0.000001]
    (min deepest-zoom-out
         (int (Math/ceil (- (/ shrink font-zoom-step) tolerance))))))

(defn text-scale-after-steps
  "Editor text size, as a share of normal, after `steps` font zoom-outs."
  [steps]
  (- 1 (* font-zoom-step steps)))

(defn arrival-text-scale
  "Size of code, as a share of the editor's, where the cloud picks up
  after zooming out of the editor past `threshold-percent`."
  [threshold-percent]
  (/ 100 (max 100 (min 200 threshold-percent))))

(defn zoom-out-press
  "What a zoom-out press does after `steps-taken`: {:action :shrink
  :text-scale} or {:action :open-cloud :text-scale}."
  [steps-taken threshold-percent]
  (let [steps-to-cloud (max 1 (editor-zoom-steps threshold-percent))]
    (if (< (inc steps-taken) steps-to-cloud)
      {:action :shrink :text-scale (text-scale-after-steps (inc steps-taken))}
      {:action :open-cloud :text-scale (arrival-text-scale threshold-percent)})))

(defn leak-dive
  "Dive pressure after `elapsed-seconds` without pushing."
  [pressure elapsed-seconds]
  (let [leak-per-second 1.6]
    (max 0 (- pressure (* leak-per-second elapsed-seconds)))))

(defn press-dive
  "Dive pressure after `wheel-pixels` into code already at editor size.
  Pressure leaks, so only a deliberate push opens the editor."
  [pressure wheel-pixels elapsed-seconds]
  (let [pressure-per-notch 0.34
        notch-pixels 100
        largest-counted-notch 1.5
        notches (min largest-counted-notch (/ (Math/abs wheel-pixels) notch-pixels))]
    (min 1 (+ (leak-dive pressure elapsed-seconds) (* pressure-per-notch notches)))))

(defn framing-distance
  "Camera distance at which a sphere of `radius` fits the view with a
  margin, whichever of the width or height is tighter."
  [radius vertical-field-of-view-degrees aspect-ratio]
  (let [half-vertical (/ (* vertical-field-of-view-degrees Math/PI) 360)
        half-horizontal (Math/atan (* (Math/tan half-vertical) aspect-ratio))
        margin 1.1]
    (/ (* radius margin) (Math/sin (min half-vertical half-horizontal)))))

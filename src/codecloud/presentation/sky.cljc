(ns codecloud.presentation.sky
  "How dark the sky is: a night amount from 0, full day, to 1, full night,
  which the sky and clouds follow, and the ink amount pages and panels
  follow, which turns from day to night early in the dusk.")

(def mode-order
  "Order the sky button cycles through."
  [:auto :day :twilight :night :clock])

(defn next-mode [sky-mode]
  (let [position (or (first (keep-indexed (fn [index mode] (when (= mode sky-mode) index))
                                          mode-order))
                     -1)]
    (nth mode-order (mod (inc position) (count mode-order)))))

(defn clock-night-amount
  "Night amount for a local time in fractional hours: full day from 7 to
  17, full night from 20 to 5, dusk and dawn between."
  [hour]
  (let [dawn-start 5.0
        dawn-end 7.0
        dusk-start 17.0
        dusk-end 20.0
        ease (fn [fraction] (* fraction fraction (- 3 (* 2 fraction))))]
    (cond
      (< hour dawn-start) 1.0
      (< hour dawn-end) (- 1 (ease (/ (- hour dawn-start) (- dawn-end dawn-start))))
      (< hour dusk-start) 0.0
      (< hour dusk-end) (ease (/ (- hour dusk-start) (- dusk-end dusk-start)))
      :else 1.0)))

(defn night-amount
  "Night amount for a sky mode. :auto follows the editor theme, so a dark
  theme gets the night sky."
  [sky-mode theme-kind local-hour]
  (let [twilight-night 0.5]
    (case sky-mode
      :day 0.0
      ;; Halfway, where the sunset colors peak.
      :twilight twilight-night
      :night 1.0
      :clock (clock-night-amount local-hour)
      (if (= :dark theme-kind) 1.0 0.0))))

(defn ink-amount
  "How far pages, text, lines, and panels have turned from their day look
  to their night look under a night amount. They turn quickly as the dusk
  begins and never rest halfway, where neither look reads, so a twilight
  sky has night pages."
  [night]
  (let [turn-start 0.2
        turn-end 0.32
        fraction (max 0.0 (min 1.0 (/ (- night turn-start) (- turn-end turn-start))))]
    (* fraction fraction (- 3 (* 2 fraction)))))

(defn dusk-amount
  "How strongly the sky shows sunset colors: strongest halfway between day
  and night."
  [night]
  (- 1 (Math/abs (- (* 2 night) 1))))

(defn approach
  "Moves `current` toward `target` by a share of the gap that depends on
  elapsed time, so the sky eases into a new mode at any frame rate."
  [current target elapsed-seconds]
  (let [seconds-to-close-most 1.2
        ;; Three time constants close 95 percent of the gap.
        time-constant (/ seconds-to-close-most 3)]
    (+ current (* (- target current) (- 1 (Math/exp (/ (- elapsed-seconds) time-constant)))))))

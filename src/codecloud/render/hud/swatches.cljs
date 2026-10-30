(ns codecloud.render.hud.swatches
  "Small marks in the colors of color layers, which follow the sky, for
  the panels that show and explain those layers."
  (:require [clojure.string :as string]
            [codecloud.presentation.color :as color]))

(defn dot
  "A dot in a {:day :night} color, or an empty ring for nil, which stands
  for a layer that leaves the page alone."
  [colors]
  (if colors
    [:span.layer-dot {:style {"--day-color" (color/oklch->hex (:day colors))
                              "--night-color" (color/oklch->hex (:night colors))}}]
    [:span.layer-dot.is-empty]))

(defn choosable
  "A dot that opens the color chooser for `target`, [layer-key entry],
  under itself."
  [dispatch colors target label]
  [:button.swatch-button {:type "button"
                          :aria-label (str "Color of " label)
                          :title "Choose a color"
                          :on-click (fn [^js event]
                                      (.stopPropagation event)
                                      (let [^js bounds (.getBoundingClientRect (.-currentTarget event))]
                                        (dispatch [:open-color-chooser {:target target
                                                                        :label label
                                                                        :x (.-left bounds)
                                                                        :y (.-bottom bounds)}])))}
   (dot colors)])

(defn strip
  "The first few colors of a category layer's legend, side by side."
  [entries]
  (let [shown 6]
    [:span.layer-strip (map dot (take shown entries))]))

(defn ramp
  "A bar from a heat layer's low color to its high one through the colors
  between, turning the shorter way round the hue wheel."
  [{:keys [low high]}]
  (let [stop-count 5
        gradient (fn [theme-name]
                   (str "linear-gradient(90deg, "
                        (string/join ", " (map (fn [index]
                                                 (color/oklch->hex (color/mix-oklch (get low theme-name) (get high theme-name)
                                                                                    (/ index (dec stop-count)))))
                                               (range stop-count)))
                        ")"))]
    [:span.layer-ramp {:style {"--day-ramp" (gradient :day) "--night-ramp" (gradient :night)}}]))

(defn preview
  "A layer's colors in brief: its ramp for a heat layer, and a strip of its
  first colors otherwise."
  [{:keys [kind legend]} context]
  (when context
    (if (= :heat kind)
      (ramp (legend context))
      (strip (legend context)))))

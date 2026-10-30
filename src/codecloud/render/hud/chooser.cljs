(ns codecloud.render.hud.chooser
  "The color chooser: swatches that read on both skies, a custom color, and
  the way back to the default. It opens under the dot that was clicked."
  (:require [codecloud.presentation.color :as color]
            [codecloud.presentation.layers :as layers]
            [codecloud.presentation.palette :as palette]
            [codecloud.render.dom :as dom]
            [codecloud.render.icons :as icons]))

(defn- hexes
  "A {:day :night} color as its two hex strings, for comparing colors."
  [colors]
  (when colors
    [(color/oklch->hex (:day colors)) (color/oklch->hex (:night colors))]))

(defn- chosen-colors
  "The colors of a picked hex value, brightened by night as the cloud
  file's hex colors are."
  [^js event]
  (layers/color-named (.. event -target -value)))

(defn- chooser-content [dispatch {:keys [target label]}]
  (list
   [:header.chooser-header
    [:span.chooser-label label]
    [:button.icon-button {:aria-label "Close" :on-click #(dispatch [:close-color-chooser])} icons/close]]
   [:div.chooser-swatches {:role "group" :aria-label (str "Colors for " label)}
    (for [colors palette/chooser-colors
          :let [[day-hex night-hex] (hexes colors)]]
      [:button.chooser-swatch {:type "button"
                               :aria-label day-hex
                               :data-colors (str day-hex night-hex)
                               :style {"--day-color" day-hex "--night-color" night-hex}
                               :on-click #(dispatch [:choose-color target colors])}])]
   [:div.chooser-actions
    [:label.chooser-custom
     [:input.chooser-input {:type "color"
                            :on-input #(dispatch [:preview-color target (chosen-colors %)])
                            :on-change #(dispatch [:choose-color target (chosen-colors %)])}]
     [:span "Custom"]]
    [:button.text-button.chooser-default {:on-click #(dispatch [:reset-color target])} "Default"]]))

(defn- place!
  "Puts the chooser under its dot, kept inside the window."
  [^js root {:keys [x y]}]
  (let [gap 6
        margin 8
        width (.-offsetWidth root)
        height (.-offsetHeight root)
        left (max margin (min (- x 10) (- (.-innerWidth js/window) width margin)))
        top (if (> (+ y gap height) (- (.-innerHeight js/window) margin))
              (max margin (- y height (* 2 gap) 18))
              (+ y gap))]
    (set! (.. root -style -left) (str left "px"))
    (set! (.. root -style -top) (str top "px"))))

(defn- show-current!
  "Marks the swatch of the current color, sets the custom input to it
  unless the input is in use, and offers the default while a color is
  chosen."
  [^js root current chosen?]
  (let [[current-day current-night] (hexes current)
        ^js input (dom/find-in root ".chooser-input")]
    (doseq [^js swatch (array-seq (.querySelectorAll root ".chooser-swatch"))]
      (dom/toggle-class! swatch "is-current" (= (str current-day current-night) (.getAttribute swatch "data-colors"))))
    (when (and current-day (not= input (.-activeElement js/document)))
      (set! (.-value input) current-day))
    (dom/toggle-class! (dom/find-in root ".chooser-default") "is-hidden" (not chosen?))))

(def panel
  {:id :chooser
   :mount (fn [dispatch]
            (let [root (dom/element [:div.hud-chooser {:role "dialog" :aria-label "Choose a color"}])]
              ;; Keys stay out of the cloud, and Escape closes the chooser.
              (.addEventListener root "keydown" (fn [^js event]
                                                  (when (= "Escape" (.-key event))
                                                    (dispatch [:close-color-chooser]))
                                                  (.stopPropagation event)))
              {:root root :dispatch dispatch :built (atom nil)}))
   :update (fn [{:keys [root dispatch built]} {:keys [color-chooser layer-registry layer-contexts color-overrides]}]
             (dom/toggle-class! root "is-open" (some? color-chooser))
             (if color-chooser
               (do (when (not= color-chooser @built)
                     (reset! built color-chooser)
                     (dom/replace-children! root (chooser-content dispatch color-chooser))
                     (place! root color-chooser))
                   (show-current! root
                                  (layers/target-color layer-registry layer-contexts (:target color-chooser))
                                  (some? (get-in color-overrides (:target color-chooser)))))
               (reset! built nil)))})

(ns codecloud.render.hud.arrange
  "The arrange panel: placement, color layers, code colors, spacing, and
  putting moved pages back."
  (:require [codecloud.presentation.arrangement :as arrangement]
            [codecloud.presentation.ui-state :as ui-state]
            [codecloud.render.dom :as dom]
            [codecloud.render.hud.swatches :as swatches]
            [codecloud.render.icons :as icons]))

(defn- spread->slider
  "Slider position for a spacing, logarithmic so halving and doubling
  take equal travel."
  [spread]
  (js/Math.log spread))

(defn- slider->spread [position]
  (js/Math.exp position))

(defn- option-buttons
  "Buttons choosing one of `options` by :key."
  [dispatch options chosen-key event-kind]
  [:div.option-grid {:role "group"}
   (for [{option-key :key :keys [label description]} options]
     [:button.option {:class (when (= option-key chosen-key) "is-on")
                      :aria-pressed (str (= option-key chosen-key))
                      :title description
                      :on-click #(dispatch [event-kind option-key])}
      label])])

(defn- layer-row
  "A color layer's checkbox and preview."
  [dispatch contexts {layer-key :key :keys [label] :as layer}]
  [:li
   [:label.layer-row
    [:input.layer-toggle {:type "checkbox"
                          :data-layer (str layer-key)
                          :on-change #(dispatch [:toggle-color-layer layer-key])}]
    [:span.layer-name label]
    (swatches/preview layer (get contexts layer-key))]])

(defn- color-section
  "Color layer checkboxes, built-in first, then the cloud file's, and
  with folder clouds in view, whether they take their folders' colors."
  [dispatch {:keys [layer-registry layer-contexts arrangement]}]
  (let [cloud-file-layer? #(= "cloud" (namespace (:key %)))
        [built-in declared] ((juxt remove filter) cloud-file-layer? layer-registry)]
    [:section.arrange-section
     [:div.section-heading
      [:h3 "Color by"]
      [:button.text-button.default-layers {:on-click #(dispatch [:set-color-layers nil])} "Defaults"]]
     [:ul.layer-list (map #(layer-row dispatch layer-contexts %) built-in)]
     (when (seq declared)
       (list [:h4.layer-group "Cloud file"]
             [:ul.layer-list (map #(layer-row dispatch layer-contexts %) declared)]))
     (when (arrangement/shows-folder-clouds? arrangement)
       [:label.layer-row.tint-row
        [:input.cloud-tint-toggle {:type "checkbox" :on-change #(dispatch [:toggle-cloud-tint])}]
        [:span.layer-name "Tinted clouds"]])]))

(defn- show-layers!
  "Checks the boxes in place, so a toggled box keeps focus, and offers
  Defaults while the layers differ from them."
  [^js root state]
  (let [enabled (into #{} (map str) (:active-layers state))]
    (doseq [^js toggle (array-seq (.querySelectorAll root ".layer-toggle"))]
      (set! (.-checked toggle) (contains? enabled (.getAttribute toggle "data-layer"))))
    (when-let [^js tint-toggle (dom/find-in root ".cloud-tint-toggle")]
      (set! (.-checked tint-toggle) (:tint-clouds? state)))
    (when-let [defaults-button (dom/find-in root ".default-layers")]
      (dom/toggle-class! defaults-button "is-hidden" (ui-state/colored-by-default? state)))))

(defn- code-section
  "The switch giving code the editor theme's colors."
  [dispatch]
  [:section.arrange-section
   [:h3 "Code"]
   [:label.layer-row.switch-row
    [:input.editor-colors-toggle {:type "checkbox" :on-change #(dispatch [:toggle-editor-colors])}]
    [:span.layer-name "Editor theme colors"]]
   [:p.arrange-note.editor-colors-missing "The editor theme's files could not be read."]])

(defn- show-code-colors!
  "Checks the editor colors switch in place, and notes when the theme
  could not be read while the switch is on."
  [^js root {:keys [settings editor-colors editor-colors-read?]}]
  (when-let [^js toggle (dom/find-in root ".editor-colors-toggle")]
    (set! (.-checked toggle) (true? (:editor-colors? settings))))
  (when-let [note (dom/find-in root ".editor-colors-missing")]
    (dom/toggle-class! note "is-hidden" (not (and (:editor-colors? settings) editor-colors-read? (nil? editor-colors))))))

(defn- spacing-section [dispatch]
  (let [{spread-least :least spread-most :most} ui-state/spread-limits
        slider-step 0.01]
    [:section.arrange-section
     [:h3 [:label {:for "codecloud-spacing"} "Spacing"]]
     [:div.spacing-row
      [:span.spacing-end "Close"]
      [:input#codecloud-spacing.spacing-slider {:type "range"
                                                :min (spread->slider spread-least)
                                                :max (spread->slider spread-most)
                                                :step slider-step}]
      [:span.spacing-end "Open"]]
     [:button.text-button.usual-spacing {:on-click #(dispatch [:set-spread 1])} "Usual spacing"]]))

(defn- arrange-content [dispatch {:keys [arrangement moved-pages] :as state}]
  (list
   [:header.panel-header
    [:h2 "Arrange"]
    [:button.icon-button {:aria-label "Close" :on-click #(dispatch [:toggle-arrange-panel])} icons/close]]
   [:section.arrange-section
    [:h3 "Place pages by"]
    (option-buttons dispatch arrangement/arrangements arrangement :set-arrangement)]
   (color-section dispatch state)
   (code-section dispatch)
   (spacing-section dispatch)
   [:section.arrange-section
    [:button.action-button.put-back-button {:disabled (empty? moved-pages)
                                            :on-click #(dispatch [:put-everything-back])}
     icons/put-back [:span "Put every page back"]]]))

(defn- attach-slider!
  "Previews spacing while the slider moves and keeps it on release."
  [^js root dispatch]
  (let [^js slider (dom/find-in root ".spacing-slider")
        chosen-spread #(slider->spread (js/parseFloat (.-value slider)))]
    (.addEventListener slider "input" #(dispatch [:preview-spread (chosen-spread)]))
    (.addEventListener slider "change" #(dispatch [:set-spread (chosen-spread)]))
    slider))

(defn- show-spread!
  "Moves the slider to `spread` unless already there, and offers the
  usual spacing when it differs."
  [^js root ^js slider spread]
  (let [position (spread->slider spread)
        tolerance 0.001]
    (when (> (js/Math.abs (- (js/parseFloat (.-value slider)) position)) tolerance)
      (set! (.-value slider) position))
    (.setAttribute slider "aria-valuetext" (str (.toFixed (* 100 spread) 0) " percent of the usual spacing"))
    (dom/toggle-class! (dom/find-in root ".usual-spacing") "is-hidden" (< (js/Math.abs (- spread 1)) tolerance))))

(def panel
  {:id :arrange
   :mount (fn [dispatch]
            (let [root (dom/element [:aside.hud-arrange {:aria-label "Arrange"}])]
              ;; The panel's own keys stay out of the cloud, except Escape, which closes it.
              (.addEventListener root "keydown" (fn [^js event]
                                                  (when-not (= "Escape" (.-key event))
                                                    (.stopPropagation event))))
              {:root root :dispatch dispatch :shown (atom nil) :slider (atom nil)}))
   :update (fn [{:keys [root dispatch shown slider]} {:keys [arrange-open? arrangement moved-pages spread
                                                             layer-registry layer-contexts]
                                                      :as state}]
             (dom/toggle-class! root "is-open" arrange-open?)
             (when arrange-open?
               (let [shown-key [arrangement (empty? moved-pages) layer-registry layer-contexts]]
                 (when (not= shown-key @shown)
                   (reset! shown shown-key)
                   (dom/replace-children! root (arrange-content dispatch state))
                   (reset! slider (attach-slider! root dispatch))))
               (show-layers! root state)
               (show-code-colors! root state)
               (show-spread! root @slider spread)))})

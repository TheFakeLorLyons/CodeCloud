(ns codecloud.render.hud.legend
  "What the colors, marks, and lines in view stand for, and the keys."
  (:require [codecloud.collections :as collections]
            [codecloud.presentation.scene :as scene]
            [codecloud.render.dom :as dom]
            [codecloud.render.hud.swatches :as swatches]
            [codecloud.render.hud.words :as words]
            [codecloud.render.icons :as icons]))

(defn- entry-dot
  "A legend entry's dot, which opens the color chooser when the entry has
  a color to choose."
  [dispatch colors target label]
  (if target
    (swatches/choosable dispatch colors target label)
    (swatches/dot colors)))

(defn- layer-legend
  "One layer's colors and what they stand for, or its ramp."
  [dispatch {:keys [label kind legend]} context]
  (let [listed (legend context)]
    [:section.legend-layer
     [:h3 label]
     (if (= :heat kind)
       (let [{:keys [low high low-label high-label low-target high-target unclassified]} listed]
         (list
          [:div.legend-ramp
           (entry-dot dispatch low low-target (str label ", " low-label))
           [:span low-label] (swatches/ramp listed) [:span high-label]
           (entry-dot dispatch high high-target (str label ", " high-label))]
          (when unclassified
            [:ul.family-list [:li (swatches/dot unclassified) [:span "Unclassified"]]])))
       [:ul.family-list
        (for [{entry-label :label :keys [target] :as entry} listed]
          [:li (entry-dot dispatch entry target entry-label) [:span entry-label]])])]))

(defn- key-row [swatch text]
  [:div.legend-row [:dt swatch] [:dd text]])

(def ^:private quiet-edge-words
  "What each kind of faint line stands for."
  {:alias "alias"
   :implicit "use without a require"
   :link "link"})

(defn- mark-rows
  "Rows for the cycles, import loops, and parse errors in view."
  [{:keys [totals]}]
  (list
   (when (pos? (:cycles totals 0))
     (key-row [:span.swatch.swatch-cycle "↻"] "Cyclic dependency"))
   (when (pos? (:cycle-warnings totals 0))
     (key-row [:span.swatch.swatch-loop "↻"] "Cyclic import its language allows"))
   (when (pos? (:parse-errors totals 0))
     (key-row [:span.swatch.swatch-parse-error "⚠"] "Parse error"))))

(defn- line-rows
  "Rows for the kinds of line in view."
  [{:keys [edges]} active-layers]
  (let [quiet? #(contains? scene/quiet-edge-kinds (:kind %))
        quiet-kinds (into #{} (comp (filter quiet?) (map :kind)) edges)]
    (list
     (when (some (complement quiet?) edges)
       (key-row [:span.swatch.swatch-line] "Dependency"))
     (when (seq quiet-kinds)
       (key-row [:span.swatch.swatch-line.is-quiet]
                (words/capitalized (collections/spoken-list (keep #(when (contains? quiet-kinds %) (quiet-edge-words %))
                                                                  [:alias :implicit :link])
                                                            "or"))))
     (when (and (contains? active-layers :language) (some :cross-language? edges))
       (key-row [:span.swatch.swatch-line.is-heavy] "Between languages")))))

(def ^:private key-rows
  [["Drag" "Orbit"]
   ["Right-drag" "Pan"]
   ["Scroll" "Zoom, and into code to open it"]
   ["Click" "Bring closer, again to set back"]
   ["Click code" "Edit it there, Esc to stop"]
   ["Ctrl+S" "Save while editing"]
   ["Double-click" "Fly there"]
   ["Ctrl or Alt drag" "Move a page"]
   ["/" "Find"]
   ["N" "Code or names"]
   ["A" "Arrange"]
   ["[ ]" "Spacing"]
   ["?" "Legend"]])

(defn- legend-content
  [dispatch {:keys [layer-registry layer-contexts active-layers] snapshot :visible-snapshot}]
  (list
   [:header.panel-header
    [:h2 "Legend"]
    [:button.icon-button {:aria-label "Close the legend" :on-click #(dispatch [:toggle-legend])}
     icons/close]]
   (when (seq layer-contexts)
     (for [layer layer-registry
           :when (contains? active-layers (:key layer))]
       (layer-legend dispatch layer (get layer-contexts (:key layer)))))
   [:dl.legend-marks
    (mark-rows snapshot)
    (line-rows snapshot active-layers)]
   [:table.legend-keys
    [:tbody (for [[gesture action] key-rows]
              [:tr [:th {:scope "row"} gesture] [:td action]])]]))

(def panel
  {:id :legend
   :mount (fn [dispatch]
            {:root (dom/element [:aside.hud-legend {:aria-label "Legend"}])
             :dispatch dispatch
             :shown (atom nil)})
   :update (fn [{:keys [root dispatch shown]} {:keys [legend-open? active-layers visible-snapshot
                                                      layer-registry layer-contexts]
                                               :as state}]
             (dom/toggle-class! root "is-open" legend-open?)
             (when legend-open?
               (let [shown-key [active-layers visible-snapshot layer-registry layer-contexts]]
                 (when (not= shown-key @shown)
                   (reset! shown shown-key)
                   (dom/replace-children! root (legend-content dispatch state))))))})

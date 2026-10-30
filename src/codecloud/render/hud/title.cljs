(ns codecloud.render.hud.title
  "The cloud's name, the snapshot shown, the totals table, and the
  arrangement, layers, and filter when they differ from the usual."
  (:require [clojure.string :as string]
            [codecloud.languages.catalog :as catalog]
            [codecloud.presentation.arrangement :as arrangement]
            [codecloud.presentation.ui-state :as ui-state]
            [codecloud.presentation.visibility :as visibility]
            [codecloud.presentation.wording :as wording]
            [codecloud.render.dom :as dom]
            [codecloud.render.hud.words :as words]
            [codecloud.render.icons :as icons]))

(defn- language-rows
  "[label pages lines] for each language of a snapshot, most pages first."
  [snapshot]
  (->> (vals (:namespaces snapshot))
       (group-by #(:language % :clojure))
       (map (fn [[language-id nodes]]
              [(:label (catalog/language language-id))
               (count nodes)
               (reduce + 0 (map #(get-in % [:metrics :lines] 0) nodes))]))
       (sort-by (fn [[label page-count]] [(- page-count) label]))))

(defn- totals-table
  "Pages and lines for each language, with a sum when there are several,
  then the cycles, import loops, and parse errors present."
  [{:keys [totals] :as snapshot}]
  (let [[_ plural] (wording/unit-words snapshot)
        rows (language-rows snapshot)
        row (fn [label & cells] [:tr [:th {:scope "row"} label] (map (fn [cell] [:td cell]) cells)])
        counted-row (fn [label value] (when (pos? (or value 0)) (row label (words/formatted-count value) "")))]
    [:table.totals-table
     [:thead [:tr [:td] [:th {:scope "col"} (words/capitalized plural)] [:th {:scope "col"} "Lines"]]]
     [:tbody
      (for [[label page-count line-count] rows]
        (row label (words/formatted-count page-count) (words/formatted-count line-count)))]
     (when (> (count rows) 1)
       [:tbody.totals-sum
        (row "All" (words/formatted-count (count (:namespaces snapshot))) (words/formatted-count (:lines totals)))])
     [:tbody.totals-problems
      (counted-row "Cyclic dependencies" (:cycles totals))
      (counted-row "Cyclic imports" (:cycle-warnings totals))
      (counted-row "Parse errors" (:parse-errors totals))]]))

(defn- arrangement-caption
  "Placement and color layers joined with plus signs when either differs
  from the usual, or nil."
  [{:keys [arrangement layer-registry active-layers] :as state}]
  (let [placement (when-not (= :folders arrangement) (:label (arrangement/arrangement arrangement)))
        colors (when-not (ui-state/colored-by-default? state)
                 (let [labels (keep #(when (contains? active-layers (:key %)) (:label %)) layer-registry)]
                   (if (seq labels) (string/join " + " labels) "Gray")))]
    (when (or placement colors)
      (string/join ", " (remove nil? [placement colors])))))

(defn- publish-bottom!
  "Tells the stylesheet where the title ends, so the legend below it
  leaves the title, its totals, and its filter in view."
  [^js root]
  (.setProperty (.-style (.-documentElement js/document)) "--title-bottom"
                (str (js/Math.ceil (.-bottom (.getBoundingClientRect root))) "px")))

(def panel
  {:id :title
   :mount (fn [dispatch]
            (let [root (dom/element
                        [:header.hud-title
                         [:h1.cloud-name "CodeCloud"]
                         [:p.snapshot-caption ""]
                         [:button.arrangement-caption {:title "Arrange the cloud (A)"
                                                       :on-click #(dispatch [:toggle-arrange-panel])}]
                         [:button.totals-toggle {:aria-expanded "false" :on-click #(dispatch [:toggle-totals])}
                          icons/disclosure [:span "Totals"]]
                         [:div.totals ""]
                         [:div.filter-chip
                          [:span.filter-text ""]
                          [:button.text-button {:on-click #(dispatch [:clear-filter])} "Show everything"]]])]
              (.observe (js/ResizeObserver. #(publish-bottom! root)) root)
              {:root root :shown (atom nil)}))
   :update (fn [{:keys [root shown]} {:keys [cloud-name view-filter totals-open?] snapshot :visible-snapshot :as state}]
             (dom/set-text! (dom/find-in root ".cloud-name") (or cloud-name "CodeCloud"))
             (dom/set-text! (dom/find-in root ".snapshot-caption") (words/snapshot-caption state))
             (let [caption (arrangement-caption state)
                   caption-button (dom/find-in root ".arrangement-caption")]
               (dom/toggle-class! caption-button "is-hidden" (nil? caption))
               (dom/set-text! caption-button (or caption "")))
             (let [toggle (dom/find-in root ".totals-toggle")
                   totals (dom/find-in root ".totals")
                   open? (boolean (and totals-open? (:totals snapshot)))]
               (dom/toggle-class! toggle "is-hidden" (nil? (:totals snapshot)))
               (dom/toggle-class! toggle "is-open" open?)
               (.setAttribute toggle "aria-expanded" (str open?))
               (dom/toggle-class! totals "is-open" open?)
               (when (and open? (not= snapshot @shown))
                 (reset! shown snapshot)
                 (dom/replace-children! totals (totals-table snapshot))))
             (let [description (visibility/describe view-filter)]
               (dom/toggle-class! (dom/find-in root ".filter-chip") "is-open" (some? description))
               (dom/set-text! (dom/find-in root ".filter-text") (or description ""))))})

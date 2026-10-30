(ns codecloud.render.hud.toolbar
  "The row of buttons at the top right of the sky."
  (:require [codecloud.presentation.ui-state :as ui-state]
            [codecloud.presentation.visibility :as visibility]
            [codecloud.render.dom :as dom]
            [codecloud.render.icons :as icons]))

(def ^:private sky-buttons
  {:auto {:icon icons/follow-theme :label "Sky follows the editor theme"}
   :day {:icon icons/sun :label "Daytime sky"}
   :twilight {:icon icons/twilight :label "Twilight sky"}
   :night {:icon icons/moon :label "Night sky"}
   :clock {:icon icons/clock :label "Sky follows the time of day"}})

(def ^:private display-buttons
  {:code {:icon icons/pages-mode :label "Show names only (N)"}
   :names {:icon icons/names-mode :label "Show code (N)"}})

(defn- show-button! [^js button {:keys [icon label]} title]
  (dom/replace-children! button icon)
  (.setAttribute button "title" title)
  (.setAttribute button "aria-label" label))

(defn- arranged-unusually?
  "True when the cloud is arranged, colored, spaced, or moved other than
  the usual way, which the arrange button marks."
  [{:keys [arrangement spread moved-pages] :as state}]
  (let [spread-tolerance 0.01]
    (or (not= :folders arrangement)
        (not (ui-state/colored-by-default? state))
        (> (js/Math.abs (- spread 1)) spread-tolerance)
        (seq moved-pages))))

(def panel
  {:id :toolbar
   :mount (fn [dispatch]
            {:root (dom/element
                    [:nav.hud-toolbar {:aria-label "Cloud controls"}
                     [:button.icon-button.search-button {:title "Find (/)" :aria-label "Find"
                                                         :on-click #(dispatch [:open-search])}
                      icons/search]
                     [:button.icon-button.views-button {:title "Views"
                                                        :aria-label "Views"
                                                        :on-click #(dispatch [:toggle-views-panel])}
                      icons/views]
                     [:button.icon-button.arrange-button {:title "Arrange (A)"
                                                          :aria-label "Arrange"
                                                          :on-click #(dispatch [:toggle-arrange-panel])}
                      icons/arrange]
                     [:button.icon-button.display-button {:on-click #(dispatch [:toggle-display-mode])}]
                     [:button.icon-button.sky-button {:on-click #(dispatch [:cycle-sky-mode])}]
                     [:button.icon-button {:title "Show the whole cloud (Home)" :aria-label "Show the whole cloud"
                                           :on-click #(dispatch [:frame-all])}
                      icons/frame-all]
                     [:button.icon-button.legend-button {:title "Legend (?)"
                                                         :aria-label "Legend"
                                                         :on-click #(dispatch [:toggle-legend])}
                      icons/legend]])
             :shown (atom nil)})
   :update (fn [{:keys [root shown]} {:keys [sky-mode display-mode legend-open? views-open? view-filter arrange-open?]
                                      :as state}]
             (when (not= [sky-mode display-mode] @shown)
               (reset! shown [sky-mode display-mode])
               (let [sky-button (get sky-buttons sky-mode (:auto sky-buttons))
                     display-button (get display-buttons display-mode (:code display-buttons))]
                 (show-button! (dom/find-in root ".sky-button") sky-button (str (:label sky-button) ". Click to change."))
                 (show-button! (dom/find-in root ".display-button") display-button (:label display-button))))
             (dom/toggle-class! (dom/find-in root ".legend-button") "is-active" legend-open?)
             (dom/toggle-class! (dom/find-in root ".views-button") "is-active"
                                (or views-open? (visibility/filtering? view-filter)))
             (dom/toggle-class! (dom/find-in root ".arrange-button") "is-active"
                                (or arrange-open? (arranged-unusually? state))))})

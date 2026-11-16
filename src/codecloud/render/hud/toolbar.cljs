(ns codecloud.render.hud.toolbar
  "The row of buttons at the top right of the sky."
  (:require [clojure.string :as string]
            [codecloud.presentation.environments :as environments]
            [codecloud.presentation.ui-state :as ui-state]
            [codecloud.presentation.visibility :as visibility]
            [codecloud.render.dom :as dom]
            [codecloud.render.icons :as icons]))

(def ^:private environment-icons
  {:simple icons/simple
   :auto icons/follow-theme
   :day icons/sun
   :twilight icons/twilight
   :night icons/moon
   :clock icons/clock
   :backrooms icons/backrooms})

(defn- environment-icon [id]
  (get environment-icons id icons/space))

(defn- environment-choice
  "One environment in the menu under the sky button, marked when chosen."
  [dispatch {:keys [id description] environment-name :name} chosen?]
  [:button.sky-choice {:type "button" :role "menuitemradio" :aria-checked (str chosen?) :title description
                       :class (when chosen? "is-chosen")
                       :on-click (fn [^js event]
                                   ;; A pointer's click leaves the menu to close
                                   ;; once the pointer leaves it; a key's keeps focus.
                                   (when (pos? (.-detail event))
                                     (.blur (.-currentTarget event)))
                                   (dispatch [:choose-sky-mode id]))}
   (environment-icon id)
   [:span.sky-choice-name environment-name]
   [:span.sky-choice-mark (when chosen? icons/check)]])

(defn- show-environments!
  "Shows the chosen environment on the sky button and lists every
  environment in its menu, the described spaces under their own heading."
  [root dispatch {:keys [sky-mode spaces] :as state}]
  (let [chosen (ui-state/shown-environment state)
        label (str "Environment: " (:name chosen) ". Click for the next one")
        ^js button (dom/find-in root ".sky-button")
        choices (fn [environments] (map #(environment-choice dispatch % (= sky-mode (:id %))) environments))]
    (dom/replace-children! button (environment-icon (:id chosen)))
    (.setAttribute button "aria-label" label)
    (dom/replace-children! (dom/find-in root ".sky-menu-list")
                           (concat (choices environments/built-in)
                                   (when (seq spaces)
                                     (cons [:div.sky-menu-heading "Spaces from settings"] (choices spaces)))))))

(def ^:private display-buttons
  {:code {:icon icons/pages-mode :label "Show names only (N)"}
   :names {:icon icons/names-mode :label "Show code (N)"}})

(defn- show-button! [^js button {:keys [icon label]} title]
  (dom/replace-children! button icon)
  (.setAttribute button "title" title)
  (.setAttribute button "aria-label" label))

(defn- agents-title
  "What the agents button says about the agent hub and who is connected."
  [{:keys [serving? agents problem]}]
  (cond
    problem (str "Coworkers. The agent hub did not start: " problem)
    (not serving?) "Coworkers. The agent hub is off"
    (empty? agents) "Coworkers. No agent is connected"
    :else (str "Coworkers: " (string/join ", " (map :name agents)))))

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
                     [:button.icon-button.new-file-button {:title "New file" :aria-label "New file"
                                                           :on-click #(dispatch [:new-file nil])}
                      icons/new-file]
                     [:button.icon-button.views-button {:title "Views"
                                                        :aria-label "Views"
                                                        :on-click #(dispatch [:toggle-views-panel])}
                      icons/views]
                     [:button.icon-button.arrange-button {:title "Arrange (A)"
                                                          :aria-label "Arrange"
                                                          :on-click #(dispatch [:toggle-arrange-panel])}
                      icons/arrange]
                     [:button.icon-button.repl-button {:title "REPL (R)"
                                                       :aria-label "REPL"
                                                       :on-click #(dispatch [:toggle-repl])}
                      icons/repl]
                     [:button.icon-button.agents-button {:on-click #(dispatch [:toggle-coworkers])}
                      icons/agents
                      [:span.agents-count {:aria-hidden "true"}]]
                     [:button.icon-button.assistant-button {:title "Assistant (Q)"
                                                            :aria-label "Assistant"
                                                            :on-click #(dispatch [:toggle-assistant])}
                      icons/assistant]
                     [:button.icon-button.display-button {:on-click #(dispatch [:toggle-display-mode])}]
                     [:div.sky-picker
                      [:button.icon-button.sky-button {:aria-haspopup "menu" :on-click #(dispatch [:cycle-sky-mode])}]
                      [:div.sky-menu
                       [:div.sky-menu-list {:role "menu" :aria-label "Environments"}]]]
                     [:button.icon-button {:title "Show the whole cloud (Home)" :aria-label "Show the whole cloud"
                                           :on-click #(dispatch [:frame-all])}
                      icons/frame-all]
                     [:button.icon-button.legend-button {:title "Legend (?)"
                                                         :aria-label "Legend"
                                                         :on-click #(dispatch [:toggle-legend])}
                      icons/legend]])
             :dispatch dispatch
             :shown (atom nil)
             :shown-environments (atom nil)
             :shown-agents (atom nil)})
   :update (fn [{:keys [root dispatch shown shown-environments shown-agents]}
                {:keys [sky-mode spaces display-mode legend-open? views-open? view-filter arrange-open? repl agents presence assistant]
                 :as state}]
             (when (not= [sky-mode spaces] @shown-environments)
               (reset! shown-environments [sky-mode spaces])
               (show-environments! root dispatch state))
             (when (not= display-mode @shown)
               (reset! shown display-mode)
               (let [display-button (get display-buttons display-mode (:code display-buttons))]
                 (show-button! (dom/find-in root ".display-button") display-button (:label display-button))))
             (set! (.-disabled (dom/find-in root ".new-file-button")) (not= :working-tree (get-in state [:snapshot :id])))
             (dom/toggle-class! (dom/find-in root ".legend-button") "is-active" legend-open?)
             (dom/toggle-class! (dom/find-in root ".repl-button") "is-active" (:open? repl))
             (dom/toggle-class! (dom/find-in root ".assistant-button") "is-active"
                                (or (:open? assistant) (:running? assistant)))
             (when (not= agents @shown-agents)
               (reset! shown-agents agents)
               (let [button (dom/find-in root ".agents-button")
                     present (count (:agents agents))
                     title (agents-title agents)]
                 (.setAttribute button "title" title)
                 (.setAttribute button "aria-label" title)
                 (set! (.-textContent (dom/find-in root ".agents-count")) (if (pos? present) (str present) ""))
                 (dom/toggle-class! button "has-problem" (some? (:problem agents)))))
             (dom/toggle-class! (dom/find-in root ".agents-button") "is-active"
                                (or (:pane-open? presence) (pos? (count (:agents agents)))))
             (dom/toggle-class! (dom/find-in root ".views-button") "is-active"
                                (or views-open? (visibility/filtering? view-filter)))
             (dom/toggle-class! (dom/find-in root ".arrange-button") "is-active"
                                (or arrange-open? (arranged-unusually? state))))})

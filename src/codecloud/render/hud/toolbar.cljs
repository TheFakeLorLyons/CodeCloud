(ns codecloud.render.hud.toolbar
  "The row of buttons at the top right of the sky."
  (:require [clojure.string :as string]
            [codecloud.presentation.environments :as environments]
            [codecloud.presentation.palette :as palette]
            [codecloud.presentation.ui-state :as ui-state]
            [codecloud.presentation.visibility :as visibility]
            [codecloud.presentation.windows-state :as windows-state]
            [codecloud.render.dom :as dom]
            [codecloud.render.icons :as icons]))

(def ^:private environment-icons
  {:simple icons/simple
   :auto icons/follow-theme
   :day icons/sun
   :twilight icons/twilight
   :night icons/moon
   :clock icons/clock
   :backrooms icons/backrooms
   :night-library icons/library})

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

(defn- simple-color-row
  "The color Simple shows, picked under the environments, and a way back
  to the editor theme's colors. Picking one shows Simple."
  [dispatch]
  (let [picked (fn [^js event] (.. event -target -value))]
    [:div.sky-color-row
     [:label.sky-color-name {:for "codecloud-simple-color"} "Simple's color"]
     [:input#codecloud-simple-color.sky-color {:type "color"
                                                :on-input #(dispatch [:preview-simple-color (picked %)])
                                                :on-change #(dispatch [:choose-simple-color (picked %)])}]
     [:button.text-button.sky-color-default {:type "button" :on-click #(dispatch [:choose-simple-color nil])}
      "Default"]]))

(defn- show-simple-color!
  "Shows Simple's chosen color in its picker, or the editor theme's color
  overhead when none is chosen, and offers the default only while one is."
  [^js root {:keys [simple-color theme-kind]}]
  (let [^js picker (dom/find-in root ".sky-color")
        shown (or simple-color (palette/hex-of (if (= :light theme-kind) :day :night) :sky-zenith))]
    (when (not= shown (.-value picker))
      (set! (.-value picker) shown))
    (dom/toggle-class! (dom/find-in root ".sky-color-default") "is-hidden" (nil? simple-color))))

(defn- set-icon-color! [^js element color-hex]
  (if color-hex
    (.setProperty (.-style element) "--icon-color" color-hex)
    (.removeProperty (.-style element) "--icon-color")))

(defn- show-icon-colors!
  "Colors each icon its own color unless `toolbar-colors` is :plain, and
  with a map, the colors it names over those, its \"all\" over every icon
  it names no color for."
  [^js root toolbar-colors]
  (let [chosen (if (map? toolbar-colors) toolbar-colors {})]
    (dom/toggle-class! root "is-colored" (not= :plain toolbar-colors))
    (set-icon-color! root (get chosen "all"))
    (doseq [^js button (array-seq (.querySelectorAll root "[data-icon]"))]
      (set-icon-color! button (get chosen (.getAttribute button "data-icon"))))))

(defn- agents-title
  "What the agents button says about the agent hub and who is connected."
  [{:keys [serving? agents problem]}]
  (cond
    problem (str "Coworkers. The agent hub did not start: " problem)
    (not serving?) "Coworkers. The agent hub is off"
    (empty? agents) "Coworkers. No agent is connected"
    :else (str "Coworkers: " (string/join ", " (map :name agents)))))

(defn- windows-menu [dispatch]
  [:div.windows-menu {:role "menu" :aria-label "Windows"}
   [:form.windows-address-form {:on-submit (fn [^js event]
                                             (.preventDefault event)
                                             (let [^js input (.querySelector (.-currentTarget event) "input")]
                                               (dispatch [:open-window {:kind :web :source (.-value input)}])
                                               (set! (.-value input) "")))}
    [:label {:for "codecloud-window-address"} "Show a web page"]
    [:div.windows-address-row
     [:input#codecloud-window-address {:type "text" :placeholder "localhost:3000" :autocomplete "off" :spellcheck "false"
                                       :title "A port alone, such as 3000, shows that port on this computer"}]
     [:button.action-button.is-primary {:type "submit"} "Show"]]]
   [:button.windows-menu-item {:type "button" :on-click #(dispatch [:pick-window-file])}
    icons/document [:span "Show a file from this folder…"]]
   [:div.windows-menu-list]])

(defn- show-window-list!
  "Lists the open windows in the windows menu, each flown to by a click,
  which shows a window hidden from the cloud again."
  [^js root dispatch windows]
  (dom/replace-children! (dom/find-in root ".windows-menu-list")
                         (when (seq windows)
                           (cons [:p.windows-menu-heading "Open windows"]
                                 (for [{:keys [id hidden?] :as window} windows]
                                   [:div.windows-menu-entry
                                    [:button.windows-menu-item {:type "button"
                                                                :class (when hidden? "is-off")
                                                                :title (when hidden? "Hidden from the cloud; a click shows it")
                                                                :on-click #(dispatch [:fly-to-window id])}
                                     [:span (windows-state/window-title window)]]
                                    [:button.windows-menu-close {:type "button" :title "Close" :aria-label "Close"
                                                                 :on-click #(dispatch [:close-window id])}
                                     icons/close]])))))

(def panel
  {:id :toolbar
   :mount (fn [dispatch]
            {:root (dom/element
                    [:nav.hud-toolbar {:aria-label "Cloud controls"}
                     [:button.icon-button.search-button {:title "Find (/)" :aria-label "Find" :data-icon "search"
                                                         :on-click #(dispatch [:open-search])}
                      icons/search]
                     [:button.icon-button.new-file-button {:title "New file" :aria-label "New file" :data-icon "newFile"
                                                           :on-click #(dispatch [:new-file nil])}
                      icons/new-file]
                     [:button.icon-button.views-button {:title "Views" :data-icon "views"
                                                        :aria-label "Views"
                                                        :on-click #(dispatch [:toggle-views-panel])}
                      icons/views]
                     [:button.icon-button.arrange-button {:title "Arrange (A)" :data-icon "arrange"
                                                          :aria-label "Arrange"
                                                          :on-click #(dispatch [:toggle-arrange-panel])}
                      icons/arrange]
                     [:button.icon-button.repl-button {:title "REPL (R)" :data-icon "repl"
                                                       :aria-label "REPL"
                                                       :on-click #(dispatch [:toggle-repl])}
                      icons/repl]
                     [:button.icon-button.agents-button {:data-icon "agents" :on-click #(dispatch [:toggle-coworkers])}
                      icons/agents
                      [:span.agents-count {:aria-hidden "true"}]]
                     [:button.icon-button.assistant-button {:title "Assistant (Q)" :data-icon "assistant"
                                                            :aria-label "Assistant"
                                                            :on-click #(dispatch [:toggle-assistant])}
                      icons/assistant]
                     [:div.windows-picker
                      [:button.icon-button.windows-button {:title "Windows" :aria-label "Windows" :aria-haspopup "menu" :data-icon "windows"
                                                           :on-click #(dispatch [:toggle-windows-menu])}
                       icons/windows]
                      (windows-menu dispatch)]
                     [:button.icon-button.grid-button {:title "Grid (G)" :aria-label "Grid" :data-icon "grid"
                                                       :on-click #(dispatch [:toggle-grid])}
                      icons/grid
                      [:span.grid-count {:aria-hidden "true"}]]
                     [:div.sky-picker
                      [:button.icon-button.sky-button {:aria-haspopup "menu" :data-icon "environment" :on-click #(dispatch [:cycle-sky-mode])}]
                      [:div.sky-menu
                       [:div.sky-menu-panel
                        [:div.sky-menu-list {:role "menu" :aria-label "Environments"}]
                        (simple-color-row dispatch)]]]
                     [:button.icon-button {:title "Show the whole cloud (Home)" :aria-label "Show the whole cloud" :data-icon "wholeCloud"
                                           :on-click #(dispatch [:frame-all])}
                      icons/frame-all]
                     [:button.icon-button.legend-button {:title "Legend (?)" :data-icon "legend"
                                                         :aria-label "Legend"
                                                         :on-click #(dispatch [:toggle-legend])}
                      icons/legend]])
             :dispatch dispatch
             :shown-colors (atom ::nothing-shown)
             :shown-environments (atom nil)
             :shown-agents (atom nil)
             :shown-windows (atom nil)})
   :update (fn [{:keys [root dispatch shown-colors shown-environments shown-agents shown-windows]}
                {:keys [sky-mode spaces legend-open? views-open? view-filter arrange-open? repl agents presence assistant
                        windows windows-menu-open? grid]
                 :as state}]
             (when (not= windows @shown-windows)
               (reset! shown-windows windows)
               (show-window-list! root dispatch windows))
             (let [^js menu (dom/find-in root ".windows-menu")
                   was-open? (.contains (.-classList menu) "is-open")]
               (dom/toggle-class! menu "is-open" windows-menu-open?)
               (when (and windows-menu-open? (not was-open?))
                 (js/setTimeout #(some-> (dom/find-in root "#codecloud-window-address") .focus) 0)))
             (dom/toggle-class! (dom/find-in root ".windows-button") "is-active" (or windows-menu-open? (seq windows)))
             (dom/toggle-class! (dom/find-in root ".grid-button") "is-active" (:shown? grid))
             (dom/set-text! (dom/find-in root ".grid-count") (if (seq (:cells grid)) (str (count (:cells grid))) ""))
             (when (not= [sky-mode spaces] @shown-environments)
               (reset! shown-environments [sky-mode spaces])
               (show-environments! root dispatch state))
             (show-simple-color! root state)
             (let [toolbar-colors (get-in state [:settings :toolbar-colors])]
               (when (not= toolbar-colors @shown-colors)
                 (reset! shown-colors toolbar-colors)
                 (show-icon-colors! root toolbar-colors)))
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
                                (or arrange-open? (not (ui-state/arranged-by-default? state)))))})

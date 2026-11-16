(ns codecloud.render.hud.coworkers
  "The Coworkers pane, listing the agents at work with what each is doing
  and where, the pins they left for the person, and what they did lately,
  each a way to fly there; and the chip saying which agent the camera
  follows."
  (:require [codecloud.agents.presence :as agent-presence]
            [codecloud.counting :refer [ago]]
            [codecloud.presentation.color :as color]
            [codecloud.presentation.presence-state :as presence-state]
            [codecloud.render.dom :as dom]
            [codecloud.render.hud.dock :as dock]
            [codecloud.render.icons :as icons]))

(defn- badge-color [hue theme-key]
  (color/oklch->hex (presence-state/agent-color hue theme-key)))

(defn- initials-mark [{:keys [hue initials]} theme-key small?]
  [:span.coworker-initials {:class (when small? "is-small")
                            :style {"--agent" (badge-color (or hue 220) theme-key)}
                            :aria-hidden "true"}
   (or initials "?")])

(defn- line-words [start end]
  (cond
    (nil? start) nil
    (or (nil? end) (= start end)) (str "line " start)
    :else (str "lines " start "-" end)))

(defn- in-family-order [agents]
  (let [subagents (group-by :parent (filter :parent agents))
        parent-ids (into #{} (map :id) agents)]
    (concat (mapcat (fn [{:keys [id] :as agent}] (cons agent (get subagents id)))
                    (remove :parent agents))
            (filter #(and (:parent %) (not (contains? parent-ids (:parent %)))) agents))))

(defn- agent-row [dispatch {:keys [following]} theme-key {:keys [id parent place doing intent present?] agent-name :name :as agent}]
  (let [followed? (= following id)]
    [:li.coworker {:class [(when parent "is-subagent") (when (false? present?) "is-gone")]}
     (initials-mark agent theme-key false)
     [:div.coworker-text
      [:span.coworker-name (if (= agent-presence/unknown-agent-id id) "An agent CodeCloud could not name" agent-name)
       (when (false? present?) [:span.coworker-note " no longer connected"])]
      (when doing [:span.coworker-doing doing])
      (when (and intent (not= intent doing)) [:span.coworker-doing (str "Intends " intent)])
      (when place
        [:button.coworker-place {:type "button" :data-focus (str "place:" id)
                                 :on-click #(dispatch [:show-agent id])}
         (str (:path place) (when-not (:whole? place) (str ", line " (:line place))))])]
     (when place
       [:button.action-button.coworker-follow {:type "button"
                                               :class (when followed? "is-primary")
                                               :data-focus (str "follow:" id)
                                               :aria-pressed (str followed?)
                                               :on-click #(dispatch (if followed? [:stop-following] [:follow-agent id]))}
        icons/follow
        (if followed? "Following" "Follow")])]))

(defn- pin-row [dispatch theme-key agents-by-id {:keys [id agent-id path line note]}]
  (let [agent (get agents-by-id agent-id)]
    [:li.coworker-pin
     (initials-mark agent theme-key true)
     [:div.coworker-text
      [:span.coworker-doing note]
      [:span.coworker-note (str (or (:name agent) "An agent") " at " path ", line " line)]]
     [:div.coworker-pin-actions
      [:button.action-button {:type "button" :data-focus (str "show-pin:" id) :on-click #(dispatch [:show-pin id])} "Show"]
      [:button.action-button {:type "button" :data-focus (str "dismiss-pin:" id) :on-click #(dispatch [:dismiss-pin id])} "Dismiss"]]]))

(defn- activity-words [{:keys [kind path start end line added removed credited?]}]
  (let [file-name (agent-presence/file-name path)
        lines (line-words start end)]
    (case kind
      :read (str "read " file-name (when lines (str ", " lines)))
      :change (str "changed " file-name (when lines (str ", " lines))
                   (when added (str ", +" added " −" removed))
                   (when credited? ", by a guess"))
      :pin (str "pointed to " file-name ", line " line)
      "")))

(defn- activity-row [dispatch theme-key now {:keys [agent kind path start line at] :as entry}]
  [:li
   [:button.activity-row {:type "button"
                          :data-focus (str "activity:" (name kind) ":" path ":" at)
                          :on-click #(dispatch [:show-place {:path path :line (or line start 1)}])}
    (initials-mark agent theme-key true)
    [:span.activity-words (activity-words entry)]
    [:span.activity-time {:data-at at} (ago now at)]]])

(def ^:private reporting-words
  [[:full "Full" "Agents and their subagents report each file they start on and what they intend."]
   [:agents "Agents" "Agents report each file and what they intend; subagents stay quiet."]
   [:brief "Brief" "Agents report once per task and subagents stay quiet. Agents can no longer leave pins or ask who else is at work."]
   [:off "Off" "No agent reports. The cloud still shows what agents read through the hub and the changes they make."]])

(defn- reporting-section [dispatch current]
  [:section.coworkers-section
   [:h3 "Reporting"]
   [:div.reporting-levels {:role "radiogroup" :aria-label "How much agents report"}
    (for [[level label] reporting-words]
      [:button.reporting-level {:type "button"
                                :role "radio"
                                :class (when (= level current) "is-selected")
                                :aria-checked (str (= level current))
                                :data-focus (str "reporting:" (name level))
                                :on-click #(dispatch [:agents-reporting level])}
       label])]
   [:p.coworkers-note (str (some (fn [[level _label description]] (when (= level current) description)) reporting-words)
                           " Each report costs the agent tokens.")]])

(defn- wavescope-section [dispatch bands]
  (let [every-band? (= :all bands)]
    [:section.coworkers-section
     [:h3 "wavescope"]
     [:label.visibility-row
      [:input.wavescope-bands-toggle (cond-> {:type "checkbox"
                                              :data-focus "wavescope-bands"
                                              :on-change (fn [^js event]
                                                           (dispatch [:agents-wavescope-bands
                                                                      (if (.. event -target -checked) :all :fine)]))}
                                       every-band? (assoc :checked true))]
      [:span.visibility-name "Add the medium and coarse bands"]]
     [:p.coworkers-note (if every-band?
                          (str "Reading around a line, agents get the exact lines nearest it, the signatures and sections "
                               "further out, and the strongest peaks, which help them find their way in a large codebase.")
                          (str "Reading around a line, agents get the exact lines nearest it alone, which costs them fewer "
                               "tokens. The medium and coarse bands add the signatures and sections further out, which "
                               "help in a large codebase."))]]))

(defn- pane-content [dispatch {:keys [agents presence]} theme-key now]
  (let [{agent-list :agents :keys [pins]} presence
        agents-by-id (into {} (map (juxt :id identity)) agent-list)
        most-activity 12
        recent (presence-state/activity presence most-activity)]
    (list
     [:header.panel-header
      [:h2 "Coworkers"]
      (dock/minimize-button dispatch :coworkers)
      [:button.icon-button {:type "button" :aria-label "Close" :on-click #(dispatch [:toggle-coworkers])} icons/close]]
     (cond
       (not (:serving? agents))
       [:p.coworkers-empty (if (:problem agents)
                             (str "The agent hub did not start: " (:problem agents))
                             "The agent hub is off, so no agent can connect.")]

       (empty? agent-list)
       [:p.coworkers-empty "No agent works here yet. Once one connects, its badge shows on the page it works on."]

       :else
       [:ul.coworkers-list (map #(agent-row dispatch presence theme-key %) (in-family-order agent-list))])
     (when (seq pins)
       [:section.coworkers-section
        [:h3 "Pins"]
        [:ul.coworkers-list (map #(pin-row dispatch theme-key agents-by-id %) pins)]])
     (when (seq recent)
       [:section.coworkers-section
        [:h3 "Lately"]
        [:ul.activity-list (map #(activity-row dispatch theme-key now %) recent)]])
     (when (seq agent-list)
       [:p.coworkers-note "CodeCloud credits a change to the agent that last read the file, so a change's agent is a guess."])
     (reporting-section dispatch (or (:reporting agents) :full))
     (wavescope-section dispatch (or (:wavescope-bands agents) :fine))
     [:footer.coworkers-footer
      [:button.action-button {:type "button" :data-focus "connect" :on-click #(dispatch [:connect-agent])}
       icons/agents "Connect an agent"]])))

(defn- focused-key [^js root]
  (let [^js active (.-activeElement js/document)]
    (when (and active (.contains root active))
      (.getAttribute active "data-focus"))))

(defn- refresh-times! [^js root]
  (let [now (js/Date.now)]
    (doseq [^js time-element (array-seq (.querySelectorAll root "[data-at]"))]
      (dom/set-text! time-element (ago now (js/parseInt (.getAttribute time-element "data-at") 10))))))

(def panel
  {:id :coworkers
   :mount (fn [dispatch]
            (let [root (dom/element [:aside.hud-coworkers {:aria-label "Coworkers"}])
                  ;; Times count in minutes, so twice a minute keeps them right.
                  refresh-milliseconds 30000]
              (js/setInterval #(when (.contains (.-classList root) "is-open") (refresh-times! root)) refresh-milliseconds)
              {:root root :dispatch dispatch :built (atom nil)}))
   :update (fn [{:keys [^js root dispatch built]} {:keys [agents presence] :as state}]
             (let [open? (:pane-open? presence)
                   ;; The overlay takes its colors from the sky drawn now.
                   theme-key (if (= "day" (.getAttribute (.-documentElement js/document) "data-sky")) :day :night)
                   build-key [agents (dissoc presence :pane-open? :noticed-through) theme-key]]
               (dom/toggle-class! root "is-open" open?)
               (when (and open? (not= build-key @built))
                 (reset! built build-key)
                 (let [scroll (.-scrollTop root)
                       focus-key (focused-key root)]
                   (dom/replace-children! root (pane-content dispatch state theme-key (js/Date.now)))
                   (set! (.-scrollTop root) scroll)
                   (when focus-key
                     (some-> (some #(when (= focus-key (.getAttribute ^js % "data-focus")) %)
                                   (array-seq (.querySelectorAll root "[data-focus]")))
                             .focus))))))})

(def following-panel
  {:id :following
   :mount (fn [dispatch]
            {:root (dom/element [:div.hud-following {:role "status"}
                                 [:span.following-words]
                                 [:button.action-button {:type "button" :on-click #(dispatch [:stop-following])} "Stop"]])})
   :update (fn [{:keys [^js root]} {:keys [presence]}]
             (let [followed (presence-state/agent-named presence (:following presence))]
               (dom/toggle-class! root "is-open" (some? followed))
               (when followed
                 (dom/set-text! (dom/find-in root ".following-words")
                                (str "Following " (:name followed) (when (:doing followed) (str ", " (:doing followed))))))))})

(ns codecloud.render.hud.timeline
  "The history along the bottom: a tick per commit, stepping, playback,
  and a way back to the working tree."
  (:require [codecloud.render.dom :as dom]
            [codecloud.render.hud.words :as words]
            [codecloud.render.icons :as icons]))

(defn- commit-tooltip-text [{:keys [subject author time]}]
  (str subject "\n" author ", " (words/relative-time time)))

(defn- commit-ticks
  "A tick for each commit, oldest at the left."
  [dispatch commits]
  (let [commit-count (count commits)]
    (map-indexed (fn [index {:keys [sha] :as commit}]
                   [:button.timeline-tick {:data-sha sha
                                           :title (commit-tooltip-text commit)
                                           :aria-label (str "Commit " (:subject commit))
                                           :style {:left (str (* 100 (/ (inc index) (inc commit-count))) "%")}
                                           :on-click #(dispatch [:select-snapshot sha])}])
                 (reverse commits))))

(defn- show-playing! [^js button playing?]
  (dom/replace-children! button (if playing? icons/pause icons/play))
  (.setAttribute button "aria-label" (if playing? "Pause history" "Play history"))
  (.setAttribute button "title" (if playing? "Pause (Space)" "Play history (Space)")))

(def panel
  {:id :timeline
   :mount (fn [dispatch]
            {:root (dom/element
                    [:footer.hud-timeline {:aria-label "History"}
                     [:button.icon-button.play-button {:on-click #(dispatch [:toggle-playback])}]
                     [:button.icon-button {:aria-label "Older commit" :title "Older commit (Left arrow)"
                                           :on-click #(dispatch [:step-history -1])}
                      icons/step-back]
                     [:div.timeline-track
                      [:div.timeline-line]
                      [:div.timeline-ticks]
                      [:button.timeline-now {:on-click #(dispatch [:select-snapshot :working-tree])
                                             :title "The files as they are now"} "Now"]]
                     [:button.icon-button {:aria-label "Newer commit" :title "Newer commit (Right arrow)"
                                           :on-click #(dispatch [:step-history 1])}
                      icons/step-forward]
                     [:p.timeline-caption {:aria-live "polite"} ""]])
             :dispatch dispatch
             :shown (atom nil)})
   :update (fn [{:keys [root dispatch shown]} {:keys [commits history-status requested-snapshot-id playing?] :as state}]
             (dom/toggle-class! root "is-hidden" (not= :ready history-status))
             (let [{shown-commits :commits shown-playing :playing? shown-requested :requested} @shown]
               (when (not= commits shown-commits)
                 (dom/replace-children! (dom/find-in root ".timeline-ticks") (commit-ticks dispatch commits)))
               (when (not= playing? shown-playing)
                 (show-playing! (dom/find-in root ".play-button") playing?))
               (when (or (not= commits shown-commits) (not= requested-snapshot-id shown-requested))
                 (doseq [^js tick (array-seq (.querySelectorAll root ".timeline-tick"))]
                   (dom/toggle-class! tick "is-current" (= requested-snapshot-id (.getAttribute tick "data-sha"))))
                 (dom/toggle-class! (dom/find-in root ".timeline-now") "is-current" (= :working-tree requested-snapshot-id))
                 (dom/set-text! (dom/find-in root ".timeline-caption")
                                (words/snapshot-caption (assoc state :snapshot {:id requested-snapshot-id})))))
             (reset! shown {:commits commits :playing? playing? :requested requested-snapshot-id}))})

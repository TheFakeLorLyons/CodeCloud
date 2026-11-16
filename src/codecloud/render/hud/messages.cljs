(ns codecloud.render.hud.messages
  "Reading progress, a chord waiting for its second key, dismissable
  notices, and the empty cloud's message."
  (:require [codecloud.render.dom :as dom]
            [codecloud.render.icons :as icons]))

(defn- chord-status [first-key]
  {:message (str first-key " pressed. Enter loads/evaluates current file, Shift+Enter loads/evaluates it and its "
                 "requires/dependencies, and Ctrl+Alt+D interrupts the evaluations.")})

(def status-panel
  {:id :status
   :mount (fn [_dispatch]
            {:root (dom/element [:div.hud-status {:role "status"}
                                 [:span.status-message ""]
                                 [:span.status-bar [:span.status-fill]]])})
   :update (fn [{:keys [root]} {:keys [status chord]}]
             (let [shown (or status (some-> chord chord-status))]
               (dom/toggle-class! root "is-open" (some? shown))
               (dom/set-text! (dom/find-in root ".status-message") (or (:message shown) ""))
               (dom/toggle-class! (dom/find-in root ".status-bar") "is-hidden" (nil? status))
               (let [^js fill (dom/find-in root ".status-fill")]
                 (set! (.. fill -style -width) (str (* 100 (or (:progress shown) 0)) "%")))))})

(def notices-panel
  {:id :notices
   :mount (fn [dispatch]
            {:root (dom/element [:div.hud-notices {:role "status"}])
             :dispatch dispatch
             :shown (atom nil)})
   :update (fn [{:keys [root dispatch shown]} {:keys [notices]}]
             (when (not= notices @shown)
               (reset! shown notices)
               (dom/replace-children!
                root
                (map-indexed (fn [index notice]
                               [:p.notice [:span notice]
                                [:button.icon-button {:aria-label "Dismiss" :on-click #(dispatch [:dismiss-notice index])}
                                 icons/close]])
                             notices))))})

(def empty-panel
  {:id :empty
   :mount (fn [dispatch]
            {:root (dom/element
                    [:div.hud-empty
                     [:h2.empty-heading ""]
                     [:p.empty-text ""]
                     [:button.action-button.empty-action {:on-click #(dispatch [:clear-filter])} "Show everything"]])})
   :update (fn [{:keys [root]} {:keys [snapshot visible-ids]}]
             (let [nothing-here? (and snapshot (empty? (:namespaces snapshot)))
                   filtered-away? (and snapshot (not nothing-here?) (empty? visible-ids))]
               (dom/toggle-class! root "is-open" (or nothing-here? filtered-away?))
               (dom/toggle-class! (dom/find-in root ".empty-action") "is-hidden" (not filtered-away?))
               (dom/set-text! (dom/find-in root ".empty-heading")
                              (if filtered-away? "Every page is hidden" "No code here yet"))
               (dom/set-text! (dom/find-in root ".empty-text")
                              (if filtered-away?
                                ""
                                "Add code here, or loosen :include, :ignore, or :languages in the cloud file."))))})

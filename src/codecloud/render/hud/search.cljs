(ns codecloud.render.hud.search
  "Finding a page by name. Matches come forward in the cloud, and the
  most important are listed for the arrow keys."
  (:require [clojure.string :as string]
            [codecloud.presentation.scene :as scene]
            [codecloud.presentation.ui-state :as ui-state]
            [codecloud.presentation.wording :as wording]
            [codecloud.render.dom :as dom]
            [codecloud.render.hud.words :as words]))

(defn- result-option [dispatch snapshot selected-index index namespace-id]
  (let [node (get-in snapshot [:namespaces namespace-id])
        selected? (= index selected-index)]
    [:li {:role "option" :aria-selected (str selected?)}
     [:button.search-option {:class (when selected? "is-selected")
                             :tabindex "-1"
                             ;; Keeps the field focused, so the arrow keys still choose.
                             :on-mousedown (fn [^js event] (.preventDefault event))
                             :on-click #(dispatch [:search-pick namespace-id])}
      [:span.search-option-name (words/name-heading (scene/label-parts node))]
      [:span.search-option-detail (words/counted (get-in node [:metrics :lines]) "line" "lines")]]]))

(defn- result-summary [query match-count listed-count]
  (cond
    (string/blank? query) ""
    (zero? match-count) "No matches"
    :else (str (words/counted match-count "match" "matches")
               (when (> match-count listed-count) (str ", top " listed-count " shown")))))

(defn- on-key-down [dispatch ^js event]
  (case (.-key event)
    "Enter" (dispatch [:search-submit])
    "ArrowDown" (do (.preventDefault event) (dispatch [:search-move 1]))
    "ArrowUp" (do (.preventDefault event) (dispatch [:search-move -1]))
    "Escape" (do (.preventDefault event) (dispatch [:close-search]))
    nil)
  (.stopPropagation event))

(def panel
  {:id :search
   :mount (fn [dispatch]
            (let [root (dom/element
                        [:div.hud-search {:role "search"}
                         [:input.search-input {:type "search" :placeholder "Find a namespace" :aria-label "Find a namespace"
                                               :spellcheck "false" :autocomplete "off" :role "combobox"
                                               :aria-controls "codecloud-search-results" :aria-expanded "false"}]
                         [:ul#codecloud-search-results.search-results {:role "listbox" :aria-label "Matching namespaces"}]
                         [:p.search-summary {:aria-live "polite"} ""]])
                  ^js input (dom/find-in root ".search-input")]
              (.addEventListener input "input" #(dispatch [:search-query (.-value input)]))
              (.addEventListener input "keydown" #(on-key-down dispatch %))
              {:root root :input input :dispatch dispatch :was-open? (atom false) :shown (atom nil)}))
   :update (fn [{:keys [root ^js input dispatch was-open? shown]} {:keys [search] snapshot :visible-snapshot :as state}]
             (let [{:keys [open? query selected]} search]
               (dom/toggle-class! root "is-open" open?)
               (when (and open? (not @was-open?))
                 (.focus input))
               (when (and (not open?) @was-open?)
                 (set! (.-value input) "")
                 (.blur input))
               (reset! was-open? open?)
               (let [shown-key [query selected snapshot]]
                 (when (and open? (not= shown-key @shown))
                   (reset! shown shown-key)
                   (let [matches (scene/search-results snapshot query (ui-state/visible-ids state))
                         listed (take ui-state/search-result-limit matches)
                         [singular plural] (wording/unit-words snapshot)]
                     (.setAttribute input "placeholder" (str "Find a " singular))
                     (.setAttribute input "aria-label" (str "Find a " singular))
                     (.setAttribute (dom/find-in root ".search-results") "aria-label" (str "Matching " plural))
                     (dom/replace-children! (dom/find-in root ".search-results")
                                            (map-indexed (fn [index namespace-id]
                                                           (result-option dispatch snapshot selected index namespace-id))
                                                         listed))
                     (.setAttribute input "aria-expanded" (str (boolean (seq listed))))
                     (dom/set-text! (dom/find-in root ".search-summary")
                                    (result-summary query (count matches) (count listed))))))))})

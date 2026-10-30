(ns codecloud.render.hud.inspector
  "Details of the lifted page or folder: measurements, colors,
  dependencies, cycles, parse errors, notable definitions, and its files."
  (:require [clojure.string :as string]
            [codecloud.model.selection :as selection]
            [codecloud.model.source :as source]
            [codecloud.presentation.arrangement :as arrangement]
            [codecloud.presentation.layers :as layers]
            [codecloud.presentation.page :as page]
            [codecloud.presentation.scene :as scene]
            [codecloud.presentation.ui-state :as ui-state]
            [codecloud.presentation.wording :as wording]
            [codecloud.render.dom :as dom]
            [codecloud.render.hud.swatches :as swatches]
            [codecloud.render.hud.words :as words]
            [codecloud.render.icons :as icons]))

(defn- namespace-chip
  "A button that brings a page closer, showing it again when hidden."
  [dispatch {:keys [snapshot visible-ids]} namespace-id]
  (let [node (get-in snapshot [:namespaces namespace-id])
        hidden? (not (contains? visible-ids namespace-id))]
    [:li [:button.chip {:class (cond hidden? "is-hidden-page" (scene/cyclic? node) "is-cyclic" (scene/looped? node) "is-looped")
                        :title (when hidden? "Hidden. Click to show it")
                        :on-click #(dispatch [:visit-namespace namespace-id])}
          (words/name-heading (scene/label-parts node))]]))

(defn- chip-section
  "Chips for the shown pages `namespace-ids`, with a count of any hidden
  ones."
  [dispatch state heading namespace-ids hidden-count]
  [:section.inspector-section
   [:h3 heading]
   (when (seq namespace-ids)
     [:ul.chip-list (map #(namespace-chip dispatch state %) namespace-ids)])
   (cond
     (pos? (or hidden-count 0)) [:p.quiet (str (if (seq namespace-ids) "+" "") hidden-count " hidden")]
     (empty? namespace-ids) [:p.quiet "None"]
     :else nil)])

(defn- close-button [dispatch]
  [:button.icon-button {:aria-label "Set it back" :title "Set it back (Esc)" :on-click #(dispatch [:click-empty])}
   icons/close])

(defn- complection-reasons
  "Why a definition scores: measurements past their thresholds, and
  flags."
  [{:keys [lines branches nesting asynchronous state flags]}]
  (string/join ", "
               (concat (remove nil?
                               [(when lines (words/counted lines "line" "lines"))
                                (when branches (words/counted branches "branch" "branches"))
                                (when nesting (str "nested " nesting " deep"))
                                (when asynchronous (words/counted asynchronous "asynchronous call" "asynchronous calls"))
                                (when state (words/counted state "change to state" "changes to state"))])
                       (for [[flag-name occurrences] (sort-by (comp name key) flags)]
                         (words/counted occurrences (str (name flag-name) " flag") (str (name flag-name) " flags"))))))

(defn- definition-button [dispatch namespace-id {definition-name :name :keys [path line]} detail]
  [:button.definition {:on-click #(dispatch [:show-definition {:namespace-id namespace-id :path path :line line}])}
   [:span.definition-name definition-name]
   [:span.definition-count detail]])

(defn- complection-section
  "The most complected definitions with scores and reasons. A click flies
  to one."
  [dispatch namespace-id {:keys [hotspots metrics]}]
  (let [listed 6]
    (when (seq hotspots)
      [:section.inspector-section
       [:h3 (str "Most complected, " (words/points (:complection metrics)) " in all")]
       [:ul.definition-list
        (for [{:keys [score excesses] :as hotspot} (take listed hotspots)]
          [:li
           (definition-button dispatch namespace-id hotspot (words/points score))
           [:span.definition-reasons (complection-reasons excesses)]])]])))

(defn- most-used-section [dispatch namespace-id definitions]
  (let [listed 8
        most-used (take listed (sort-by (juxt (comp - :inbound-count) :name) definitions))]
    (when (seq most-used)
      [:section.inspector-section
       [:h3 "Most used"]
       [:ul.definition-list
        (for [{:keys [inbound-count] :as definition} most-used]
          [:li (definition-button dispatch namespace-id definition
                                  (if (pos? inbound-count) (words/counted inbound-count "use" "uses") ""))])]])))

(defn- ranking-line
  "A page's place on a ranked wall, such as \"#3 of 61 in Longest files,
  with 812 lines\"."
  [{:keys [rank total label value unit]}]
  (let [[singular plural] unit]
    [:p.inspector-rank (str "#" rank " of " total " in " label ", with " (words/counted value singular plural))]))

(defn- cycle-section [dispatch state found-cycle]
  (let [first-member (get-in state [:snapshot :namespaces (first (:path found-cycle))])]
    [:section.inspector-section {:class (if (= :warning (:severity found-cycle)) "inspector-loop" "inspector-cycle")}
     [:h3 (wording/cycle-heading (:severity found-cycle))]
     [:p (wording/cycle-explanation found-cycle)]
     [:ol.cycle-path {:style {"--link-word" (pr-str (string/lower-case (:uses (wording/node-words first-member))))}}
      (map #(namespace-chip dispatch state %) (:path found-cycle))]]))

(defn- parse-error-section [{:keys [message line]}]
  [:section.inspector-section.inspector-parse-error
   [:h3 "⚠ Parse error"]
   [:p (str message (when line (str ", line " line)))]])

(defn- open-file-buttons [dispatch files historical?]
  [:footer.inspector-actions
   (for [{:keys [path]} files]
     [:button.action-button {:on-click #(dispatch [:dive {:path path :line 1}])
                             :title (if historical? (str "Open " path " as it is now") (str "Open " path))}
      icons/open-file [:span (str "Open " (source/file-name path))]])])

(defn- outside-line
  "What a page reaches outside the cloud, such as libraries, or nil."
  [node]
  (let [outside (:external-requires node)
        listed 6]
    (when (seq outside)
      [:p.quiet.inspector-outside
       (str "Outside this cloud: " (string/join ", " (take listed outside))
            (when (> (count outside) listed) (str ", and " (- (count outside) listed) " more")))])))

(defn- color-section
  "What each layer turned on gives the page, with the blend by the
  heading. A click on a layer's dot chooses that color."
  [dispatch {:keys [layer-registry layer-contexts active-layers page-colors]} namespace-id node]
  (let [rows (layers/breakdown layer-registry layer-contexts active-layers node)]
    (when (seq rows)
      [:section.inspector-section
       [:h3.color-heading (swatches/dot (get page-colors namespace-id)) "Color"]
       [:dl.layer-breakdown
        (for [{:keys [label color description target]} rows]
          [:div
           [:dt (if (and target color)
                  (swatches/choosable dispatch color target (or description label))
                  (swatches/dot color))
            [:span label]]
           [:dd (or description "none")]])]])))

(defn- namespace-inspector [dispatch state namespace-id historical? ranking]
  (let [snapshot (:visible-snapshot state)
        node (get-in snapshot [:namespaces namespace-id])
        {:keys [metrics files definitions cycle-id]} node
        found-cycle (some #(when (= cycle-id (:id %)) %) (:cycles snapshot))
        parse-error (some :parse-error files)
        {:keys [uses used-by]} (wording/node-words node)]
    (list
     [:header.inspector-header
      [:h2.inspector-name (words/name-heading (scene/label-parts node))]
      (close-button dispatch)]
     (when-let [kind-line (wording/kind-description node)] [:p.inspector-kind kind-line])
     (when ranking (ranking-line ranking))
     (when-let [paragraph (words/first-paragraph (:doc node))] [:p.inspector-doc paragraph])
     [:dl.inspector-metrics
      [:div [:dt "Functions"] [:dd (words/formatted-count (:functions metrics))]]
      [:div [:dt "Used elsewhere"] [:dd (words/formatted-count (:references metrics))]]
      [:div [:dt "Lines"] [:dd (words/formatted-count (:lines metrics))]]
      [:div [:dt used-by] [:dd (words/formatted-count (:dependents metrics))]]]
     (color-section dispatch state namespace-id node)
     (when found-cycle (cycle-section dispatch state found-cycle))
     (when parse-error (parse-error-section parse-error))
     (chip-section dispatch state uses (remove #(= % namespace-id) (:requires node)) (:hidden-requires node))
     (chip-section dispatch state used-by (:required-by node) (:hidden-required-by node))
     (outside-line node)
     (complection-section dispatch namespace-id node)
     (most-used-section dispatch namespace-id definitions)
     (open-file-buttons dispatch files historical?))))

(defn- folder-namespace-ids
  "Ids of the namespaces whose files live in `directory`."
  [snapshot directory]
  (sort (keep (fn [[id node]] (when (= directory (page/directory node)) id))
              (:namespaces snapshot))))

(defn- folder-color-section
  "The folder's color while the folder layer is on, chosen with a click on
  its dot."
  [dispatch {:keys [active-layers layer-registry layer-contexts]} directory label]
  (when (contains? active-layers :folder)
    (when-let [colors (layers/target-color layer-registry layer-contexts [:folder directory])]
      [:section.inspector-section
       [:h3.color-heading (swatches/choosable dispatch colors [:folder directory] label) "Color"]])))

(defn- folder-inspector [dispatch {:keys [cloud-name] snapshot :visible-snapshot :as state} directory]
  (let [member-ids (folder-namespace-ids snapshot directory)
        nodes (map #(get-in snapshot [:namespaces %]) member-ids)
        line-total (reduce + 0 (map #(get-in % [:metrics :lines] 0) nodes))
        cyclic-count (count (filter :cycle-id nodes))
        hidden-count (- (count (folder-namespace-ids (:snapshot state) directory)) (count member-ids))
        [_ plural] (wording/unit-words {:namespaces (zipmap member-ids nodes)})
        label (scene/directory-label directory cloud-name)]
    (list
     [:header.inspector-header
      [:h2.inspector-name (words/name-heading label)]
      (close-button dispatch)]
     [:p.inspector-kind "Folder"]
     [:dl.inspector-metrics
      [:div [:dt (words/capitalized plural)] [:dd (words/formatted-count (count member-ids))]]
      [:div [:dt "Lines"] [:dd (words/formatted-count line-total)]]
      (when (pos? cyclic-count)
        [:div [:dt "In a cycle"] [:dd (words/formatted-count cyclic-count)]])]
     (folder-color-section dispatch state directory (str (:prefix label) (:leaf label)))
     (chip-section dispatch state (str (words/capitalized plural) " here")
                   (sort-by #(- (get-in snapshot [:namespaces % :importance] 0)) member-ids)
                   hidden-count)
     [:footer.inspector-actions
      [:button.action-button {:on-click #(dispatch [:show-only [(selection/directory-pattern directory)]])}
       icons/eye [:span "Show only this folder"]]])))

(defn- ranking-of
  "A page's place among those shown on a ranked wall, or nil."
  [state namespace-id]
  (let [{:keys [metric label unit]} (arrangement/arrangement (:arrangement state))]
    (when metric
      (let [snapshot (:visible-snapshot state)
            ranked (arrangement/ranked-ids snapshot (ui-state/visible-ids state) metric)
            place (first (keep-indexed (fn [index id] (when (= id namespace-id) index)) ranked))]
        (when place
          {:rank (inc place)
           :total (count ranked)
           :label label
           :unit unit
           :value (arrangement/metric-value (get-in snapshot [:namespaces namespace-id]) metric)})))))

(def panel
  {:id :inspector
   :mount (fn [dispatch]
            {:root (dom/element [:aside.hud-inspector {:aria-label "Details"}])
             :dispatch dispatch
             :shown (atom nil)})
   :update (fn [{:keys [root dispatch shown]} {:keys [focused-id lifted-folder visible-snapshot arrangement
                                                      page-colors layer-contexts active-layers] :as state}]
             (let [shown-key [focused-id lifted-folder visible-snapshot arrangement page-colors layer-contexts active-layers]]
               (when (not= shown-key @shown)
                 (reset! shown shown-key)
                 (cond
                   (and focused-id (get-in visible-snapshot [:namespaces focused-id]))
                   (do (dom/replace-children! root (namespace-inspector dispatch state focused-id
                                                                        (not= :working-tree (:id visible-snapshot))
                                                                        (ranking-of state focused-id)))
                       (dom/toggle-class! root "is-open" true))

                   (and lifted-folder visible-snapshot)
                   (do (dom/replace-children! root (folder-inspector dispatch state lifted-folder))
                       (dom/toggle-class! root "is-open" true))

                   :else (dom/toggle-class! root "is-open" false)))))})

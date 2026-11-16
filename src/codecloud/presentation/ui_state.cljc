(ns codecloud.presentation.ui-state
  "The cloud view's state and its changes. `handle` routes an event to its
  handler, which maps the state and the event's arguments to {:state
  :effects}, effects being data for the renderer."
  (:require [clojure.string :as string]
            [codecloud.languages.catalog :as catalog]
            [codecloud.model.restriction :as restriction]
            [codecloud.model.source :as source]
            [codecloud.presentation.arrangement :as arrangement]
            [codecloud.presentation.assistant-state :as assistant-state]
            [codecloud.presentation.code-colors :as code-colors]
            [codecloud.presentation.definitions :as definitions]
            [codecloud.presentation.editing-state :as editing-state]
            [codecloud.presentation.environments :as environments]
            [codecloud.presentation.handling :refer [changed send-effect unchanged with-notice]]
            [codecloud.presentation.layers :as layers]
            [codecloud.presentation.page :as page]
            [codecloud.presentation.pairing :as pairing]
            [codecloud.presentation.presence-state :as presence-state]
            [codecloud.presentation.repl-state :as repl-state]
            [codecloud.presentation.scene :as scene]
            [codecloud.presentation.visibility :as visibility]))

(def dock-defaults
  "The dock's layout before the viewer changes it, its width in pixels,
  each pane's share of its height, and the panes minimized to their
  headers."
  {:width 360 :weights {} :minimized #{}})

(def initial-state
  {:snapshot nil
   :visible-ids #{}
   :visible-snapshot nil
   :working-snapshot nil
   :requested-snapshot-id :working-tree
   :commits []
   :history-status :loading
   :playing? false
   :focused-id nil
   :lifted-folder nil
   :hovered-id nil
   :sky-mode environments/default-id
   :spaces []
   :theme-kind :dark
   :display-mode :code
   :arrangement :folders
   :header-mode :beside
   :color-layers nil
   :custom-layers []
   :layer-registry layers/built-in
   :layer-contexts {}
   :active-layers #{}
   :default-layers #{}
   :page-colors {}
   :folder-hues {}
   :tint-clouds? true
   :color-overrides {}
   :color-chooser nil
   :spread 1.0
   :moved-pages {}
   :arrange-open? false
   :totals-open? false
   :inspector-minimized? false
   :dock dock-defaults
   :named-views {}
   :view-filter visibility/no-filter
   :views-open? false
   :open-folders #{""}
   :menu nil
   :search {:open? false :query "" :selected 0}
   :legend-open? false
   :status nil
   :notices []
   :sources {}
   :source-order []
   :requested-sources #{}
   :arrival nil
   :editing nil
   :chord nil
   :buffers {}
   :dirty-paths #{}
   :settings {}
   :editor-font nil
   :editor-colors nil
   :editor-colors-read? false
   :cloud-name nil
   :repl repl-state/initial
   :inline-results {}
   :agents {:serving? false :url nil :agents [] :problem nil}
   :presence presence-state/initial
   :assistant assistant-state/initial})

(defn- remember [preference value]
  (send-effect :view/save-preference {:key preference :value value}))

;; Handler shapes. A handler takes the state and an event's arguments
;; and returns {:state :effects}.

(defn- quietly [update-state]
  (fn [state & arguments]
    (unchanged (apply update-state state arguments))))

(defn- remembered
  "A handler setting the state at `path` to what `next-value` makes of the
  state and the event's arguments, and remembering it as `preference`."
  [preference path next-value]
  (fn [state & arguments]
    (let [value (apply next-value state arguments)]
      (changed (assoc-in state path value) (remember preference value)))))

(defn- remembered-toggle [preference path]
  (remembered preference path #(not (get-in % path))))

(def ^:private side-pane-paths
  "Where the state says whether each pane of the dock on the right side
  of the view shows."
  {:views [:views-open?]
   :arrange [:arrange-open?]
   :coworkers [:presence :pane-open?]
   :assistant [:assistant :open?]})

(defn- with-side-pane
  "The state showing side pane `pane` when `open?`, beside any other pane
  open in the dock, and expanded, with any menu closed."
  [state pane open?]
  (cond-> (-> state (assoc :menu nil) (assoc-in (side-pane-paths pane) open?))
    open? (update-in [:dock :minimized] disj pane)))

(defn- side-pane-toggle [pane]
  (fn [state]
    (let [shown (with-side-pane state pane (not (get-in state (side-pane-paths pane))))]
      (changed shown (remember :dock (:dock shown))))))

(defn- dock-layout
  "`layout` read back as the dock's {:width :weights :minimized}, a part
  that does not read as such taking its default."
  [layout]
  (let [{:keys [width weights minimized]} (when (map? layout) layout)
        narrowest 260
        widest 900]
    {:width (if (number? width) (max narrowest (min widest width)) (:width dock-defaults))
     :weights (into {} (filter (fn [[pane weight]] (and (keyword? pane) (number? weight) (pos? weight)))) (when (map? weights) weights))
     :minimized (into #{} (filter keyword?) (when (coll? minimized) minimized))}))

(defn- with-dock
  "A handler changing the dock's layout with `change`, of the layout and
  the event's arguments, and remembering it."
  [change]
  (fn [state & arguments]
    (let [layout (dock-layout (apply change (:dock state) arguments))]
      (changed (assoc state :dock layout) (remember :dock layout)))))

(defn awaiting-snapshot?
  "True while the view waits for the snapshot it asked for."
  [state]
  (not= (:requested-snapshot-id state) (get-in state [:snapshot :id])))

(defn visible-ids
  "Ids of the pages the filter lets the view show."
  [state]
  (:visible-ids state))

(defn source-key
  "Key of a page's source text, its id and its files' content keys, which
  stay the same across commits that leave the files alone."
  [snapshot namespace-id]
  [namespace-id (mapv :content-key (get-in snapshot [:namespaces namespace-id :files]))])

(defn source-of
  "Files of a namespace in the shown snapshot, when their text is here."
  [state namespace-id]
  (get (:sources state) (source-key (:snapshot state) namespace-id)))

(defn- namespace-for-path
  "Id of the page holding the file at `path`, ignoring letter case when
  nothing matches exactly, as on Windows."
  [snapshot path]
  (let [holds? (fn [matches?] (some (fn [[id node]] (when (some #(matches? (:path %)) (:files node)) id))
                                    (:namespaces snapshot)))]
    (or (holds? #(= path %))
        (holds? #(= (string/lower-case path) (string/lower-case %))))))

(defn- request-sources
  "[state effect] asking the host for the sources of `namespace-ids` not
  here and not asked for yet, with a nil effect when none are."
  [state namespace-ids]
  (let [snapshot (:snapshot state)
        batch-limit 16
        wanted (into []
                     (comp (filter #(contains? (:namespaces snapshot) %))
                           (map (fn [namespace-id] {:namespace-id namespace-id
                                                    :source-key (source-key snapshot namespace-id)}))
                           (remove #(contains? (:sources state) (:source-key %)))
                           (remove #(contains? (:requested-sources state) (:source-key %)))
                           (take batch-limit))
                     (distinct namespace-ids))]
    (if (and snapshot (seq wanted))
      [(update state :requested-sources into (map :source-key wanted))
       (send-effect :view/request-sources {:snapshot-id (:id snapshot) :namespaces wanted})]
      [state nil])))

(defn- folder-shown? [state shown-ids directory]
  (some #(= directory (page/directory (get-in state [:snapshot :namespaces %]))) shown-ids))

(defn- settle-lifts
  "[state effects] setting down a lifted page or folder no longer shown."
  [state]
  (let [shown-ids (visible-ids state)
        {:keys [focused-id lifted-folder]} state
        page-gone? (and focused-id (not (contains? shown-ids focused-id)))
        folder-gone? (and lifted-folder (not (folder-shown? state shown-ids lifted-folder)))]
    [(cond-> state
       page-gone? (assoc :focused-id nil)
       folder-gone? (assoc :lifted-folder nil))
     (cond-> []
       page-gone? (conj [:set-down focused-id])
       folder-gone? (conj [:set-down-folder lifted-folder]))]))

(declare with-visibility settle-editing settle-buffers stop-editing)

(defn- save-filter
  "The state with a new view filter remembered, setting down what it hides
  and framing the cloud again when `refit?`."
  ([state view-filter] (save-filter state view-filter true))
  ([state view-filter refit?]
   (let [[settled lift-effects] (settle-lifts (with-visibility (assoc state :view-filter view-filter :menu nil)))]
     (apply changed settled
            (remember :view-filter view-filter)
            (cond-> lift-effects refit? (conj [:refit]))))))

(defn- set-down-everything [state]
  (let [{:keys [focused-id lifted-folder]} state]
    [(assoc state :focused-id nil :lifted-folder nil)
     (cond-> []
       focused-id (conj [:set-down focused-id])
       lifted-folder (conj [:set-down-folder lifted-folder]))]))

(defn- revealed
  "[state effect] with `namespace-id` shown, the filter changed as little
  as it can and remembered, or the state as it was when the page shows."
  [state namespace-id]
  (if (contains? (visible-ids state) namespace-id)
    [state nil]
    (let [view-filter (visibility/reveal (:view-filter state) (:snapshot state) (:named-views state) [namespace-id])]
      [(with-visibility (assoc state :view-filter view-filter))
       (remember :view-filter view-filter)])))

(defn- arrival-effects
  "Camera and source effects for a pending arrival from the editor, once
  its page is in the shown snapshot. A hidden page shows again."
  [state]
  (let [{:keys [path] :as arrival} (:arrival state)
        namespace-id (when (and arrival (:snapshot state))
                       (namespace-for-path (:snapshot state) path))]
    (if namespace-id
      (let [[shown-state filter-effect] (revealed state namespace-id)
            [state source-effect] (request-sources (assoc shown-state :arrival nil) [namespace-id])]
        (changed state filter-effect source-effect [:arrive (assoc arrival :namespace-id namespace-id)]))
      (changed (assoc state :arrival nil) [:snapshot-shown (get-in state [:snapshot :id])]))))

;; Color layers

(defn enabled-layers
  "Keys of the color layers turned on, as the viewer chose them, or the
  snapshot's defaults while :color-layers is nil."
  [{:keys [color-layers snapshot layer-registry]}]
  (let [offered (set (map :key layer-registry))]
    (into #{} (filter offered) (or color-layers (layers/default-keys snapshot)))))

(defn colored-by-default? [{:keys [active-layers default-layers]}]
  (= active-layers default-layers))

(defn shown-code-colors
  "The code colors pages show, the editor theme's while the switch is on
  and the host could read them, and CodeCloud's own otherwise."
  [{:keys [settings editor-colors]}]
  (if (and (:editor-colors? settings) editor-colors)
    editor-colors
    code-colors/own))

(defn shown-environment [{:keys [sky-mode spaces]}]
  (environments/environment-of sky-mode spaces))

(defn- recolored
  "The state with every page's color blended again, preparing each layer
  for the shown pages first when `prepare?`. The default layers follow the
  whole snapshot, so hiding pages never switches layers on or off."
  [{:keys [snapshot visible-snapshot layer-registry color-overrides] :as state} prepare?]
  (let [enabled (enabled-layers state)
        colored (assoc state :active-layers enabled :default-layers (set (layers/default-keys snapshot)))]
    (if visible-snapshot
      (let [contexts (if prepare?
                       (layers/prepare layer-registry visible-snapshot color-overrides)
                       (:layer-contexts state))]
        (assoc colored
               :layer-contexts contexts
               :page-colors (layers/page-colors layer-registry contexts enabled visible-snapshot)
               :folder-hues (layers/folder-hues contexts visible-snapshot)))
      colored)))

(defn- with-visibility
  "The state with the shown pages worked out again from the snapshot,
  views, filter, and header mode, then measured and colored. Editing
  stops once its page leaves the cloud."
  [{:keys [snapshot named-views view-filter header-mode] :as state}]
  (if snapshot
    (let [folded (if (= :hidden header-mode) (pairing/folding snapshot) {})
          shown-ids (reduce disj (visibility/visible-ids snapshot named-views view-filter) (keys folded))]
      (settle-editing (recolored (assoc state
                                        :visible-ids shown-ids
                                        :visible-snapshot (restriction/restrict snapshot shown-ids folded))
                                 true)))
    state))

(defn- show-snapshot
  "Shows `snapshot`. Requests still unanswered are forgotten, so a page
  whose text never came asks again."
  [state snapshot]
  (let [[shown lift-effects] (settle-lifts
                              (cond-> (with-visibility (assoc state :snapshot snapshot :requested-sources #{}))
                                (not (contains? (:namespaces snapshot) (:hovered-id state))) (assoc :hovered-id nil)))
        {:keys [state effects]} (if (:arrival shown)
                                  (arrival-effects shown)
                                  (changed shown [:snapshot-shown (:id snapshot)]))]
    {:state state :effects (into lift-effects effects)}))

(defn- select-snapshot [state snapshot-id]
  (let [state (assoc state :requested-snapshot-id snapshot-id)]
    (cond
      (= snapshot-id (get-in state [:snapshot :id])) (unchanged state)
      (and (= :working-tree snapshot-id) (:working-snapshot state)) (show-snapshot state (:working-snapshot state))
      :else (changed state (send-effect :view/select-snapshot {:snapshot-id snapshot-id})))))

(def spread-limits
  "Least and greatest spacing, as a multiple of the layout's own."
  {:least 0.4 :most 3.0})

(defn- bounded-spread [spread]
  (let [{:keys [least most]} spread-limits]
    (max least (min most (if (number? spread) spread 1.0)))))

;; Inline results

(defn- without-results-on [state path]
  (update state :inline-results #(into {} (remove (fn [[[result-path] _result]] (= path result-path))) %)))

(defn- results-still-matching
  "The state keeping the inline results on the file at `path` only where
  their line still reads as it did, now that the file holds `lines`."
  [state path lines]
  (update state :inline-results
          #(into {}
                 (remove (fn [[[result-path line] {:keys [line-text]}]]
                           (and (= path result-path) (not= line-text (get lines line)))))
                 %)))

;; Messages from the host

(defn- take-init
  "The state with the settings and preferences the host sends first."
  [state {:keys [settings theme-kind sky-mode spaces cloud-name editor-font display-mode view-filter
                 arrangement header-mode color-layers color-overrides spread totals-open? tint-clouds?
                 inspector-minimized? repl-geometry dock]}]
  (let [chosen-layers (when (sequential? color-layers) (vec (filter keyword? color-layers)))
        described-spaces (if (sequential? spaces) (vec spaces) [])
        chosen-colors (if (map? color-overrides) color-overrides {})
        shown-filter (if view-filter (merge visibility/no-filter view-filter) (:view-filter state))
        shown-headers (if header-mode (pairing/header-mode header-mode) (:header-mode state))
        updated (cond-> (assoc state
                               :settings (or settings {})
                               :theme-kind (or theme-kind :dark)
                               :spaces described-spaces
                               :sky-mode (:id (environments/environment-of sky-mode described-spaces))
                               :cloud-name cloud-name
                               :editor-font editor-font
                               :totals-open? (true? totals-open?)
                               :inspector-minimized? (true? inspector-minimized?)
                               :dock (dock-layout dock)
                               :tint-clouds? (not (false? tint-clouds?))
                               :color-layers chosen-layers
                               :color-overrides chosen-colors
                               :view-filter shown-filter
                               :header-mode shown-headers)
                  display-mode (assoc :display-mode display-mode)
                  arrangement (assoc :arrangement (:key (arrangement/arrangement arrangement)))
                  spread (assoc :spread (bounded-spread spread))
                  (repl-state/valid-geometry repl-geometry) (assoc-in [:repl :geometry] (repl-state/valid-geometry repl-geometry)))]
    (unchanged (cond
                 (or (not= shown-filter (:view-filter state)) (not= shown-headers (:header-mode state)))
                 (with-visibility updated)
                 (not= chosen-colors (:color-overrides state)) (recolored updated true)
                 (not= chosen-layers (:color-layers state)) (recolored updated false)
                 :else updated))))

(defn- take-views
  "The state with the cloud file's named views and color layers."
  [state {:keys [views color-layers]}]
  (let [custom-layers (vec color-layers)
        named-views (or views {})
        layers-changed? (not= custom-layers (:custom-layers state))
        views-changed? (not= named-views (:named-views state))
        updated (cond-> (assoc state :named-views named-views)
                  layers-changed? (assoc :custom-layers custom-layers :layer-registry (layers/registry custom-layers)))]
    (unchanged (cond
                 views-changed? (with-visibility updated)
                 layers-changed? (recolored updated true)
                 :else updated))))

(defn- take-snapshot [state {:keys [snapshot]}]
  (let [state (cond-> state
                (= :working-tree (:id snapshot)) (-> (assoc :working-snapshot snapshot) settle-buffers))]
    (if (= (:id snapshot) (:requested-snapshot-id state))
      (show-snapshot state snapshot)
      (unchanged state))))

(defn- take-sources
  "The state holding arrived source texts, keeping the most recent, and
  keeping inline results only on working-tree lines that still read as
  they did."
  [state {:keys [snapshot-id entries]}]
  (let [kept-sources 160
        arrived-keys (mapv :source-key entries)
        order (into (vec (remove (set arrived-keys) (:source-order state))) arrived-keys)
        evicted (drop-last kept-sources order)
        result-paths (into #{} (map (comp first key)) (:inline-results state))
        checked (if (and (= :working-tree snapshot-id) (seq result-paths))
                  (reduce (fn [checked {:keys [path text]}]
                            (if (contains? result-paths path)
                              (results-still-matching checked path (editing-state/text-lines (or text "")))
                              checked))
                          state
                          (mapcat :files entries))
                  state)]
    (apply changed
           (-> checked
               (update :requested-sources #(reduce disj % arrived-keys))
               (assoc :source-order (vec (take-last kept-sources order)))
               (assoc :sources (apply dissoc
                                      (into (:sources state) (map (juxt :source-key :files)) entries)
                                      evicted)))
           (map (fn [source-key] [:source-arrived source-key]) arrived-keys))))

(defn- arrive
  "Starts the view at the line the viewer zoomed out of in the editor."
  [state message]
  (let [[state lift-effects] (set-down-everything (stop-editing state))
        state (assoc state
                     :arrival (select-keys message [:path :line :text-scale])
                     :menu nil
                     :playing? false)
        {:keys [state effects]} (if (= :working-tree (get-in state [:snapshot :id]))
                                  (arrival-effects (assoc state :requested-snapshot-id :working-tree))
                                  (select-snapshot state :working-tree))]
    {:state state :effects (into lift-effects effects)}))

;; Interaction

(defn- click-namespace
  "Lifts the clicked page, or sets it down when lifted."
  [state namespace-id]
  (cond
    (= namespace-id (:focused-id state))
    (changed (assoc state :focused-id nil :menu nil) [:set-down namespace-id])

    ;; A click lands a moment after the pointer rises, by which time the
    ;; page may have left the cloud.
    (not (contains? (visible-ids state) namespace-id))
    (unchanged state)

    :else
    (let [[state source-effect] (request-sources (repl-state/follow-page (assoc state :focused-id namespace-id :menu nil) namespace-id)
                                                 [namespace-id])]
      (changed state source-effect [:lift namespace-id]))))

(defn- fly-to-namespace [state namespace-id]
  (let [[settled lift-effects] (set-down-everything (stop-editing (assoc state :menu nil)))
        [state source-effect] (request-sources (repl-state/follow-page settled namespace-id) [namespace-id])]
    (apply changed state (concat lift-effects [source-effect [:fly-to namespace-id]]))))

(defn- click-folder
  "Lifts the clicked folder, or sets it down when lifted."
  [state directory]
  (if (= directory (:lifted-folder state))
    (changed (assoc state :lifted-folder nil :menu nil) [:set-down-folder directory])
    (let [{:keys [focused-id lifted-folder]} state]
      (changed (assoc state :lifted-folder directory :focused-id nil :menu nil)
               (when focused-id [:set-down focused-id])
               (when lifted-folder [:set-down-folder lifted-folder])
               [:lift-folder directory]))))

(defn- visit-namespace [state namespace-id]
  (let [[shown-state filter-effect] (revealed state namespace-id)
        [state source-effect] (request-sources (assoc shown-state :focused-id namespace-id :menu nil) [namespace-id])]
    (changed state filter-effect source-effect [:lift namespace-id])))

(defn- set-down-one [state]
  (cond
    (:focused-id state) (changed (assoc state :focused-id nil) [:set-down (:focused-id state)])
    (:lifted-folder state) (changed (assoc state :lifted-folder nil) [:set-down-folder (:lifted-folder state)])
    :else (unchanged state)))

(defn- click-empty
  "Closes what a click on the sky closes first, then sets down what is
  lifted."
  [state]
  (cond
    (:editing state) (unchanged (stop-editing state))
    (:menu state) (unchanged (assoc state :menu nil))
    (:color-chooser state) (unchanged (assoc state :color-chooser nil))
    :else (set-down-one state)))

(defn- without-results [state]
  (when (or (seq (:inline-results state)) (seq (:notices state)))
    (assoc state :inline-results {} :notices [])))

(defn- escape
  "Closes the innermost open thing, inline results and notices counting
  as one, then stops following an agent, then sets down what is lifted."
  [state]
  (cond
    (:menu state) (unchanged (assoc state :menu nil))
    (:color-chooser state) (unchanged (assoc state :color-chooser nil))
    (without-results state) (unchanged (without-results state))
    (:editing state) (unchanged (stop-editing state))
    (get-in state [:search :open?]) (unchanged (assoc state :search (:search initial-state)))
    (:views-open? state) (unchanged (assoc state :views-open? false))
    (:arrange-open? state) (unchanged (assoc state :arrange-open? false))
    (get-in state [:presence :pane-open?]) (unchanged (assoc-in state [:presence :pane-open?] false))
    (get-in state [:assistant :open?]) (unchanged (assoc-in state [:assistant :open?] false))
    (get-in state [:presence :following]) (unchanged (assoc-in state [:presence :following] nil))
    :else (set-down-one state)))

(defn- show-definition [state {:keys [namespace-id] :as location}]
  (let [[settled lift-effects] (set-down-everything (assoc state :menu nil))
        [state source-effect] (request-sources settled [namespace-id])]
    (apply changed state (concat lift-effects [source-effect [:fly-to-line location]]))))

(defn- jump-to-name
  "Flies to the definition of a name written on a page, offers a menu at
  the pointer when the name could mean several, or says none is shown."
  [state {written-name :name :keys [namespace-id x y]}]
  (let [menu-limit 10
        found (definitions/choices (:visible-snapshot state) namespace-id written-name menu-limit)]
    (case (count found)
      0 (unchanged (with-notice (assoc state :menu nil) (str "No page shown defines " written-name ".")))
      1 (show-definition state (first found))
      (unchanged (assoc state :menu {:kind :definitions :name written-name :choices found :x x :y y})))))

(defn- frame-directory [state directory]
  (let [[settled lift-effects] (set-down-everything (assoc state :menu nil))]
    (apply changed settled (conj lift-effects [:frame-directory directory]))))

(def ^:private interaction-handlers
  {:ready (fn [state] (changed state (send-effect :view/ready {})))
   :hover (quietly (fn [state namespace-id] (assoc state :hovered-id namespace-id)))
   :click-namespace (fn [state namespace-id]
                      (let [editing-id (get-in state [:editing :namespace-id])]
                        (cond
                          ;; A click on the lifted page being edited, off its
                          ;; code, leaves it lifted with the caret in place.
                          (and (= namespace-id editing-id) (= namespace-id (:focused-id state))) (unchanged state)
                          (= namespace-id editing-id) (click-namespace state namespace-id)
                          :else (click-namespace (stop-editing state) namespace-id))))
   :double-click-namespace fly-to-namespace
   :click-folder click-folder
   :visit-namespace visit-namespace
   :click-empty click-empty
   :escape escape
   :show-definition show-definition
   :jump-to-name jump-to-name
   :need-sources (fn [state namespace-ids]
                   (let [[state source-effect] (request-sources state namespace-ids)]
                     (changed state source-effect)))
   :dive (fn [state {:keys [path line column]}]
           (changed (stop-editing (assoc state :menu nil))
                    (send-effect :view/open-location {:path path :line line :column (or column 1)})))
   :frame-all (fn [state] (changed (assoc state :focused-id nil :lifted-folder nil :menu nil) [:frame-all]))
   :frame-directory frame-directory
   :cycle-sky-mode (remembered :sky-mode [:sky-mode] #(environments/next-id (:sky-mode %) (:spaces %)))
   :choose-sky-mode (remembered :sky-mode [:sky-mode] (fn [state id] (:id (environments/environment-of id (:spaces state)))))
   :toggle-display-mode (remembered :display-mode [:display-mode] #(if (= :names (:display-mode %)) :code :names))
   :toggle-editor-colors (remembered-toggle :editor-colors? [:settings :editor-colors?])
   :toggle-legend (quietly #(update % :legend-open? not))
   :toggle-inspector (remembered-toggle :inspector-minimized? [:inspector-minimized?])
   :toggle-pane-minimized (with-dock (fn [layout pane]
                                       (update layout :minimized #(if (contains? % pane) (disj % pane) (conj % pane)))))
   :resize-dock (with-dock (fn [layout width] (assoc layout :width width)))
   :resize-panes (with-dock (fn [layout weights] (update layout :weights merge weights)))
   :dismiss-notice (quietly (fn [state notice-index]
                              (update state :notices
                                      (fn [notices]
                                        (vec (keep-indexed (fn [index notice] (when (not= index notice-index) notice))
                                                           notices))))))})

;; Arranging the cloud

(defn- set-arrangement [state arrangement-key]
  (let [chosen (:key (arrangement/arrangement arrangement-key))]
    (if (= chosen (:arrangement state))
      (unchanged state)
      (let [[settled lift-effects] (set-down-everything (assoc state :arrangement chosen :moved-pages {} :menu nil))]
        (apply changed settled (remember :arrangement chosen) (conj lift-effects [:refit]))))))

(defn- set-header-mode
  "Puts header pages beside their sources, on their own, or out of the
  cloud, remembering the choice."
  [state mode-key]
  (let [chosen (pairing/header-mode mode-key)]
    (if (= chosen (:header-mode state))
      (unchanged state)
      (let [[settled lift-effects] (settle-lifts (with-visibility (assoc state :header-mode chosen :menu nil)))]
        (apply changed settled (remember :header-mode chosen) (conj lift-effects [:refit]))))))

(defn- choose-layers [state chosen]
  (changed (recolored (assoc state :color-layers chosen) false) (remember :color-layers chosen)))

(defn- toggle-color-layer [state layer-key]
  (let [enabled (enabled-layers state)
        toggled (if (contains? enabled layer-key) (disj enabled layer-key) (conj enabled layer-key))]
    (choose-layers state (into [] (comp (map :key) (filter toggled)) (:layer-registry state)))))

(defn- with-chosen-color
  "The state recolored with `colors` chosen for `target`, [layer-key
  entry], or with the choice undone when `colors` is nil."
  [state [layer-key entry] colors]
  (let [overrides (if colors
                    (assoc-in (:color-overrides state) [layer-key entry] colors)
                    (let [remaining (dissoc (get-in state [:color-overrides layer-key]) entry)]
                      (if (seq remaining)
                        (assoc (:color-overrides state) layer-key remaining)
                        (dissoc (:color-overrides state) layer-key))))]
    (recolored (assoc state :color-overrides overrides) true)))

(defn- choose-color [state target colors]
  (let [chosen (with-chosen-color state target colors)]
    (changed chosen (remember :color-overrides (:color-overrides chosen)))))

(def ^:private set-spread
  (remembered :spread [:spread] (fn [_state spread] (bounded-spread spread))))

(def ^:private arrangement-handlers
  {:toggle-arrange-panel (side-pane-toggle :arrange)
   :set-arrangement set-arrangement
   :set-header-mode set-header-mode
   :toggle-color-layer toggle-color-layer
   :set-color-layers (fn [state layer-keys] (choose-layers state (some-> layer-keys vec)))
   :toggle-cloud-tint (remembered-toggle :tint-clouds? [:tint-clouds?])
   :open-color-chooser (quietly (fn [state chooser] (assoc state :color-chooser chooser :menu nil)))
   :close-color-chooser (quietly #(assoc % :color-chooser nil))
   :preview-color (quietly with-chosen-color)
   :choose-color choose-color
   :reset-color (fn [state target] (choose-color state target nil))
   :toggle-totals (remembered-toggle :totals-open? [:totals-open?])
   :preview-spread (quietly (fn [state spread] (assoc state :spread (bounded-spread spread))))
   :set-spread set-spread
   :step-spread (fn [state direction]
                  (let [step-ratio 1.25]
                    (set-spread state (* (:spread state) (Math/pow step-ratio direction)))))
   :move-page (quietly (fn [state namespace-id offset] (assoc-in state [:moved-pages namespace-id] offset)))
   :put-back (quietly (fn [state namespace-id] (-> state (update :moved-pages dissoc namespace-id) (assoc :menu nil))))
   :put-everything-back (quietly #(assoc % :moved-pages {} :menu nil))})

;; Views and filters

(defn- filtering
  "A handler saving the view filter that `next-filter` makes of the state
  and the event's arguments, framing the cloud again unless `refit?` is
  false."
  ([next-filter] (filtering next-filter true))
  ([next-filter refit?]
   (fn [state & arguments]
     (save-filter state (apply next-filter state arguments) refit?))))

(defn- save-view
  "Asks the host to write the current selection into the cloud file under
  `view-label`."
  [state view-label]
  (let [trimmed (string/trim (or view-label ""))]
    (if (string/blank? trimmed)
      (unchanged state)
      (changed state (send-effect :view/save-view
                                  {:name trimmed
                                   :selection (visibility/as-selection (:named-views state) (:view-filter state)
                                                                       (:snapshot state))})))))

(def ^:private filter-handlers
  {:toggle-views-panel (side-pane-toggle :views)
   :toggle-view (filtering (fn [state view-label] (visibility/toggle-view (:view-filter state) view-label)))
   :show-only (filtering (fn [_state patterns] (visibility/show-only patterns)))
   :hide-page (filtering (fn [state namespace-id] (visibility/hide-pages (:view-filter state) (:snapshot state) [namespace-id])))
   :hide-folder (filtering (fn [state directory] (visibility/hide-folder (:view-filter state) directory)))
   :clear-filter (filtering (fn [_state] visibility/no-filter))
   :toggle-page (filtering (fn [{:keys [view-filter snapshot named-views] :as state} namespace-id]
                             (visibility/toggle-pages view-filter snapshot named-views (visible-ids state) [namespace-id]))
                           false)
   :toggle-folder (filtering (fn [{:keys [view-filter snapshot named-views] :as state} directory]
                               (visibility/toggle-folder view-filter snapshot named-views (visible-ids state) directory))
                             false)
   :toggle-language (filtering (fn [{:keys [view-filter snapshot named-views] :as state} language-id]
                                 (visibility/toggle-language view-filter snapshot named-views (visible-ids state) language-id))
                               false)
   :toggle-folder-open (quietly (fn [state directory]
                                  (update state :open-folders #(if (contains? % directory) (disj % directory) (conj % directory)))))
   :save-view save-view
   :open-menu (quietly (fn [state menu] (assoc state :menu menu)))})

;; Search

(def search-result-limit
  "Most namespaces the search lists."
  8)

(defn search-results
  "Ids of the shown pages the search lists, most important first."
  [{:keys [visible-snapshot search] :as state}]
  (vec (take search-result-limit (scene/search-results visible-snapshot (:query search) (visible-ids state)))))

(defn- lift-search-result [state namespace-id]
  (let [[state source-effect] (request-sources (repl-state/follow-page (assoc state :search (:search initial-state)
                                                                              :focused-id namespace-id
                                                                              :menu nil)
                                                                       namespace-id)
                                               [namespace-id])]
    (changed state source-effect [:lift namespace-id])))

(defn- search-submit [state]
  (let [results (search-results state)]
    (if (seq results)
      (lift-search-result state (get results (get-in state [:search :selected] 0) (first results)))
      (unchanged state))))

(def ^:private search-handlers
  {:open-search (quietly #(assoc-in % [:search :open?] true))
   :close-search (quietly #(assoc % :search (:search initial-state)))
   :search-query (quietly (fn [state query] (assoc state :search {:open? true :query query :selected 0})))
   :search-move (quietly (fn [state direction]
                           (let [result-count (count (search-results state))]
                             (if (pos? result-count)
                               (update-in state [:search :selected] #(mod (+ (or % 0) direction) result-count))
                               state))))
   :search-submit search-submit
   :search-pick lift-search-result})

;; History

(defn commit-index
  "Position of the requested snapshot among the commits, newest first, or
  nil for the working tree."
  [state]
  (let [requested (:requested-snapshot-id state)]
    (first (keep-indexed (fn [index commit] (when (= requested (:sha commit)) index))
                         (:commits state)))))

(defn- step-target
  "Snapshot id one step newer for a positive `direction`, or older, or nil
  past either end."
  [state direction]
  (let [commits (:commits state)
        index (commit-index state)]
    (cond
      (and (nil? index) (neg? direction)) (:sha (first commits))
      (nil? index) nil
      (pos? direction) (if (zero? index) :working-tree (:sha (nth commits (dec index))))
      :else (:sha (get commits (inc index))))))

(defn- toggle-playback
  "Starts or stops playing the history, from the oldest commit when the
  view shows the working tree."
  [state]
  (cond
    (:playing? state) (unchanged (assoc state :playing? false))
    (empty? (:commits state)) (unchanged state)
    (nil? (commit-index state)) (select-snapshot (assoc state :playing? true) (:sha (peek (:commits state))))
    :else (unchanged (assoc state :playing? true))))

(defn- playback-tick
  "Steps a playing history one commit newer, stopping at the working tree."
  [state]
  (if (and (:playing? state) (not (awaiting-snapshot? state)))
    (let [target (step-target state 1)]
      (if target
        (select-snapshot (cond-> state (= :working-tree target) (assoc :playing? false)) target)
        (unchanged (assoc state :playing? false))))
    (unchanged state)))

(def ^:private history-handlers
  {:select-snapshot (fn [state snapshot-id] (select-snapshot (assoc state :playing? false) snapshot-id))
   :step-history (fn [state direction]
                   (if-let [target (step-target state direction)]
                     (select-snapshot (assoc state :playing? false) target)
                     (unchanged state)))
   :toggle-playback toggle-playback
   :playback-tick playback-tick})

;; Editing from the cloud

(defn- file-language [node path]
  (or (some #(when (= path (:path %)) (:language %)) (:files node))
      (page/language-of node)))

(defn editable?
  "True when the file at `path` on page `node` can be edited from the
  cloud, as a working tree file of a language CodeCloud reads with an
  adapter can."
  [state node path]
  (and (= :working-tree (get-in state [:snapshot :id]))
       (catalog/analyzed? (file-language node path))))

(defn comment-marker
  "What comments out a line of the file at `path` in `language-id`, or nil
  for files whose comments only enclose text."
  [path language-id]
  (when-not (or (= :html language-id) (= "css" (source/file-extension path)))
    (:line-comment (catalog/language language-id))))

(defn- new-buffer
  "A buffer for the file at `path` on page `namespace-id` holding `text`,
  with no edits sent yet."
  [namespace-id path language text]
  (let [opened (editing-state/buffer text)]
    (assoc opened
           :namespace-id namespace-id
           :path path
           :language language
           :unit (editing-state/indent-unit (:lines opened) language)
           :revision 0
           :acked 0
           :acked-key nil
           :epoch 0)))

(defn- working-content-keys [state]
  (into {}
        (for [node (vals (get-in state [:working-snapshot :namespaces]))
              {:keys [path content-key]} (:files node)]
          [path content-key])))

(defn- settle-buffers
  "The state without the buffers the working tree has caught up with,
  those never changed and those whose last acknowledged edit the snapshot
  already holds."
  [state]
  (let [editing-path (get-in state [:editing :path])
        content-keys (delay (working-content-keys state))]
    (update state :buffers
            (fn [buffers]
              (into {}
                    (remove (fn [[path {:keys [revision acked acked-key]}]]
                              (and (not= path editing-path)
                                   (or (zero? revision)
                                       (and (= revision acked)
                                            (some? acked-key)
                                            (= acked-key (get @content-keys path)))))))
                    buffers)))))

(defn- stop-editing [state]
  (settle-buffers (assoc state :editing nil)))

(defn- settle-editing
  "The state without its editing session when the page left the cloud or
  the cloud shows a commit."
  [state]
  (let [{:keys [namespace-id]} (:editing state)]
    (if (and namespace-id
             (or (not= :working-tree (get-in state [:snapshot :id]))
                 (not (contains? (visible-ids state) namespace-id))))
      (stop-editing state)
      state)))

(defn- with-dirty [state path dirty?]
  (update state :dirty-paths (if dirty? conj disj) path))

(defn- begin-editing
  "Puts the caret at `position` in the file at `path` on a page, opening a
  buffer for it, or explains why the file cannot be edited."
  [state {:keys [namespace-id path position select columns rows]}]
  (let [node (get-in state [:snapshot :namespaces namespace-id])
        language (file-language node path)
        existing (get-in state [:buffers path])
        source-file (some #(when (= path (:path %)) %) (source-of state namespace-id))]
    (cond
      (not= :working-tree (get-in state [:snapshot :id]))
      (unchanged (with-notice state "History is read-only. Return to Now to edit."))

      (not (catalog/analyzed? language))
      (unchanged (with-notice state (str (:label (catalog/language language)) " is read-only until CodeCloud reads it.")))

      (and (nil? existing) (nil? source-file))
      (unchanged state)

      :else
      (let [opened (or existing (new-buffer namespace-id path language (:text source-file)))
            placed (case select
                     :word (let [[from to] (editing-state/word-range opened position language)]
                             (editing-state/select-range opened from to))
                     :line (let [[from to] (editing-state/line-range opened position)]
                             (editing-state/select-range opened from to))
                     (editing-state/place opened position false))]
        (unchanged (-> (if (= path (get-in state [:editing :path])) state (stop-editing state))
                       (assoc-in [:buffers path] placed)
                       (assoc :editing {:namespace-id namespace-id :path path :columns columns :rows rows}
                              :menu nil)
                       (repl-state/follow-page namespace-id)))))))

(defn- edited
  "The state after `edit` changed the buffer being edited. A change to
  the text goes to the host and clears the file's inline results."
  [state edit]
  (let [path (get-in state [:editing :path])
        before (get-in state [:buffers path])
        after (edit before)
        change (when-not (identical? (:lines before) (:lines after))
                 (editing-state/change-between (:lines before) (:lines after)))]
    (if change
      (let [revision (inc (:revision before))]
        (changed (-> state
                     (assoc-in [:buffers path] (assoc after :revision revision))
                     (with-dirty path true)
                     (without-results-on path))
                 (send-effect :view/edit (assoc change :path path :revision revision :epoch (:epoch before)))))
      (unchanged (assoc-in state [:buffers path] after)))))

(defn- edit
  "Applies an editing `command` to the buffer being edited."
  [state command & arguments]
  (let [{:keys [path rows]} (:editing state)
        {:keys [language unit]} (get-in state [:buffers path])
        options {:language language :tab-size page/tab-size :page-lines (max 1 (or rows 1))}
        [first-argument second-argument] arguments]
    (if (get-in state [:buffers path])
      (edited state
              (case command
                :type #(cond
                         (string/includes? first-argument "\n") (editing-state/insert % first-argument :paste)
                         (false? (get-in state [:settings :pairs-brackets?])) (editing-state/insert % first-argument :typing)
                         :else (editing-state/type-text % first-argument language))
                :paste #(editing-state/insert % first-argument :paste)
                :cut #(editing-state/insert % "" :cut)
                :move #(editing-state/move % first-argument second-argument options)
                :delete #(editing-state/delete % first-argument options)
                :break-line #(editing-state/break-line % language unit)
                :indent #(editing-state/indent % unit page/tab-size)
                :outdent #(editing-state/outdent % unit)
                :comment #(if-let [marker (comment-marker path language)] (editing-state/toggle-comment % marker) %)
                :undo editing-state/undo
                :redo editing-state/redo
                :select-all editing-state/select-all
                :place #(editing-state/place % first-argument second-argument)
                :select #(editing-state/select-range % first-argument second-argument)
                :select-word #(let [[from to] (editing-state/word-range % first-argument language)]
                                (editing-state/select-range % from to))
                :select-line #(let [[from to] (editing-state/line-range % first-argument)]
                                (editing-state/select-range % from to))))
      (unchanged state))))

(defn- acknowledge-edit [state {:keys [path revision content-key dirty?]}]
  (settle-buffers (cond-> (with-dirty state path dirty?)
                    (get-in state [:buffers path])
                    (update-in [:buffers path] assoc :acked revision :acked-key content-key))))

(defn- take-document
  "The state with the document's text for `path` in its buffer, after an
  edit that could not apply or a change from elsewhere, keeping inline
  results only on lines that still read as they did."
  [state {:keys [path text content-key epoch dirty?]}]
  (settle-buffers
   (cond-> (-> state
               (with-dirty path dirty?)
               (results-still-matching path (editing-state/text-lines text)))
     (get-in state [:buffers path])
     (update-in [:buffers path] (fn [current]
                                  (assoc (editing-state/rebase current text)
                                         :epoch epoch
                                         :acked (:revision current)
                                         :acked-key content-key))))))

(defn- request-format
  "Asks the host to format the file being edited with the editor's
  formatter, the top-level forms at the caret for :form and the whole
  file for :file."
  [state scope]
  (let [path (get-in state [:editing :path])
        {:keys [lines language revision epoch] :as buffer} (get-in state [:buffers path])]
    (if buffer
      (let [[start-line end-line] (when (= :form scope) (editing-state/format-range buffer))]
        (changed state (send-effect :view/format (cond-> {:path path :language language :revision revision :epoch epoch
                                                         :text (string/join "\n" lines)}
                                                  start-line (assoc :start-line start-line :end-line end-line)))))
      (unchanged state))))

(defn- tab
  "Formats the top-level Clojure form at the caret when Tab is pressed
  while editing, as Calva does, and otherwise indents the selected lines,
  or the caret's."
  [state]
  (let [{:keys [language] :as buffer} (get-in state [:buffers (get-in state [:editing :path])])]
    (if (and buffer (= :clojure language) (not (editing-state/selected? buffer)))
      (request-format state :form)
      (edit state :indent))))

(defn- take-formatted
  "The state after the host formatted the text of `path`, the buffer
  taking the formatted text as one change undo takes back, unless it
  changed after the text was sent."
  [state {:keys [path revision epoch text]}]
  (let [buffer (get-in state [:buffers path])]
    (if (and buffer text (= path (get-in state [:editing :path]))
             (= revision (:revision buffer)) (= epoch (:epoch buffer)))
      (edited state #(editing-state/reformatted % (editing-state/text-lines text) (:language %)))
      (unchanged state))))

(def ^:private editing-handlers
  {:begin-editing begin-editing
   :edit edit
   :tab tab
   :format-code request-format
   ;; The first key of a chord, such as Ctrl+Alt+C, waiting for its second.
   :chord (fn [state first-key] (unchanged (assoc state :chord first-key)))
   ;; Escape takes away inline results and notices before it stops editing.
   :end-editing (fn [state] (unchanged (or (without-results state) (stop-editing state))))
   :save-file (fn [state]
                (if-let [path (get-in state [:editing :path])]
                  (changed state (send-effect :view/save {:path path}))
                  (unchanged state)))})

;; Agents

(defn- take-presence
  "The state after the host said where agents work, with a notice for each
  change to a file open in the cloud and a flight when the followed agent
  moved."
  [state message]
  (let [before (:presence state)
        after (presence-state/received before message)
        [notices latest] (presence-state/change-notices (:agents after) (set (keys (:buffers state))) (:noticed-through before))
        noticed (reduce with-notice
                        (assoc state :presence (assoc after :noticed-through latest))
                        (remove (set (:notices state)) notices))
        destination (when (= :working-tree (get-in state [:snapshot :id]))
                      (presence-state/followed-place before after (presence-state/page-index (:snapshot state)) (visible-ids state)))]
    (if destination
      (let [[requested source-effect] (request-sources noticed [(:namespace-id destination)])]
        (changed requested source-effect [:fly-to-line (assoc destination :framing :following)]))
      (unchanged noticed))))

(defn- show-place
  "Takes the camera to `place`, {:path :line}. A page the filter hides
  shows again and lifts, and a page in view is approached at the line,
  with everything set down. A place no page holds changes nothing."
  [state place]
  (let [namespace-id (when (and (:path place) (:snapshot state)) (namespace-for-path (:snapshot state) (:path place)))]
    (cond
      (nil? namespace-id) (unchanged state)
      (not (contains? (visible-ids state) namespace-id)) (visit-namespace state namespace-id)
      :else (show-definition state {:namespace-id namespace-id :path (:path place) :line (max 1 (or (:line place) 1))}))))

(defn- follow-agent
  "Follows agent `agent-id` with the camera until the person moves it,
  flying to where the agent works now, framed as following."
  [state agent-id]
  (let [following (assoc-in state [:presence :following] agent-id)]
    (if-let [place (:place (presence-state/agent-named (:presence state) agent-id))]
      (update (show-place following place) :effects
              (fn [effects]
                (mapv (fn [[kind payload :as effect]]
                        (if (= :fly-to-line kind)
                          [kind (cond-> (assoc payload :framing :following) (:whole? place) (assoc :whole? true))]
                          effect))
                      effects)))
      (unchanged following))))

(defn- dismiss-pin [state pin-id]
  (changed (update-in state [:presence :pins] (fn [pins] (filterv #(not= pin-id (:id %)) pins)))
           (send-effect :view/dismiss-pin {:pin-id pin-id})))

;; New files

(defn- context-directory
  "The folder a new file goes in unless one is named, that of the file
  being edited, else of the page or folder brought closer, else the
  cloud's own."
  [state]
  (or (some-> (get-in state [:editing :path]) source/directory-of)
      (some->> (:focused-id state) (get (:namespaces (:snapshot state))) page/directory)
      (:lifted-folder state)
      ""))

(defn- new-file
  "Asks the host for the name of a new file in `directory`, a folder of
  the cloud relative to its root, the empty string naming the root, or
  without one in the folder at hand."
  [state directory]
  (if (= :working-tree (get-in state [:snapshot :id]))
    (changed (assoc state :menu nil) (send-effect :view/new-file {:directory (or directory (context-directory state))}))
    (unchanged (with-notice (assoc state :menu nil) "History is read-only. Return to Now to make a file."))))

(defn- advance-pending-edit
  "`outcome` carried one step toward editing the new file the host made.
  Its page shows, its text is asked for, and the page comes closer with
  the caret in place; a file whose page never shows is let go."
  [{:keys [state effects] :as outcome}]
  (let [{:keys [path line column requested? waited] :as pending} (:pending-edit state)
        snapshot (:snapshot state)
        namespace-id (when (and pending (= :working-tree (:id snapshot))) (namespace-for-path snapshot path))
        most-snapshots-waited 4
        joined (fn [{next-state :state next-effects :effects}] {:state next-state :effects (into effects next-effects)})]
    (cond
      (nil? pending) outcome
      (nil? namespace-id) (cond-> outcome (> waited most-snapshots-waited) (update :state dissoc :pending-edit))

      (some #(= path (:path %)) (source-of state namespace-id))
      (let [{shown :state shown-effects :effects}
            (if (contains? (visible-ids state) namespace-id)
              (let [[settled lift-effects] (set-down-everything (assoc (dissoc state :pending-edit) :menu nil))]
                {:state (assoc settled :focused-id namespace-id) :effects (conj (vec lift-effects) [:lift namespace-id])})
              (show-place (dissoc state :pending-edit) {:path path :line (inc line)}))
            {begun :state begun-effects :effects} (begin-editing shown {:namespace-id namespace-id :path path
                                                                        :position [line column]})]
        {:state begun :effects (-> effects (into shown-effects) (into begun-effects))})

      requested? outcome
      :else (let [[requested source-effect] (request-sources (assoc-in state [:pending-edit :requested?] true) [namespace-id])]
              (joined (changed requested source-effect))))))

(defn- take-created
  "The state on its way to editing the new file the host made, at the
  place the host gave, once the file's page shows."
  [state {:keys [path line column]}]
  (advance-pending-edit (unchanged (assoc state :pending-edit {:path path :line (or line 0) :column (or column 0) :waited 0}))))

(defn- carrying-pending-edit
  "`take-message` followed by a step toward editing a new file, counting
  the snapshots it waits through when `snapshot?`."
  [take-message snapshot?]
  (fn [state message]
    (advance-pending-edit (take-message (cond-> state
                                          (and snapshot? (:pending-edit state)) (update-in [:pending-edit :waited] inc))
                                        message))))

(def ^:private agent-handlers
  {:connect-agent (fn [state] (changed state (send-effect :view/connect-agent {})))
   :agents-reporting (fn [state level]
                       (changed (assoc-in state [:agents :reporting] level)
                                (send-effect :view/agents-reporting {:level level})))
   :open-coworkers (quietly #(with-side-pane % :coworkers true))
   :toggle-coworkers (side-pane-toggle :coworkers)
   :toggle-assistant (side-pane-toggle :assistant)
   :follow-agent follow-agent
   :stop-following (quietly (fn [state] (assoc-in state [:presence :following] nil)))
   :show-agent (fn [state agent-id]
                 (show-place state (:place (presence-state/agent-named (:presence state) agent-id))))
   :show-pin (fn [state pin-id]
               (show-place state (some #(when (= pin-id (:id %)) %) (get-in state [:presence :pins]))))
   :show-place show-place
   :dismiss-pin dismiss-pin
   :new-file new-file})

;; Routing

(def ^:private message-handlers
  "Each host message's handler, by its :type. A handler takes the state
  and the message."
  {:host/init take-init
   :host/views take-views
   :host/snapshot (carrying-pending-edit take-snapshot true)
   :host/history (quietly (fn [state {:keys [commits available?]}]
                            (assoc state :commits (vec commits) :history-status (if available? :ready :unavailable))))
   :host/sources (carrying-pending-edit take-sources false)
   :host/status (quietly (fn [state {:keys [message progress]}]
                           (assoc state :status (when message {:message message :progress progress}))))
   :host/theme (quietly (fn [state {:keys [theme-kind]}] (assoc state :theme-kind theme-kind)))
   :host/editor-colors (quietly (fn [state {:keys [code-colors]}]
                                  (assoc state :editor-colors code-colors :editor-colors-read? true)))
   :host/notice (quietly (fn [state {:keys [message]}] (with-notice state message)))
   :host/shown (fn [state _message] (changed state [:return-from-dive]))
   :host/arrive arrive
   :host/edited (quietly acknowledge-edit)
   :host/document (quietly take-document)
   :host/dirty (quietly (fn [state {:keys [path dirty?]}] (with-dirty state path dirty?)))
   :host/agents (quietly (fn [state message]
                           (assoc state :agents (select-keys message [:serving? :url :agents :problem :reporting]))))
   :host/presence take-presence
   :host/formatted take-formatted
   :host/created take-created
   :host/diagnostics (quietly (fn [state {:keys [files]}] (assoc state :diagnostics (or files {}))))})

(def ^:private all-message-handlers
  (merge message-handlers
         (into {}
               (map (fn [[message-type take-message]] [message-type (quietly take-message)]))
               (concat repl-state/message-handlers assistant-state/message-handlers))
         repl-state/sending-message-handlers))

(def ^:private event-handlers
  "Each event's handler, by the event's first element. A host message
  arrives as [:host message] and routes again on its :type."
  (merge {:host (fn [state message]
                  (if-let [handler (get all-message-handlers (:type message))]
                    (handler state message)
                    (unchanged state)))}
         interaction-handlers
         arrangement-handlers
         filter-handlers
         search-handlers
         history-handlers
         editing-handlers
         repl-state/handlers
         agent-handlers
         assistant-state/handlers))

(defn handle
  "Applies one event, a vector led by its type, to the state. An event
  without a handler changes nothing."
  [state [event-type & arguments]]
  (if-let [handler (get event-handlers event-type)]
    (apply handler state arguments)
    (unchanged state)))

(defn handle-all
  "Applies events in order, collecting every effect."
  [state events]
  (reduce (fn [{:keys [state effects]} event]
            (let [outcome (handle state event)]
              {:state (:state outcome) :effects (into effects (:effects outcome))}))
          {:state state :effects []}
          events))

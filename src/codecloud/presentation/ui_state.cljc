(ns codecloud.presentation.ui-state
  "The cloud view's state and its changes. `handle` maps a state and an
  event to {:state :effects}, effects being data for the renderer."
  (:require [clojure.string :as string]
            [codecloud.languages.catalog :as catalog]
            [codecloud.model.restriction :as restriction]
            [codecloud.model.source :as source]
            [codecloud.presentation.arrangement :as arrangement]
            [codecloud.presentation.code-colors :as code-colors]
            [codecloud.presentation.editing :as editing]
            [codecloud.presentation.layers :as layers]
            [codecloud.presentation.page :as page]
            [codecloud.presentation.scene :as scene]
            [codecloud.presentation.sky :as sky]
            [codecloud.presentation.visibility :as visibility]
            [codecloud.protocol :as protocol]))

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
   :sky-mode :auto
   :theme-kind :dark
   :display-mode :code
   :arrangement :folders
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
   :buffers {}
   :dirty-paths #{}
   :settings {}
   :editor-font nil
   :editor-colors nil
   :editor-colors-read? false
   :cloud-name nil})

(defn- unchanged [state] {:state state :effects []})

(defn- changed [state & effects] {:state state :effects (vec (remove nil? effects))})

(defn- send-effect [message-type fields]
  [:send (protocol/message message-type fields)])

(defn- with-notice [state notice]
  (let [kept-notices 4]
    (update state :notices #(vec (take-last kept-notices (conj (vec %) notice))))))

(defmulti handle-event
  "Methods take the state and the event vector and return {:state :effects}."
  (fn [_state event] (first event)))

(defn handle
  "Applies one event to the state."
  [state event]
  (handle-event state event))

(defn handle-all
  "Applies events in order, collecting every effect."
  [state events]
  (reduce (fn [{:keys [state effects]} event]
            (let [outcome (handle state event)]
              {:state (:state outcome) :effects (into effects (:effects outcome))}))
          {:state state :effects []}
          events))

(defmethod handle-event :default [state _event]
  (unchanged state))

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

(defn- folder-shown?
  "True when some shown namespace lives in `directory`."
  [state shown-ids directory]
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
            (send-effect :view/save-preference {:key :view-filter :value view-filter})
            (cond-> lift-effects refit? (conj [:refit]))))))

(defn- set-down-everything
  "[state effects] with nothing lifted."
  [state]
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
       (send-effect :view/save-preference {:key :view-filter :value view-filter})])))

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
  "Keys of the color layers turned on: the viewer's choice, or the
  snapshot's defaults while :color-layers is nil."
  [{:keys [color-layers snapshot layer-registry]}]
  (let [offered (set (map :key layer-registry))]
    (into #{} (filter offered) (or color-layers (layers/default-keys snapshot)))))

(defn colored-by-default?
  "True while the layers turned on are the defaults for the snapshot."
  [{:keys [active-layers default-layers]}]
  (= active-layers default-layers))

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
  "The state with the shown pages worked out again from the snapshot, the
  named views, and the filter, then measured among themselves and
  colored. Editing stops once its page leaves the cloud."
  [{:keys [snapshot named-views view-filter] :as state}]
  (if snapshot
    (let [shown-ids (visibility/visible-ids snapshot named-views view-filter)]
      (settle-editing (recolored (assoc state
                                        :visible-ids shown-ids
                                        :visible-snapshot (restriction/restrict snapshot shown-ids))
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

(defn- select-snapshot
  "Asks for a snapshot, showing it at once when the view already has it."
  [state snapshot-id]
  (let [state (assoc state :requested-snapshot-id snapshot-id)]
    (cond
      (= snapshot-id (get-in state [:snapshot :id])) (unchanged state)
      (and (= :working-tree snapshot-id) (:working-snapshot state)) (show-snapshot state (:working-snapshot state))
      :else (changed state (send-effect :view/select-snapshot {:snapshot-id snapshot-id})))))

;; Messages from the host

(defmulti handle-host-message
  "Methods take the state and a host message and return {:state :effects}."
  (fn [_state message] (:type message)))

(defmethod handle-host-message :default [state _message]
  (unchanged state))

(defmethod handle-event :host [state [_ message]]
  (handle-host-message state message))

(def spread-limits
  "Least and greatest spacing, as a multiple of the layout's own."
  {:least 0.4 :most 3.0})

(defn- bounded-spread [spread]
  (let [{:keys [least most]} spread-limits]
    (max least (min most (if (number? spread) spread 1.0)))))

(defmethod handle-host-message :host/init
  [state {:keys [settings theme-kind sky-mode cloud-name editor-font display-mode view-filter
                 arrangement color-layers color-overrides spread totals-open? tint-clouds?]}]
  (let [chosen-layers (when (sequential? color-layers) (vec (filter keyword? color-layers)))
        chosen-colors (if (map? color-overrides) color-overrides {})
        shown-filter (if view-filter (merge visibility/no-filter view-filter) (:view-filter state))
        updated (cond-> (assoc state
                               :settings (or settings {})
                               :theme-kind (or theme-kind :dark)
                               :sky-mode (or sky-mode :auto)
                               :cloud-name cloud-name
                               :editor-font editor-font
                               :totals-open? (true? totals-open?)
                               :tint-clouds? (not (false? tint-clouds?))
                               :color-layers chosen-layers
                               :color-overrides chosen-colors
                               :view-filter shown-filter)
                  display-mode (assoc :display-mode display-mode)
                  arrangement (assoc :arrangement (:key (arrangement/arrangement arrangement)))
                  spread (assoc :spread (bounded-spread spread)))]
    (unchanged (cond
                 (not= shown-filter (:view-filter state)) (with-visibility updated)
                 (not= chosen-colors (:color-overrides state)) (recolored updated true)
                 (not= chosen-layers (:color-layers state)) (recolored updated false)
                 :else updated))))

(defmethod handle-host-message :host/views [state {:keys [views color-layers]}]
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

(defmethod handle-host-message :host/snapshot [state {:keys [snapshot]}]
  (let [state (cond-> state
                (= :working-tree (:id snapshot)) (-> (assoc :working-snapshot snapshot) settle-buffers))]
    (if (= (:id snapshot) (:requested-snapshot-id state))
      (show-snapshot state snapshot)
      (unchanged state))))

(defmethod handle-host-message :host/history [state {:keys [commits available?]}]
  (unchanged (assoc state
                    :commits (vec commits)
                    :history-status (if available? :ready :unavailable))))

(defmethod handle-host-message :host/sources [state {:keys [entries]}]
  (let [kept-sources 160
        arrived-keys (mapv :source-key entries)
        order (into (vec (remove (set arrived-keys) (:source-order state))) arrived-keys)
        evicted (drop-last kept-sources order)]
    (apply changed
           (-> state
               (update :requested-sources #(reduce disj % arrived-keys))
               (assoc :source-order (vec (take-last kept-sources order)))
               (assoc :sources (apply dissoc
                                      (into (:sources state) (map (juxt :source-key :files)) entries)
                                      evicted)))
           (map (fn [source-key] [:source-arrived source-key]) arrived-keys))))

(defmethod handle-host-message :host/status [state {:keys [message progress]}]
  (unchanged (assoc state :status (when message {:message message :progress progress}))))

(defmethod handle-host-message :host/theme [state {:keys [theme-kind]}]
  (unchanged (assoc state :theme-kind theme-kind)))

(defmethod handle-host-message :host/editor-colors [state {:keys [code-colors]}]
  (unchanged (assoc state :editor-colors code-colors :editor-colors-read? true)))

(defmethod handle-host-message :host/notice [state {:keys [message]}]
  (unchanged (with-notice state message)))

(defmethod handle-host-message :host/shown [state _message]
  (changed state [:return-from-dive]))

(defmethod handle-host-message :host/arrive [state message]
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

(defmethod handle-event :ready [state _event]
  (changed state (send-effect :view/ready {})))

(defmethod handle-event :hover [state [_ namespace-id]]
  (unchanged (assoc state :hovered-id namespace-id)))

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
    (let [[state source-effect] (request-sources (assoc state :focused-id namespace-id :menu nil) [namespace-id])]
      (changed state source-effect [:lift namespace-id]))))

(defmethod handle-event :click-namespace [state [_ namespace-id]]
  (click-namespace (if (= namespace-id (get-in state [:editing :namespace-id])) state (stop-editing state))
                   namespace-id))

(defmethod handle-event :double-click-namespace [state [_ namespace-id]]
  (let [[settled lift-effects] (set-down-everything (stop-editing (assoc state :menu nil)))
        [state source-effect] (request-sources settled [namespace-id])]
    (apply changed state (concat lift-effects [source-effect [:fly-to namespace-id]]))))

(defmethod handle-event :click-folder [state [_ directory]]
  (if (= directory (:lifted-folder state))
    (changed (assoc state :lifted-folder nil :menu nil) [:set-down-folder directory])
    (let [{:keys [focused-id lifted-folder]} state]
      (changed (assoc state :lifted-folder directory :focused-id nil :menu nil)
               (when focused-id [:set-down focused-id])
               (when lifted-folder [:set-down-folder lifted-folder])
               [:lift-folder directory]))))

(defmethod handle-event :visit-namespace [state [_ namespace-id]]
  (let [[shown-state filter-effect] (revealed state namespace-id)
        [state source-effect] (request-sources (assoc shown-state :focused-id namespace-id :menu nil) [namespace-id])]
    (changed state filter-effect source-effect [:lift namespace-id])))

(defn- set-down-one
  "Sets down the lifted page, or else the lifted folder."
  [state]
  (cond
    (:focused-id state) (changed (assoc state :focused-id nil) [:set-down (:focused-id state)])
    (:lifted-folder state) (changed (assoc state :lifted-folder nil) [:set-down-folder (:lifted-folder state)])
    :else (unchanged state)))

(defmethod handle-event :click-empty [state _event]
  (cond
    (:editing state) (unchanged (stop-editing state))
    (:menu state) (unchanged (assoc state :menu nil))
    (:color-chooser state) (unchanged (assoc state :color-chooser nil))
    :else (set-down-one state)))

(defmethod handle-event :escape [state _event]
  (cond
    (:menu state) (unchanged (assoc state :menu nil))
    (:color-chooser state) (unchanged (assoc state :color-chooser nil))
    (:editing state) (unchanged (stop-editing state))
    (get-in state [:search :open?]) (unchanged (assoc state :search (:search initial-state)))
    (:views-open? state) (unchanged (assoc state :views-open? false))
    (:arrange-open? state) (unchanged (assoc state :arrange-open? false))
    :else (set-down-one state)))

(defmethod handle-event :show-definition [state [_ {:keys [namespace-id] :as location}]]
  (let [[settled lift-effects] (set-down-everything state)
        [state source-effect] (request-sources settled [namespace-id])]
    (apply changed state (concat lift-effects [source-effect [:fly-to-line location]]))))

(defmethod handle-event :need-sources [state [_ namespace-ids]]
  (let [[state source-effect] (request-sources state namespace-ids)]
    (changed state source-effect)))

(defmethod handle-event :dive [state [_ {:keys [path line column]}]]
  (changed (stop-editing (assoc state :menu nil))
           (send-effect :view/open-location {:path path :line line :column (or column 1)})))

(defmethod handle-event :frame-all [state _event]
  (changed (assoc state :focused-id nil :lifted-folder nil :menu nil) [:frame-all]))

(defmethod handle-event :frame-directory [state [_ directory]]
  (let [[settled lift-effects] (set-down-everything (assoc state :menu nil))]
    (apply changed settled (conj lift-effects [:frame-directory directory]))))

(defmethod handle-event :cycle-sky-mode [state _event]
  (let [next-sky-mode (sky/next-mode (:sky-mode state))]
    (changed (assoc state :sky-mode next-sky-mode)
             (send-effect :view/save-preference {:key :sky-mode :value next-sky-mode}))))

(defmethod handle-event :toggle-display-mode [state _event]
  (let [next-mode (if (= :names (:display-mode state)) :code :names)]
    (changed (assoc state :display-mode next-mode)
             (send-effect :view/save-preference {:key :display-mode :value next-mode}))))

(defmethod handle-event :toggle-editor-colors [state _event]
  (let [editor-colors? (not (get-in state [:settings :editor-colors?]))]
    (changed (assoc-in state [:settings :editor-colors?] editor-colors?)
             (send-effect :view/save-preference {:key :editor-colors? :value editor-colors?}))))

(defn shown-code-colors
  "The code colors pages show: the editor theme's while the switch is on
  and the host could read them, CodeCloud's own otherwise."
  [{:keys [settings editor-colors]}]
  (if (and (:editor-colors? settings) editor-colors)
    editor-colors
    code-colors/own))

(defmethod handle-event :toggle-legend [state _event]
  (unchanged (update state :legend-open? not)))

(defmethod handle-event :dismiss-notice [state [_ notice-index]]
  (unchanged (update state :notices
                     (fn [notices]
                       (vec (keep-indexed (fn [index notice] (when (not= index notice-index) notice))
                                          notices))))))

;; Arranging the cloud

(defmethod handle-event :toggle-arrange-panel [state _event]
  (unchanged (-> state (update :arrange-open? not) (assoc :views-open? false :menu nil))))

(defmethod handle-event :set-arrangement [state [_ arrangement-key]]
  (let [chosen (:key (arrangement/arrangement arrangement-key))]
    (if (= chosen (:arrangement state))
      (unchanged state)
      (let [[settled lift-effects] (set-down-everything (assoc state :arrangement chosen :moved-pages {} :menu nil))]
        (apply changed settled
               (send-effect :view/save-preference {:key :arrangement :value chosen})
               (conj lift-effects [:refit]))))))

(defn- choose-layers
  "The state recolored with `chosen` layers on, nil for the defaults,
  remembering the choice."
  [state chosen]
  (changed (recolored (assoc state :color-layers chosen) false)
           (send-effect :view/save-preference {:key :color-layers :value chosen})))

(defmethod handle-event :toggle-color-layer [state [_ layer-key]]
  (let [enabled (enabled-layers state)
        toggled (if (contains? enabled layer-key) (disj enabled layer-key) (conj enabled layer-key))]
    (choose-layers state (into [] (comp (map :key) (filter toggled)) (:layer-registry state)))))

(defmethod handle-event :set-color-layers [state [_ layer-keys]]
  (choose-layers state (some-> layer-keys vec)))

(defmethod handle-event :toggle-cloud-tint [state _event]
  (let [tinted? (not (:tint-clouds? state))]
    (changed (assoc state :tint-clouds? tinted?)
             (send-effect :view/save-preference {:key :tint-clouds? :value tinted?}))))

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

(defmethod handle-event :open-color-chooser [state [_ chooser]]
  (unchanged (assoc state :color-chooser chooser :menu nil)))

(defmethod handle-event :close-color-chooser [state _event]
  (unchanged (assoc state :color-chooser nil)))

(defmethod handle-event :preview-color [state [_ target colors]]
  (unchanged (with-chosen-color state target colors)))

(defmethod handle-event :choose-color [state [_ target colors]]
  (let [chosen (with-chosen-color state target colors)]
    (changed chosen (send-effect :view/save-preference {:key :color-overrides :value (:color-overrides chosen)}))))

(defmethod handle-event :reset-color [state [_ target]]
  (handle-event state [:choose-color target nil]))

(defmethod handle-event :toggle-totals [state _event]
  (let [open? (not (:totals-open? state))]
    (changed (assoc state :totals-open? open?)
             (send-effect :view/save-preference {:key :totals-open? :value open?}))))

(defmethod handle-event :preview-spread [state [_ spread]]
  (unchanged (assoc state :spread (bounded-spread spread))))

(defmethod handle-event :set-spread [state [_ spread]]
  (let [bounded (bounded-spread spread)]
    (changed (assoc state :spread bounded)
             (send-effect :view/save-preference {:key :spread :value bounded}))))

(defmethod handle-event :step-spread [state [_ direction]]
  (let [step-ratio 1.25]
    (handle-event state [:set-spread (* (:spread state) (Math/pow step-ratio direction))])))

(defmethod handle-event :move-page [state [_ namespace-id offset]]
  (unchanged (assoc-in state [:moved-pages namespace-id] offset)))

(defmethod handle-event :put-back [state [_ namespace-id]]
  (unchanged (-> state (update :moved-pages dissoc namespace-id) (assoc :menu nil))))

(defmethod handle-event :put-everything-back [state _event]
  (unchanged (assoc state :moved-pages {} :menu nil)))

;; Views and filters

(defmethod handle-event :toggle-views-panel [state _event]
  (unchanged (-> state (update :views-open? not) (assoc :arrange-open? false :menu nil))))

(defmethod handle-event :toggle-view [state [_ view-label]]
  (save-filter state (visibility/toggle-view (:view-filter state) view-label)))

(defmethod handle-event :show-only [state [_ patterns]]
  (save-filter state (visibility/show-only patterns)))

(defmethod handle-event :hide-page [state [_ namespace-id]]
  (save-filter state (visibility/hide-pages (:view-filter state) (:snapshot state) [namespace-id])))

(defmethod handle-event :hide-folder [state [_ directory]]
  (save-filter state (visibility/hide-folder (:view-filter state) directory)))

(defmethod handle-event :clear-filter [state _event]
  (save-filter state visibility/no-filter))

(defmethod handle-event :toggle-page [{:keys [view-filter snapshot named-views] :as state} [_ namespace-id]]
  (save-filter state (visibility/toggle-pages view-filter snapshot named-views (visible-ids state) [namespace-id]) false))

(defmethod handle-event :toggle-folder [{:keys [view-filter snapshot named-views] :as state} [_ directory]]
  (save-filter state (visibility/toggle-folder view-filter snapshot named-views (visible-ids state) directory) false))

(defmethod handle-event :toggle-language [{:keys [view-filter snapshot named-views] :as state} [_ language-id]]
  (save-filter state (visibility/toggle-language view-filter snapshot named-views (visible-ids state) language-id) false))

(defmethod handle-event :toggle-folder-open [state [_ directory]]
  (unchanged (update state :open-folders #(if (contains? % directory) (disj % directory) (conj % directory)))))

(defmethod handle-event :save-view [state [_ view-label]]
  (let [trimmed (string/trim (or view-label ""))]
    (if (string/blank? trimmed)
      (unchanged state)
      (changed state (send-effect :view/save-view
                                  {:name trimmed
                                   :selection (visibility/as-selection (:named-views state) (:view-filter state)
                                                                       (:snapshot state))})))))

(defmethod handle-event :open-menu [state [_ menu]]
  (unchanged (assoc state :menu menu)))

;; Search

(def search-result-limit
  "Most namespaces the search lists."
  8)

(defn search-results
  "Ids of the shown pages the search lists, most important first."
  [{:keys [visible-snapshot search] :as state}]
  (vec (take search-result-limit (scene/search-results visible-snapshot (:query search) (visible-ids state)))))

(defn- lift-search-result [state namespace-id]
  (let [[state source-effect] (request-sources (assoc state :search (:search initial-state)
                                                      :focused-id namespace-id
                                                      :menu nil)
                                               [namespace-id])]
    (changed state source-effect [:lift namespace-id])))

(defmethod handle-event :open-search [state _event]
  (unchanged (assoc-in state [:search :open?] true)))

(defmethod handle-event :close-search [state _event]
  (unchanged (assoc state :search (:search initial-state))))

(defmethod handle-event :search-query [state [_ query]]
  (unchanged (assoc state :search {:open? true :query query :selected 0})))

(defmethod handle-event :search-move [state [_ direction]]
  (let [result-count (count (search-results state))]
    (if (pos? result-count)
      (unchanged (update-in state [:search :selected] #(mod (+ (or % 0) direction) result-count)))
      (unchanged state))))

(defmethod handle-event :search-submit [state _event]
  (let [results (search-results state)]
    (if (seq results)
      (lift-search-result state (get results (get-in state [:search :selected] 0) (first results)))
      (unchanged state))))

(defmethod handle-event :search-pick [state [_ namespace-id]]
  (lift-search-result state namespace-id))

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

(defmethod handle-event :select-snapshot [state [_ snapshot-id]]
  (select-snapshot (assoc state :playing? false) snapshot-id))

(defmethod handle-event :step-history [state [_ direction]]
  (if-let [target (step-target state direction)]
    (select-snapshot (assoc state :playing? false) target)
    (unchanged state)))

(defmethod handle-event :toggle-playback [state _event]
  (cond
    (:playing? state) (unchanged (assoc state :playing? false))
    (empty? (:commits state)) (unchanged state)
    (nil? (commit-index state)) (select-snapshot (assoc state :playing? true) (:sha (peek (:commits state))))
    :else (unchanged (assoc state :playing? true))))

(defmethod handle-event :playback-tick [state _event]
  (if (and (:playing? state) (not (awaiting-snapshot? state)))
    (let [target (step-target state 1)]
      (if target
        (select-snapshot (cond-> state (= :working-tree target) (assoc :playing? false)) target)
        (unchanged (assoc state :playing? false))))
    (unchanged state)))

;; Editing from the cloud

(defn- file-language
  "The language of the file at `path` on page `node`."
  [node path]
  (or (some #(when (= path (:path %)) (:language %)) (:files node))
      (page/language-of node)))

(defn editable?
  "True when the file at `path` on page `node` can be edited from the
  cloud: a file of a language CodeCloud reads with an adapter, in the
  working tree."
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
  (let [opened (editing/buffer text)]
    (assoc opened
           :namespace-id namespace-id
           :path path
           :language language
           :unit (editing/indent-unit (:lines opened) language)
           :revision 0
           :acked 0
           :acked-key nil
           :epoch 0)))

(defn- working-content-keys
  "Map of each file path in the working tree to its content key."
  [state]
  (into {}
        (for [node (vals (get-in state [:working-snapshot :namespaces]))
              {:keys [path content-key]} (:files node)]
          [path content-key])))

(defn- settle-buffers
  "The state without the buffers the working tree has caught up with: not
  being edited, and either never changed or with every edit acknowledged
  and the working tree's snapshot holding the text the last one left."
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

(defn- stop-editing
  "The state with no file being edited."
  [state]
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

(defn- with-dirty
  "The state noting whether the file at `path` has unsaved changes."
  [state path dirty?]
  (update state :dirty-paths (if dirty? conj disj) path))

(defmethod handle-event :begin-editing [state [_ {:keys [namespace-id path position select columns rows]}]]
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
                     :word (let [[from to] (editing/word-range opened position language)]
                             (editing/select-range opened from to))
                     :line (let [[from to] (editing/line-range opened position)]
                             (editing/select-range opened from to))
                     (editing/place opened position false))]
        (unchanged (-> (if (= path (get-in state [:editing :path])) state (stop-editing state))
                       (assoc-in [:buffers path] placed)
                       (assoc :editing {:namespace-id namespace-id :path path :columns columns :rows rows}
                              :menu nil)))))))

(defn- edited
  "The state after `edit` changed the buffer being edited, sending the
  change to the host when the text changed."
  [state edit]
  (let [path (get-in state [:editing :path])
        before (get-in state [:buffers path])
        after (edit before)
        change (when-not (identical? (:lines before) (:lines after))
                 (editing/change-between (:lines before) (:lines after)))]
    (if change
      (let [revision (inc (:revision before))]
        (changed (-> state
                     (assoc-in [:buffers path] (assoc after :revision revision))
                     (with-dirty path true))
                 (send-effect :view/edit (assoc change :path path :revision revision :epoch (:epoch before)))))
      (unchanged (assoc-in state [:buffers path] after)))))

(defmethod handle-event :edit [state [_ command & arguments]]
  (let [{:keys [path rows]} (:editing state)
        {:keys [language unit]} (get-in state [:buffers path])
        options {:language language :tab-size page/tab-size :page-lines (max 1 (or rows 1))}
        [first-argument second-argument] arguments]
    (if (get-in state [:buffers path])
      (edited state
              (case command
                :type #(editing/insert % first-argument (if (string/includes? first-argument "\n") :paste :typing))
                :paste #(editing/insert % first-argument :paste)
                :cut #(editing/insert % "" :cut)
                :move #(editing/move % first-argument second-argument options)
                :delete #(editing/delete % first-argument options)
                :break-line #(editing/break-line % language unit)
                :indent #(editing/indent % unit page/tab-size)
                :outdent #(editing/outdent % unit)
                :comment #(if-let [marker (comment-marker path language)] (editing/toggle-comment % marker) %)
                :undo editing/undo
                :redo editing/redo
                :select-all editing/select-all
                :place #(editing/place % first-argument second-argument)
                :select #(editing/select-range % first-argument second-argument)
                :select-word #(let [[from to] (editing/word-range % first-argument language)]
                                (editing/select-range % from to))
                :select-line #(let [[from to] (editing/line-range % first-argument)]
                                (editing/select-range % from to))))
      (unchanged state))))

(defmethod handle-event :end-editing [state _event]
  (unchanged (stop-editing state)))

(defmethod handle-event :save-file [state _event]
  (if-let [path (get-in state [:editing :path])]
    (changed state (send-effect :view/save {:path path}))
    (unchanged state)))

(defmethod handle-host-message :host/edited [state {:keys [path revision content-key dirty?]}]
  (unchanged (settle-buffers (cond-> (with-dirty state path dirty?)
                               (get-in state [:buffers path])
                               (update-in [:buffers path] assoc :acked revision :acked-key content-key)))))

(defmethod handle-host-message :host/document [state {:keys [path text content-key epoch dirty?]}]
  (unchanged (settle-buffers
              (cond-> (with-dirty state path dirty?)
                (get-in state [:buffers path])
                (update-in [:buffers path] (fn [current]
                                             (assoc (editing/rebase current text)
                                                    :epoch epoch
                                                    :acked (:revision current)
                                                    :acked-key content-key)))))))

(defmethod handle-host-message :host/dirty [state {:keys [path dirty?]}]
  (unchanged (with-dirty state path dirty?)))

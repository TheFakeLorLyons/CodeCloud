(ns codecloud.presentation.ui-state
  "The cloud view's state and its changes. `handle` routes an event to its
  handler, which maps the state and the event's arguments to {:state
  :effects}, effects being data for the renderer."
  (:require [clojure.string :as string]
            [codecloud.languages.catalog :as catalog]
            [codecloud.model.restriction :as restriction]
            [codecloud.model.source :as source]
            [codecloud.presentation.arrangement :as arrangement]
            [codecloud.presentation.code-colors :as code-colors]
            [codecloud.presentation.definitions :as definitions]
            [codecloud.presentation.editing :as editing]
            [codecloud.presentation.forms :as forms]
            [codecloud.presentation.layers :as layers]
            [codecloud.presentation.page :as page]
            [codecloud.presentation.pairing :as pairing]
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
   :cloud-name nil
   :repl {:open? false
          :status :disconnected
          :message nil
          :port nil
          :ns nil
          :log []
          :entries-made 0
          :history []
          :requests-made 0
          :pending {}}
   :inline-results {}})

(defn- unchanged [state] {:state state :effects []})

(defn- changed [state & effects] {:state state :effects (vec (remove nil? effects))})

(defn- send-effect [message-type fields]
  [:send (protocol/message message-type fields)])

(defn- remember
  "The effect saving `value` as the viewer's `preference`."
  [preference value]
  (send-effect :view/save-preference {:key preference :value value}))

(defn- with-notice [state notice]
  (let [kept-notices 4]
    (update state :notices #(vec (take-last kept-notices (conj (vec %) notice))))))

;; Handler shapes. A handler takes the state and an event's arguments
;; and returns {:state :effects}.

(defn- quietly
  "A handler applying `update-state` to the state and the event's
  arguments, with no effects."
  [update-state]
  (fn [state & arguments]
    (unchanged (apply update-state state arguments))))

(defn- remembered
  "A handler setting the state at `path` to what `next-value` makes of the
  state and the event's arguments, and remembering it as `preference`."
  [preference path next-value]
  (fn [state & arguments]
    (let [value (apply next-value state arguments)]
      (changed (assoc-in state path value) (remember preference value)))))

(defn- remembered-toggle
  "A handler flipping the switch at `path` and remembering it as
  `preference`."
  [preference path]
  (remembered preference path #(not (get-in % path))))

(defn- panel-toggle
  "A handler opening or closing the panel at `open-key`, closing the panel
  at `other-key` and any menu."
  [open-key other-key]
  (quietly #(-> % (update open-key not) (assoc other-key false :menu nil))))

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
            (remember :view-filter view-filter)
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
  "Keys of the color layers turned on: the viewer's choice, or the
  snapshot's defaults while :color-layers is nil."
  [{:keys [color-layers snapshot layer-registry]}]
  (let [offered (set (map :key layer-registry))]
    (into #{} (filter offered) (or color-layers (layers/default-keys snapshot)))))

(defn colored-by-default?
  "True while the layers turned on are the defaults for the snapshot."
  [{:keys [active-layers default-layers]}]
  (= active-layers default-layers))

(defn shown-code-colors
  "The code colors pages show: the editor theme's while the switch is on
  and the host could read them, CodeCloud's own otherwise."
  [{:keys [settings editor-colors]}]
  (if (and (:editor-colors? settings) editor-colors)
    editor-colors
    code-colors/own))

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
  named views, the filter, and the header mode, then measured among
  themselves and colored. Hidden headers fold into their partners.
  Editing stops once its page leaves the cloud."
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

(defn- select-snapshot
  "Asks for a snapshot, showing it at once when the view already has it."
  [state snapshot-id]
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

(defn- without-results-on
  "The state without inline results on the file at `path`."
  [state path]
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
  [state {:keys [settings theme-kind sky-mode cloud-name editor-font display-mode view-filter
                 arrangement header-mode color-layers color-overrides spread totals-open? tint-clouds?
                 inspector-minimized?]}]
  (let [chosen-layers (when (sequential? color-layers) (vec (filter keyword? color-layers)))
        chosen-colors (if (map? color-overrides) color-overrides {})
        shown-filter (if view-filter (merge visibility/no-filter view-filter) (:view-filter state))
        shown-headers (if header-mode (pairing/header-mode header-mode) (:header-mode state))
        updated (cond-> (assoc state
                               :settings (or settings {})
                               :theme-kind (or theme-kind :dark)
                               :sky-mode (or sky-mode :auto)
                               :cloud-name cloud-name
                               :editor-font editor-font
                               :totals-open? (true? totals-open?)
                               :inspector-minimized? (true? inspector-minimized?)
                               :tint-clouds? (not (false? tint-clouds?))
                               :color-layers chosen-layers
                               :color-overrides chosen-colors
                               :view-filter shown-filter
                               :header-mode shown-headers)
                  display-mode (assoc :display-mode display-mode)
                  arrangement (assoc :arrangement (:key (arrangement/arrangement arrangement)))
                  spread (assoc :spread (bounded-spread spread)))]
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

(defn- take-snapshot
  "The state with a snapshot from the host, shown when it was the one
  asked for."
  [state {:keys [snapshot]}]
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
                              (results-still-matching checked path (editing/text-lines (or text "")))
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
    (let [[state source-effect] (request-sources (assoc state :focused-id namespace-id :menu nil) [namespace-id])]
      (changed state source-effect [:lift namespace-id]))))

(defn- fly-to-namespace
  "Sets everything down and flies to a page."
  [state namespace-id]
  (let [[settled lift-effects] (set-down-everything (stop-editing (assoc state :menu nil)))
        [state source-effect] (request-sources settled [namespace-id])]
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

(defn- visit-namespace
  "Shows a page, hidden or not, and lifts it."
  [state namespace-id]
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

(defn- click-empty
  "Closes what a click on the sky closes first, then sets down what is
  lifted."
  [state]
  (cond
    (:editing state) (unchanged (stop-editing state))
    (:menu state) (unchanged (assoc state :menu nil))
    (:color-chooser state) (unchanged (assoc state :color-chooser nil))
    :else (set-down-one state)))

(defn- escape
  "Closes the innermost open thing, then sets down what is lifted."
  [state]
  (cond
    (:menu state) (unchanged (assoc state :menu nil))
    (:color-chooser state) (unchanged (assoc state :color-chooser nil))
    (:editing state) (unchanged (stop-editing state))
    (get-in state [:search :open?]) (unchanged (assoc state :search (:search initial-state)))
    (:views-open? state) (unchanged (assoc state :views-open? false))
    (:arrange-open? state) (unchanged (assoc state :arrange-open? false))
    :else (set-down-one state)))

(defn- show-definition
  "Sets everything down and flies to a definition's line."
  [state {:keys [namespace-id] :as location}]
  (let [[settled lift-effects] (set-down-everything (assoc state :menu nil))
        [state source-effect] (request-sources settled [namespace-id])]
    (apply changed state (concat lift-effects [source-effect [:fly-to-line location]]))))

(defn- jump-to-name
  "Flies to the definition of a name written on a page, offers a menu at
  the pointer when the name could mean several, or says none is shown."
  [state {:keys [namespace-id name x y]}]
  (let [menu-limit 10
        found (definitions/choices (:visible-snapshot state) namespace-id name menu-limit)]
    (case (count found)
      0 (unchanged (with-notice (assoc state :menu nil) (str "No page shown defines " name ".")))
      1 (show-definition state (first found))
      (unchanged (assoc state :menu {:kind :definitions :name name :choices found :x x :y y})))))

(defn- frame-directory
  "Sets everything down and frames a folder."
  [state directory]
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
   :cycle-sky-mode (remembered :sky-mode [:sky-mode] #(sky/next-mode (:sky-mode %)))
   :toggle-display-mode (remembered :display-mode [:display-mode] #(if (= :names (:display-mode %)) :code :names))
   :toggle-editor-colors (remembered-toggle :editor-colors? [:settings :editor-colors?])
   :toggle-legend (quietly #(update % :legend-open? not))
   :toggle-inspector (remembered-toggle :inspector-minimized? [:inspector-minimized?])
   :dismiss-notice (quietly (fn [state notice-index]
                              (update state :notices
                                      (fn [notices]
                                        (vec (keep-indexed (fn [index notice] (when (not= index notice-index) notice))
                                                           notices))))))})

;; Arranging the cloud

(defn- set-arrangement
  "Switches to another arrangement, putting every page back."
  [state arrangement-key]
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

(defn- choose-layers
  "The state recolored with `chosen` layers on, nil for the defaults,
  remembering the choice."
  [state chosen]
  (changed (recolored (assoc state :color-layers chosen) false) (remember :color-layers chosen)))

(defn- toggle-color-layer
  "Turns one color layer on or off."
  [state layer-key]
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

(defn- choose-color
  "Recolors with `colors` chosen for `target`, or with the choice undone
  when `colors` is nil, remembering the choice."
  [state target colors]
  (let [chosen (with-chosen-color state target colors)]
    (changed chosen (remember :color-overrides (:color-overrides chosen)))))

(def ^:private set-spread
  (remembered :spread [:spread] (fn [_state spread] (bounded-spread spread))))

(def ^:private arrangement-handlers
  {:toggle-arrange-panel (panel-toggle :arrange-open? :views-open?)
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
  {:toggle-views-panel (panel-toggle :views-open? :arrange-open?)
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
  (let [[state source-effect] (request-sources (assoc state :search (:search initial-state)
                                                      :focused-id namespace-id
                                                      :menu nil)
                                               [namespace-id])]
    (changed state source-effect [:lift namespace-id])))

(defn- search-submit
  "Lifts the search result the arrows chose."
  [state]
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
  "The state after `edit` changed the buffer being edited. A change to
  the text goes to the host and clears the file's inline results."
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

(defn- acknowledge-edit
  "The state after the host applied edit `revision` of `path`."
  [state {:keys [path revision content-key dirty?]}]
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
               (results-still-matching path (editing/text-lines text)))
     (get-in state [:buffers path])
     (update-in [:buffers path] (fn [current]
                                  (assoc (editing/rebase current text)
                                         :epoch epoch
                                         :acked (:revision current)
                                         :acked-key content-key))))))

(def ^:private editing-handlers
  {:begin-editing begin-editing
   :edit edit
   :end-editing (quietly stop-editing)
   :save-file (fn [state]
                (if-let [path (get-in state [:editing :path])]
                  (changed state (send-effect :view/save {:path path}))
                  (unchanged state)))})

;; The REPL

(defn- with-log
  "The state with `text` of `kind` (:input, :out, :err, :value, or :info)
  added to the REPL's log for request `id`, joining the last entry when
  it is more output of the same kind for the same request. The log keeps
  its latest entries, each cut to its latest text."
  [state kind text id]
  (let [kept-entries 400
        longest-entry 20000
        clipped (fn [entry-text]
                  (if (> (count entry-text) longest-entry)
                    (subs entry-text (- (count entry-text) longest-entry))
                    entry-text))
        {:keys [log entries-made]} (:repl state)
        last-entry (peek log)]
    (if (and last-entry (contains? #{:out :err} kind) (= kind (:kind last-entry)) (= id (:id last-entry)))
      (assoc-in state [:repl :log (dec (count log))] (update last-entry :text #(clipped (str % text))))
      (let [kept (if (>= (count log) kept-entries) (subvec log (- (count log) (dec kept-entries))) log)]
        (update state :repl assoc
                :log (conj kept {:number entries-made :kind kind :text (clipped text) :id id})
                :entries-made (inc entries-made))))))

(defn- echoed
  "How code sent from a page shows in the REPL's log: its first line, cut
  short when long, marked when more follows."
  [code]
  (let [longest-echo 120
        [first-line & more] (string/split-lines (string/trim code))
        shown (if (> (count first-line) longest-echo) (str (subs first-line 0 longest-echo) "…") first-line)]
    (if more (str shown " …") shown)))

(defn- prompt
  "The REPL's prompt for namespace `namespace-name`, the session's own,
  user, when nil."
  [namespace-name]
  (str (or namespace-name "user") "=> "))

(defn- pruned-results
  "The inline results without all but the most recent."
  [results]
  (let [kept-results 24]
    (if (> (count results) kept-results)
      (into {} (take-last kept-results (sort-by (comp :order val) results)))
      results)))

(defn- remembered-input
  "The pane's input history with `code` as its latest entry."
  [history code]
  (let [kept-inputs 100]
    (vec (take-last kept-inputs (conj (filterv #(not= code %) history) code)))))

(defn- connecting
  "The state while the view waits to hear how connecting went."
  [state message]
  (update state :repl assoc :status :connecting :message message))

(defn- toggle-repl
  "Opens or closes the REPL pane. Opening it while disconnected connects
  to the server the project's port file names."
  [state]
  (let [opening? (not (get-in state [:repl :open?]))
        connect? (and opening? (= :disconnected (get-in state [:repl :status])))]
    (cond-> (changed (assoc-in state [:repl :open?] opening?))
      connect? (-> (update :state connecting "Looking for an nREPL server…")
                   (update :effects conj (send-effect :view/repl-connect {}))))))

(defn- port-target
  "The {:host :port} that text typed into the REPL pane names, as 7888 or
  localhost:7888, or nil."
  [text]
  (when-let [[_ host port] (re-matches #"(?:([^\s:]+):)?(\d{1,5})" text)]
    (cond-> {:port #?(:clj (Long/parseLong port) :cljs (js/parseInt port 10))}
      host (assoc :host host))))

(defn- repl-submit
  "Sends what the viewer typed into the REPL pane: a port to connect to
  while not connected, or else code to evaluate in the pane's namespace."
  [state text]
  (let [code (string/trim text)
        {:keys [status ns requests-made]} (:repl state)
        target (when (not= :connected status) (port-target code))]
    (cond
      (string/blank? code) (unchanged state)

      target
      (changed (connecting state (str "Connecting to port " (:port target) "…"))
               (send-effect :view/repl-connect target))

      :else
      (let [id (str "eval-" requests-made)]
        (changed (-> state
                     (update :repl assoc :requests-made (inc requests-made))
                     (update-in [:repl :history] remembered-input code)
                     (assoc-in [:repl :pending id] {:origin :pane :ns ns})
                     (with-log :input (str (prompt ns) code) id))
                 (send-effect :view/repl-eval (cond-> {:id id :code code} ns (assoc :ns ns))))))))

(defn- evaluation
  "What evaluating `scope` sends from `buffer`, or a notice saying why
  nothing can be sent: the selection or the form at the caret for :form,
  the top-level form around the caret for :top-level, or the whole file
  for :file, as {:code :start :end :namespace-name}."
  [{:keys [lines caret language] :as buffer} scope]
  (let [read-forms (delay (forms/read-forms lines))
        selection? (and (= :form scope) (editing/selected? buffer))
        form (when-not selection?
               (case scope
                 :form (forms/form-at @read-forms caret)
                 :top-level (forms/top-level-at @read-forms caret)
                 nil))]
    (cond
      (not= :clojure language)
      {:notice "Only Clojure files evaluate in the REPL."}

      selection?
      (let [[start end] (editing/ordered buffer)]
        {:code (editing/selected-text buffer) :start start :end end
         :namespace-name (forms/namespace-at @read-forms start)})

      (= :file scope)
      {:code (string/join "\n" lines) :start [0 0] :end [0 0]
       :namespace-name (forms/namespace-at @read-forms [0 0])}

      (nil? form) {:notice "Put the caret in or beside a form to evaluate it."}
      (not (:closed? form)) {:notice "The form at the caret is not closed."}

      :else
      {:code (forms/form-text lines form) :start (:start form) :end (:end form)
       :namespace-name (forms/namespace-at @read-forms (:start form))})))

(defn- evaluate
  "Sends the REPL the selection or the form at the caret (`scope` :form),
  the top-level form around the caret (:top-level), or the whole file
  being edited (:file), marking the line where the result will show: the
  last line of the code sent, or the first line of a loaded file."
  [state scope]
  (let [{:keys [path namespace-id]} (:editing state)
        buffer (get-in state [:buffers path])
        {:keys [notice code start end namespace-name]} (when buffer (evaluation buffer scope))]
    (cond
      (nil? buffer) (unchanged state)
      notice (unchanged (with-notice state notice))
      :else
      (let [number (get-in state [:repl :requests-made])
            id (str "eval-" number)
            [start-line start-column] start
            [end-line end-column] end
            ;; A selection of whole lines ends at the start of the next.
            result-line (if (and (zero? end-column) (> end-line start-line)) (dec end-line) end-line)
            line-text (nth (:lines buffer) result-line)
            result-key [path result-line]]
        (changed (-> state
                     (assoc-in [:repl :requests-made] (inc number))
                     (assoc-in [:repl :pending id] {:origin :page :result-key result-key :ns namespace-name})
                     (assoc-in [:inline-results result-key]
                               {:id id
                                :order number
                                :namespace-id namespace-id
                                :path path
                                :line result-line
                                :column (editing/visual-column line-text (count line-text) page/tab-size)
                                :line-text line-text
                                :loading? (= :file scope)
                                :status :pending})
                     (update :inline-results pruned-results)
                     (with-log :input (str (prompt namespace-name) (if (= :file scope) (str "Loading " path) (echoed code))) id))
                 (if (= :file scope)
                   (send-effect :view/repl-load-file {:id id :code code :path path})
                   (send-effect :view/repl-eval {:id id :code code :ns namespace-name :path path
                                                 :line (inc start-line) :column (inc start-column)})))))))

(defn- finished-result
  "An inline result once its evaluation ended: its value, or else what
  went wrong, read from `words`, the last reply's status, or from the
  last line of error output."
  [{:keys [status err] :as result} words]
  (if (= :value status)
    result
    (let [error-line (last (remove string/blank? (string/split-lines (or err ""))))]
      (assoc result
             :status :error
             :text (cond
                     (contains? words "interrupted") "Interrupted"
                     (contains? words "namespace-not-found") "Namespace not loaded"
                     (contains? words "not-connected") "Not connected"
                     (contains? words "connection-closed") "Connection closed"
                     error-line error-line
                     :else "No value")))))

(defn- answered-result
  "An inline result after one more reply to its evaluation. An error
  after a value outweighs it."
  [result {:keys [err value ex]} words]
  (let [longest-error 2000]
    (cond-> result
      value (assoc :status :value :text value)
      err (update :err #(let [joined (str % err)]
                          (subs joined (max 0 (- (count joined) longest-error)))))
      (or ex (contains? words "eval-error")) (assoc :status :error)
      (contains? words "done") (finished-result words))))

(defn- take-repl-status
  "The state with the REPL connection's status, noting each change in the
  log. A failed connection opens the pane, which says why."
  [state {:keys [status port message]}]
  (let [{previous-status :status previous-message :message} (:repl state)]
    (cond-> (update state :repl assoc :status status :port port :message message)
      (and message (not= [status message] [previous-status previous-message])) (with-log :info message nil)
      (= :failed status) (assoc-in [:repl :open?] true))))

(defn- take-repl-output
  "The state after one reply to a REPL request: its output in the log,
  its result on the page it came from, and, for the pane's own requests,
  the namespace the pane evaluates in."
  [state {:keys [id out err value ns] :as reply}]
  (let [{:keys [origin result-key] request-ns :ns} (get-in state [:repl :pending id])
        words (set (:status reply))]
    (cond-> state
      out (with-log :out out id)
      err (with-log :err err id)
      value (with-log :value value id)
      (and ns (= :pane origin)) (assoc-in [:repl :ns] ns)
      (contains? words "namespace-not-found")
      (with-log :err (str "The REPL has not loaded " (or request-ns "that namespace") " yet. Load its file first.") id)
      (contains? words "interrupted") (with-log :info "Interrupted." id)
      (and result-key (= id (get-in state [:inline-results result-key :id])))
      (update-in [:inline-results result-key] answered-result reply words)
      (contains? words "done") (update-in [:repl :pending] dissoc id))))

(def ^:private repl-handlers
  {:toggle-repl toggle-repl
   :repl-connect (fn [state]
                   (changed (connecting state "Looking for an nREPL server…") (send-effect :view/repl-connect {})))
   :repl-disconnect (fn [state] (changed state (send-effect :view/repl-disconnect {})))
   :repl-submit repl-submit
   :repl-interrupt (fn [state]
                     (if (seq (get-in state [:repl :pending]))
                       (changed state (send-effect :view/repl-interrupt {}))
                       (unchanged state)))
   :repl-clear (quietly #(assoc-in % [:repl :log] []))
   :evaluate evaluate})

;; Routing

(def ^:private message-handlers
  "Each host message's handler, by its :type. A handler takes the state
  and the message."
  {:host/init take-init
   :host/views take-views
   :host/snapshot take-snapshot
   :host/history (quietly (fn [state {:keys [commits available?]}]
                            (assoc state :commits (vec commits) :history-status (if available? :ready :unavailable))))
   :host/sources take-sources
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
   :host/repl-status (quietly take-repl-status)
   :host/repl-output (quietly take-repl-output)})

(def ^:private event-handlers
  "Each event's handler, by the event's first element. A host message
  arrives as [:host message] and routes again on its :type."
  (merge {:host (fn [state message]
                  (if-let [handler (get message-handlers (:type message))]
                    (handler state message)
                    (unchanged state)))}
         interaction-handlers
         arrangement-handlers
         filter-handlers
         search-handlers
         history-handlers
         editing-handlers
         repl-handlers))

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

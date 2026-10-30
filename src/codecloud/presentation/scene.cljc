(ns codecloud.presentation.scene
  "How pages, folder clouds, and edges look, decided from a snapshot and
  the interface state. The renderer makes no decisions about appearance."
  (:require [clojure.string :as string]
            [codecloud.languages.catalog :as catalog]
            [codecloud.model.source :as source]
            [codecloud.presentation.arrangement :as arrangement]
            [codecloud.presentation.page :as page]))

(defn label-parts
  "A name split into its shared prefix and last segment, or a path at its
  last slash, without `root-prefix`, which the title already names."
  ([node] (label-parts node nil))
  ([{full-name :name :as node} root-prefix]
   (let [separator (if (page/named-by-path? node) "/" ".")
         shown-name (if (and root-prefix
                             (string/starts-with? full-name root-prefix)
                             (> (count full-name) (count root-prefix)))
                      (subs full-name (count root-prefix))
                      full-name)
         last-index (string/last-index-of shown-name separator)]
     (if (and last-index (pos? last-index) (< last-index (dec (count shown-name))))
       {:prefix (subs shown-name 0 (inc last-index)) :leaf (subs shown-name (inc last-index))}
       {:prefix "" :leaf shown-name}))))

(defn root-prefix
  "The first name segment most pages share, with its dot, such as
  \"malli.\", or nil."
  [snapshot]
  (let [names (keep (fn [node] (when (= :namespace (:kind node)) (:name node)))
                    (vals (:namespaces snapshot)))
        first-segments (keep (fn [namespace-name]
                               (let [dot-index (string/index-of namespace-name ".")]
                                 (when (and dot-index (pos? dot-index)) (subs namespace-name 0 dot-index))))
                             names)
        [segment occurrences] (first (sort-by (comp - val) (frequencies first-segments)))
        least-namespaces 4
        majority 0.6]
    (when (and segment
               (>= (count names) least-namespaces)
               (>= occurrences (* majority (count names))))
      (str segment "."))))

(defn neighbor-ids
  "Ids of the namespaces directly required by or requiring `id`."
  [snapshot id]
  (let [node (get-in snapshot [:namespaces id])]
    (into #{} (concat (:requires node) (:required-by node)))))

(defn search-matches
  "Ids of namespaces whose name contains every word of `query`, ignoring
  case. An empty query matches nothing."
  [snapshot query]
  (let [words (remove string/blank? (string/split (string/lower-case (or query "")) #"\s+"))]
    (if (empty? words)
      #{}
      (into #{}
            (comp (filter (fn [[_id node]]
                            (let [lowered (string/lower-case (:name node))]
                              (every? #(string/includes? lowered %) words))))
                  (map key))
            (:namespaces snapshot)))))

(defn search-results
  "Ids of the namespaces among `candidate-ids` matching `query`, most
  important first, with names breaking ties."
  [snapshot query candidate-ids]
  (sort-by (fn [id]
             (let [{:keys [importance] namespace-name :name} (get-in snapshot [:namespaces id])]
               [(- (or importance 0)) namespace-name]))
           (filter candidate-ids (search-matches snapshot query))))

(defn- emphasis-for
  "How strongly a page is drawn, from pushed back to 1. A lifted page or
  folder dims everything unrelated to it, and hovering only hints."
  [id directory {:keys [focused-id focused-neighbors lifted-folder hovered-id hovered-neighbors matches]}]
  (let [pushed-back 0.3]
    (cond
      (seq matches) (if (contains? matches id) 1.0 pushed-back)
      focused-id (cond (= id focused-id) 1.0
                       (contains? focused-neighbors id) 0.9
                       (= directory lifted-folder) 0.8
                       :else pushed-back)
      lifted-folder (if (= directory lifted-folder) 1.0 pushed-back)
      hovered-id (cond (= id hovered-id) 1.0
                       (contains? hovered-neighbors id) 0.95
                       :else 0.75)
      :else 1.0)))

(defn- emphasis-context [snapshot {:keys [focused-id lifted-folder hovered-id search]}]
  {:focused-id focused-id
   :focused-neighbors (when focused-id (neighbor-ids snapshot focused-id))
   :lifted-folder lifted-folder
   :hovered-id hovered-id
   :hovered-neighbors (when hovered-id (neighbor-ids snapshot hovered-id))
   :matches (when (:open? search) (search-matches snapshot (:query search)))})

(defn cyclic?
  "True for a page in a dependency cycle its language refuses, drawn red."
  [node]
  (and (some? (:cycle-id node)) (not= :warning (:cycle-severity node))))

(defn looped?
  "True for a page in an import cycle its language allows, drawn with an
  amber edge."
  [node]
  (and (some? (:cycle-id node)) (= :warning (:cycle-severity node))))

(defn- color-spec
  "A page's color for the palette: the :blend of its layers, nil when none
  colors it, under the marks of cycles and parse errors."
  [{:keys [cyclic? cycle-warning? parse-error?]} blend]
  {:blend blend :cyclic? cyclic? :cycle-warning? cycle-warning? :parse-error? parse-error?})

(defn- ranks
  "Map of id to place, from 1, of the shown namespaces in a ranked
  arrangement, or nil for the others."
  [snapshot visible-ids arrangement-key]
  (when-let [metric (:metric (arrangement/arrangement arrangement-key))]
    (zipmap (arrangement/ranked-ids snapshot visible-ids metric) (iterate inc 1))))

(defn several-languages?
  "True when the snapshot's pages are in more than one language."
  [snapshot]
  (> (count (set (map :language (vals (:namespaces snapshot))))) 1))

(defn language-tag
  "The short tag a page's name band carries in a cloud of several
  languages, such as py or c++, or nil when `mixed?` is false."
  [mixed? node]
  (when mixed?
    (:tag (catalog/language (page/language-of node)))))

(defn page-bases
  "Id to the parts of a page's look that change only with the snapshot,
  filter, arrangement, or font: :geometry, :prefix, :leaf, :rank, :tag,
  and the cycle and parse marks."
  [snapshot arrangement-key visible-ids character-width]
  (let [shared-prefix (root-prefix snapshot)
        mixed? (several-languages? snapshot)
        rank-of (ranks snapshot visible-ids arrangement-key)]
    (into {}
          (keep (fn [[id node]]
                  (when (contains? visible-ids id)
                    [id (merge (label-parts node shared-prefix)
                               {:id id
                                :kind (:kind node)
                                :importance (:importance node)
                                :rank (get rank-of id)
                                :language (page/language-of node)
                                :tag (language-tag mixed? node)
                                :geometry (page/geometry (:files node) character-width)
                                :files (:files node)
                                :directory (page/directory node)
                                :cyclic? (cyclic? node)
                                :cycle-warning? (looped? node)
                                :parse-error? (boolean (:parse-error? node))})])))
          (:namespaces snapshot))))

(defn page-visuals
  "`bases` with the look of the moment: a :color-spec for the palette, the
  :folder-hue its cloud leans toward, :emphasis, :lifted?, and :dirty?
  when a file of the page has unsaved changes."
  [snapshot ui-state bases]
  (let [context (emphasis-context snapshot ui-state)
        {:keys [page-colors folder-hues focused-id lifted-folder dirty-paths]} ui-state]
    (persistent!
     (reduce-kv (fn [visuals id {:keys [directory files] :as base}]
                  (assoc! visuals id (assoc base
                                            :color-spec (color-spec base (get page-colors id))
                                            :folder-hue (get folder-hues id)
                                            :lifted? (or (= id focused-id) (= directory lifted-folder))
                                            :emphasis (emphasis-for id directory context)
                                            :dirty? (boolean (some #(contains? dirty-paths (:path %)) files)))))
                (transient {})
                bases))))

(defn shaped-by-buffers
  "`visuals` with each page whose files have buffers sized for the
  buffers' text, remembering the size the snapshot gave it as :base-size.
  The page being edited keeps its columns and is marked :edited?."
  [visuals {:keys [buffers editing snapshot]} character-width]
  (if (or (empty? buffers) (not= :working-tree (:id snapshot)))
    visuals
    (persistent!
     (reduce-kv (fn [result id {:keys [files geometry] :as visual}]
                  (if (some #(contains? buffers (:path %)) files)
                    (let [edited? (= id (:namespace-id editing))
                          sized-files (mapv (fn [{:keys [path] :as file}]
                                              (if-let [{:keys [lines]} (get buffers path)]
                                                (merge file (page/line-length-metrics lines))
                                                file))
                                            files)]
                      (assoc! result id (assoc visual
                                               :geometry (page/geometry sized-files character-width
                                                                        (when edited? (:columns editing)))
                                               :base-size [(:width geometry) (:height geometry)]
                                               :edited? edited?)))
                    result))
                (transient visuals)
                visuals))))

(defn directory-label
  "{:prefix :leaf} naming a folder, with the root folder named after the
  cloud itself."
  [directory cloud-name]
  (if (= "" directory)
    {:prefix "" :leaf (or cloud-name "top level")}
    (let [slash-index (string/last-index-of directory "/")]
      (if slash-index
        {:prefix (subs directory 0 (inc slash-index)) :leaf (subs directory (inc slash-index))}
        {:prefix "" :leaf directory}))))

(defn- mean-hue
  "Mean folder hue of pages, or nil without one. Folder hues never cross
  zero degrees, so a plain mean works."
  [members]
  (let [hues (keep :folder-hue members)]
    (when (seq hues)
      (/ (reduce + hues) (count hues)))))

(defn directory-visuals
  "Folder path to its cloud's look: :member-ids, :emphasis, :hue, :prefix,
  and :leaf."
  [visuals cloud-name]
  (into {}
        (map (fn [[directory members]]
               [directory (merge (directory-label directory cloud-name)
                                 {:path directory
                                  :member-ids (mapv :id (sort-by :id members))
                                  :emphasis (reduce max 0 (map :emphasis members))
                                  :hue (mean-hue members)})]))
        (group-by :directory (vals visuals))))

(def quiet-edge-kinds
  "Edge kinds that mark a name or a link with nothing loaded behind it,
  drawn fainter."
  #{:alias :implicit :link})

(defn edge-visuals
  "Edges between shown pages with :emphasis and a :role, :requires or
  :required-by, toward the lifted or hovered page. Ranked walls dim edges
  outside cycles; the language layer makes cross-language edges :heavy?."
  [snapshot {:keys [focused-id hovered-id search arrangement active-layers]} visible-ids]
  (let [anchor-id (or focused-id hovered-id)
        searching? (and (:open? search) (seq (search-matches snapshot (:query search))))
        wall-share (if (arrangement/ranked? arrangement) 0.35 1)
        by-language? (contains? active-layers :language)]
    (into []
          (comp (filter (fn [{:keys [from to]}]
                          (and (contains? visible-ids from) (contains? visible-ids to))))
                (map (fn [{:keys [from to cyclic? cross-language? kind] :as edge}]
                       (let [role (cond (nil? anchor-id) nil
                                        (= from anchor-id) :requires
                                        (= to anchor-id) :required-by
                                        :else nil)
                             heavy? (boolean (and by-language? cross-language?))
                             base-emphasis (cond cyclic? 1.0
                                                 heavy? wall-share
                                                 (contains? quiet-edge-kinds kind) (* wall-share 0.5)
                                                 :else (* wall-share 0.8))]
                         (assoc edge
                                :role role
                                :heavy? heavy?
                                :emphasis (cond
                                            searching? (* 0.4 base-emphasis)
                                            role 1.0
                                            focused-id (* 0.2 base-emphasis)
                                            hovered-id (* 0.6 base-emphasis)
                                            :else base-emphasis))))))
          (:edges snapshot))))

(defn layout-radius
  "Room a page claims in the layout. Pages always face the camera, so a
  page needs about half its diagonal on every side."
  [{:keys [radius]}]
  (let [spacing-share 0.9]
    (* spacing-share radius)))

(defn layout-nodes
  "Nodes for the layout simulation, each giving a page's id, the room it
  claims, its folder, and its folder's parent."
  [visuals]
  (mapv (fn [[id {:keys [geometry directory]}]]
          {:id id
           :radius (layout-radius geometry)
           :group directory
           :parent-group (source/directory-of directory)})
        (sort-by key visuals)))

(defn layout-links
  "Edges the layout pulls together between shown namespaces. Implicit
  references pull too, since they are real uses."
  [snapshot visible-ids]
  (into []
        (comp (filter (fn [{:keys [from to]}]
                        (and (contains? visible-ids from) (contains? visible-ids to))))
              (map #(select-keys % [:from :to])))
        (:edges snapshot)))

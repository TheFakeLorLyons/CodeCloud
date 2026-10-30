(ns codecloud.model.graph
  "The page graph: a node per unit, a namespace, script, or file, and an
  edge per internal dependency. Edge :kind is :load, :alias, :implicit,
  or the import's own kind; edges between language families are
  :cross-language?."
  (:require [clojure.string :as string]
            [codecloud.collections :as collections]
            [codecloud.languages.catalog :as catalog]
            [codecloud.model.restriction :as restriction]))

(defn node-id
  "The node a file belongs to: its unit, which for Clojure is the declared
  namespace or else the path."
  [facts]
  (or (:unit-id facts) (:namespace facts) (:path facts)))

(defn split-qualified-name
  "[namespace name] from \"ns/name\", split at the first slash, which keeps
  clojure.core// as the division function."
  [qualified-name]
  (let [slash-index (string/index-of qualified-name "/")]
    (when (and slash-index (pos? slash-index) (< slash-index (dec (count qualified-name))))
      [(subs qualified-name 0 slash-index) (subs qualified-name (inc slash-index))])))

(defn- merged-tallies [tally-maps]
  (or (apply merge-with + (remove nil? tally-maps)) {}))

(defn- merge-files
  "One node from every file of the same unit, in path order, except that
  the file the unit is named after comes last and gives the node its
  language, so a C header reads before its source."
  [id facts-list]
  (let [ordered-facts (sort-by (juxt #(= id (:path %)) :path) facts-list)
        primary (or (some #(when (= id (:path %)) %) ordered-facts) (first ordered-facts))
        namespace-name (some :namespace ordered-facts)
        definitions (collections/distinct-by
                     :name
                     (mapcat (fn [facts]
                               (map #(assoc % :path (:path facts)) (:definitions facts)))
                             ordered-facts))]
    {:id id
     :name (or namespace-name id)
     :language (:language primary :clojure)
     :kind (cond
             namespace-name :namespace
             (= :clojure (:language primary :clojure)) :script
             :else :module)
     :doc (some :doc ordered-facts)
     :files (mapv #(select-keys % [:path :language :dialect :line-count :longest-line :content-key :parse-error])
                  ordered-facts)
     :line-count (reduce + (map :line-count ordered-facts))
     :method-count (reduce + (map #(:method-count % 0) ordered-facts))
     :requirements (vec (distinct (mapcat :requirements ordered-facts)))
     :definitions definitions
     :references (merged-tallies (map :references ordered-facts))
     :local-references (merged-tallies (map :local-references ordered-facts))
     :unresolved (merged-tallies (map :unresolved ordered-facts))
     :resolved-references (apply merge-with (partial merge-with +) {} (keep :resolved-references ordered-facts))
     :hotspots (into []
                     (mapcat (fn [facts] (map #(assoc % :path (:path facts)) (:measured-forms facts))))
                     ordered-facts)
     :flag-counts (merged-tallies (map :flag-counts ordered-facts))}))

(defn- defined-names [node]
  (into #{} (map :name) (:definitions node)))

(defn- resolve-references
  "Target node id to {definition-name use-count} for one node. Names from
  :refer :all resolve against the referred namespaces."
  [node nodes-by-id names-by-id]
  (let [source-id (:id node)
        qualified (reduce-kv (fn [resolved qualified-name reference-count]
                               (if-let [[target-id definition-name] (split-qualified-name qualified-name)]
                                 (if (and (contains? nodes-by-id target-id) (not= target-id source-id))
                                   (update-in resolved [target-id definition-name] (fnil + 0) reference-count)
                                   resolved)
                                 resolved))
                             {}
                             (:references node))
        refer-all-targets (into []
                                (comp (filter :refer-all?)
                                      (map :target)
                                      (filter #(contains? nodes-by-id %))
                                      (remove #(= % source-id)))
                                (:requirements node))]
    (merge-with (partial merge-with +)
                (reduce-kv (fn [resolved unqualified-name reference-count]
                             (if-let [target-id (some #(when (contains? (get names-by-id %) unqualified-name) %)
                                                      refer-all-targets)]
                               (update-in resolved [target-id unqualified-name] (fnil + 0) reference-count)
                               resolved))
                           qualified
                           (:unresolved node))
                (into {}
                      (filter (fn [[target-id]] (and (contains? nodes-by-id target-id) (not= target-id source-id))))
                      (:resolved-references node)))))

(defn- edge-kind
  ":alias when every requirement is :as-alias, else the kind of the first
  loading requirement."
  [requirements]
  (if (every? :alias-only? requirements)
    :alias
    (or (some :kind (remove :alias-only? requirements)) :load)))

(defn- requirement-edges
  "Edges from a node's requirements. :load-platforms lists where the
  source waits for its target; :require-macros never forms a cycle."
  [node nodes-by-id]
  (let [by-target (group-by :target
                            (filter #(and (not (:js-module? %))
                                          (not (:external? %))
                                          (contains? nodes-by-id (:target %)))
                                    (:requirements node)))]
    (into []
          (comp (map (fn [[target-id requirements]]
                       {:from (:id node)
                        :to target-id
                        :kind (edge-kind requirements)
                        :load-platforms (into (sorted-set)
                                              (comp (remove :alias-only?)
                                                    (remove :macros?)
                                                    (keep :platform))
                                              requirements)}))
                (remove (fn [{:keys [from to load-platforms]}]
                          (and (= from to) (empty? load-platforms)))))
          by-target)))

(defn- graph-edges
  "Every edge of the graph, sorted by its ends, with its :reference-count
  and, when it has any, the :uses of each definition it reaches."
  [nodes-by-id resolved-by-id]
  (let [required-edges (into [] (mapcat #(requirement-edges % nodes-by-id)) (vals nodes-by-id))
        required-pairs (into #{} (map (juxt :from :to)) required-edges)
        implicit-edges (for [[source-id targets] resolved-by-id
                             target-id (keys targets)
                             :when (not (contains? required-pairs [source-id target-id]))]
                         {:from source-id :to target-id :kind :implicit :load-platforms #{}})]
    (->> (concat required-edges implicit-edges)
         (map (fn [{:keys [from to] :as edge}]
                (let [uses (get-in resolved-by-id [from to])]
                  (cond-> (assoc edge :reference-count (reduce + 0 (vals uses)))
                    (seq uses) (assoc :uses uses)))))
         (map (fn [{:keys [from to] :as edge}]
                (cond-> edge
                  (not= (catalog/language-family (get-in nodes-by-id [from :language]))
                        (catalog/language-family (get-in nodes-by-id [to :language])))
                  (assoc :cross-language? true))))
         (sort-by (juxt :from :to))
         vec)))

(defn- inbound-by-definition
  "Map from [target-id definition-name] to its uses from other namespaces."
  [resolved-by-id]
  (reduce (fn [tallies [target-id definition-name reference-count]]
            (update tallies [target-id definition-name] (fnil + 0) reference-count))
          {}
          (for [[_source-id targets] resolved-by-id
                [target-id counts] targets
                [definition-name reference-count] counts]
            [target-id definition-name reference-count])))

(defn- finish-node
  "`node` with what only the whole graph can tell: its neighbors, its
  outside requires, and the uses of it and of each of its definitions."
  [{:keys [id] :as node} {:keys [incoming outgoing inbound nodes-by-id]}]
  (let [from-elsewhere (remove #(= id (:from %)) incoming)
        count-uses (fn [definition]
                     (assoc definition
                            :inbound-count (get inbound [id (:name definition)] 0)
                            :local-count (get (:local-references node) (:name definition) 0)))]
    (-> node
        (assoc :requires (vec (sort (distinct (map :to outgoing))))
               :required-by (vec (sort (distinct (map :from from-elsewhere))))
               :external-requires (vec (into (sorted-set)
                                             (comp (map :target) (remove #(contains? nodes-by-id %)))
                                             (:requirements node)))
               :inbound-references (restriction/inbound-references id incoming))
        (update :definitions #(mapv count-uses %))
        (dissoc :references :local-references :unresolved :requirements :resolved-references))))

(defn build-graph
  "{:nodes {id node} :edges [edge]} for file facts, nodes gaining
  :requires, :required-by, :external-requires, and reference counts."
  [file-facts]
  (let [nodes-by-id (into {}
                          (map (fn [[id facts-list]] [id (merge-files id facts-list)]))
                          (group-by node-id file-facts))
        names-by-id (into {} (map (fn [[id node]] [id (defined-names node)])) nodes-by-id)
        resolved-by-id (into {}
                             (map (fn [[id node]] [id (resolve-references node nodes-by-id names-by-id)]))
                             nodes-by-id)
        edges (graph-edges nodes-by-id resolved-by-id)
        incoming-by-id (group-by :to edges)
        outgoing-by-id (group-by :from edges)
        inbound (inbound-by-definition resolved-by-id)]
    {:nodes (into {}
                  (map (fn [[id node]]
                         [id (finish-node node {:incoming (get incoming-by-id id)
                                                :outgoing (get outgoing-by-id id)
                                                :inbound inbound
                                                :nodes-by-id nodes-by-id})]))
                  nodes-by-id)
     :edges edges}))

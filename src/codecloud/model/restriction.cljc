(ns codecloud.model.restriction
  "A snapshot narrowed to the pages shown, measured among themselves. The
  view runs this whenever the shown pages change, so it stays free of the
  analysis code."
  (:require [codecloud.model.importance :as importance]))

(defn totals
  "Counts of a snapshot's pages by kind and language, files, lines,
  definitions, parse errors, cycles, and edges."
  [namespaces cycles edges]
  (let [nodes (vals namespaces)
        files (mapcat :files nodes)
        kind-count (fn [kind] (count (filter #(= kind (:kind %)) nodes)))]
    {:namespaces (kind-count :namespace)
     :scripts (kind-count :script)
     :modules (kind-count :module)
     :languages (into (sorted-map) (frequencies (map :language nodes)))
     :files (count files)
     :lines (reduce + 0 (map #(get-in % [:metrics :lines] 0) nodes))
     :definitions (reduce + 0 (map (comp count :definitions) nodes))
     :parse-errors (count (filter :parse-error files))
     :cycles (count (filter #(= :error (:severity %)) cycles))
     :cycle-warnings (count (filter #(= :warning (:severity %)) cycles))
     :edges (count edges)}))

(defn inbound-references
  "Uses of page `id` through the `incoming` edges from other pages, each
  use of one of its names counting, plus one for each page that loads it."
  [id incoming]
  (let [from-elsewhere (remove #(= id (:from %)) incoming)]
    (+ (reduce + 0 (map :reference-count from-elsewhere))
       (count (remove #(= :implicit (:kind %)) from-elsewhere)))))

(defn- restricted-node
  "`node` among the shown pages, its requires, uses, and measurements
  counted among them, with how many neighbors are hidden."
  [{:keys [id] :as node} {:keys [incoming outgoing members shown?]}]
  (let [shown-requires (vec (sort (distinct (map :to outgoing))))
        shown-required-by (vec (sort (distinct (keep #(when (not= id (:from %)) (:from %)) incoming))))
        member-ids (into #{} (map :id) members)
        hidden-count (fn [neighbor-ids]
                       (count (into #{} (remove #(or (contains? member-ids %) (shown? %))) neighbor-ids)))
        uses (apply merge-with + {} (keep :uses (remove #(= id (:from %)) incoming)))]
    (-> node
        (assoc :requires shown-requires
               :required-by shown-required-by
               :hidden-requires (hidden-count (mapcat :requires members))
               :hidden-required-by (hidden-count (mapcat :required-by members)))
        (update :metrics assoc
                :references (inbound-references id incoming)
                :dependents (count shown-required-by)
                :dependencies (count (remove #(= id %) shown-requires)))
        (update :definitions (fn [definitions]
                               (mapv #(assoc % :inbound-count (get uses (:name %) 0)) definitions))))))

(defn- folded-edge
  "One edge for `edges`, which join the same two pages once folded, taken
  from the first that loads its target, or else the first, and carrying
  the uses, references, and marks of all of them."
  [[first-edge :as edges]]
  (if (next edges)
    (cond-> (assoc (or (first (remove #(= :implicit (:kind %)) edges)) first-edge)
                   :reference-count (reduce + 0 (map #(:reference-count % 0) edges))
                   :load-platforms (into (sorted-set) (mapcat :load-platforms) edges))
      (some :uses edges) (assoc :uses (apply merge-with + (keep :uses edges)))
      (some :cross-language? edges) (assoc :cross-language? true)
      (some :cyclic? edges) (assoc :cyclic? true)
      (some :cycle-warning? edges) (assoc :cycle-warning? true))
    first-edge))

(defn- shown-edges
  "The edges between shown pages, each end in `stand-ins` moved to the
  page standing in for it. Edges that come to join the same two pages
  merge, and edges folded onto one page drop."
  [edges shown? stand-ins]
  (if (empty? stand-ins)
    (filterv (fn [{:keys [from to]}] (and (shown? from) (shown? to))) edges)
    (->> edges
         (keep (fn [{:keys [from to] :as edge}]
                 (let [from-page (get stand-ins from from)
                       to-page (get stand-ins to to)]
                   (when (and (shown? from-page) (shown? to-page) (or (= from to) (not= from-page to-page)))
                     (assoc edge :from from-page :to to-page)))))
         (group-by (juxt :from :to))
         (sort-by key)
         (mapv (comp folded-edge val)))))

(defn restrict
  "The snapshot as if only `visible-ids` existed, each page counting its
  hidden neighbors. Hidden pages `stand-ins` names fold into shown ones;
  cycles stay the whole codebase's."
  ([snapshot visible-ids] (restrict snapshot visible-ids {}))
  ([snapshot visible-ids stand-ins]
   (if (and (empty? stand-ins) (every? #(contains? visible-ids %) (keys (:namespaces snapshot))))
     snapshot
     (let [shown? #(contains? visible-ids %)
           placed #(get stand-ins % %)
           edges (shown-edges (:edges snapshot) shown? stand-ins)
           incoming (group-by :to edges)
           outgoing (group-by :from edges)
           members-of (group-by placed (keys (:namespaces snapshot)))
           restricted (into {}
                            (keep (fn [[id node]]
                                    (when (shown? id)
                                      [id (restricted-node node {:incoming (get incoming id)
                                                                 :outgoing (get outgoing id)
                                                                 :members (map #(get-in snapshot [:namespaces %])
                                                                               (get members-of id))
                                                                 :shown? (comp shown? placed)})])))
                            (:namespaces snapshot))
           importance-of (importance/scores (into {} (map (fn [[id node]] [id (:metrics node)])) restricted)
                                            (or (:weights snapshot) importance/default-weights))
           namespaces (into {}
                            (map (fn [[id node]] [id (assoc node :importance (get importance-of id 0))]))
                            restricted)
           cycles (filterv #(some (comp shown? placed) (:members %)) (:cycles snapshot))]
       (assoc snapshot
              :namespaces namespaces
              :edges edges
              :cycles cycles
              :totals (totals namespaces cycles edges))))))

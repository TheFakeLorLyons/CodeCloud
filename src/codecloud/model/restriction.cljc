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
  "Uses of page `id` through the `incoming` edges from other pages: each
  use of one of its names, plus one for each page that loads it."
  [id incoming]
  (let [from-elsewhere (remove #(= id (:from %)) incoming)]
    (+ (reduce + 0 (map :reference-count from-elsewhere))
       (count (remove #(= :implicit (:kind %)) from-elsewhere)))))

(defn- restricted-node
  "`node` among the visible pages: requires, dependents, uses, and
  measurements counted among them, with how many neighbors are hidden.
  `incoming` holds the visible edges that reach it."
  [{:keys [id requires required-by] :as node} visible? incoming]
  (let [shown-requires (filterv visible? requires)
        shown-required-by (filterv visible? required-by)
        uses (apply merge-with + {} (keep :uses (remove #(= id (:from %)) incoming)))]
    (-> node
        (assoc :requires shown-requires
               :required-by shown-required-by
               :hidden-requires (count (remove #(or (= id %) (visible? %)) requires))
               :hidden-required-by (- (count required-by) (count shown-required-by)))
        (update :metrics assoc
                :references (inbound-references id incoming)
                :dependents (count shown-required-by)
                :dependencies (count (remove #(= id %) shown-requires)))
        (update :definitions (fn [definitions]
                               (mapv #(assoc % :inbound-count (get uses (:name %) 0)) definitions))))))

(defn restrict
  "The snapshot as if only the pages in `visible-ids` existed: requires,
  uses, measurements, importance, and totals counted among them, and each
  page's :hidden-requires and :hidden-required-by. Cycles stay the whole
  codebase's, kept while any member shows. With nothing hidden, the
  snapshot itself."
  [snapshot visible-ids]
  (if (every? #(contains? visible-ids %) (keys (:namespaces snapshot)))
    snapshot
    (let [visible? #(contains? visible-ids %)
          edges (filterv (fn [{:keys [from to]}] (and (visible? from) (visible? to))) (:edges snapshot))
          incoming (group-by :to edges)
          restricted (into {}
                           (keep (fn [[id node]]
                                   (when (visible? id) [id (restricted-node node visible? (get incoming id))])))
                           (:namespaces snapshot))
          importance-of (importance/scores (into {} (map (fn [[id node]] [id (:metrics node)])) restricted)
                                           (or (:weights snapshot) importance/default-weights))
          namespaces (into {}
                           (map (fn [[id node]] [id (assoc node :importance (get importance-of id 0))]))
                           restricted)
          cycles (filterv #(some visible? (:members %)) (:cycles snapshot))]
      (assoc snapshot
             :namespaces namespaces
             :edges edges
             :cycles cycles
             :totals (totals namespaces cycles edges)))))

(ns codecloud.model.snapshot
  "A snapshot: one moment of a codebase, its measured pages with the edges
  and cycles between them."
  (:require [codecloud.languages :as languages]
            [codecloud.languages.catalog :as catalog]
            [codecloud.model.cycles :as cycles]
            [codecloud.model.graph :as graph]
            [codecloud.model.importance :as importance]
            [codecloud.model.metrics :as metrics]
            [codecloud.model.resolution :as resolution]
            [codecloud.model.restriction :as restriction]))

(defn- shortened-doc
  "A namespace docstring cut to the length the inspector shows."
  [doc]
  (let [longest-doc 600]
    (when doc
      (if (> (count doc) longest-doc) (str (subs doc 0 longest-doc) "…") doc))))

(defn- worst-hotspots
  "A namespace's most tangled definitions, worst first."
  [hotspots]
  (let [most-listed 12]
    (vec (take most-listed (sort-by (juxt (comp - :score) :path :line) hotspots)))))

(defn- platform-severity
  "How loud a cycle on `platform` is: Clojure refuses to load one, and
  other languages say in the catalog."
  [platform]
  (if (contains? catalog/clojure-platforms platform) :error (catalog/cycle-severity platform)))

(defn- with-severity
  "A cycle with its :severity, :error when any of its platforms refuses
  it and :warning otherwise."
  [found-cycle]
  (assoc found-cycle :severity (if (some #(= :error (platform-severity %)) (:platforms found-cycle)) :error :warning)))

(defn build-snapshot
  "Snapshot of file facts with :id, :label, :time, and optional :weights
  and :include-paths. Manifests guide resolution and never become pages."
  [all-facts {:keys [id label time weights include-paths]}]
  (let [manifests (filterv :manifest all-facts)
        file-facts (resolution/resolve-imports (languages/assign-units (filterv (complement :manifest) all-facts))
                                               manifests
                                               {:include-paths include-paths})
        {:keys [nodes edges]} (graph/build-graph file-facts)
        {found-cycles :cycles :keys [cycle-id-of]} (cycles/find-cycles (keys nodes) edges)
        cycles (mapv with-severity found-cycles)
        severity-of-cycle (into {} (map (juxt :id :severity)) cycles)
        weights (merge importance/default-weights weights)
        metrics-by-id (into {}
                            (map (fn [[node-id node]] [node-id (metrics/namespace-metrics node)]))
                            nodes)
        importance-of (importance/scores metrics-by-id weights)
        namespaces (into {}
                         (map (fn [[node-id node]]
                                [node-id (-> node
                                             (assoc :metrics (get metrics-by-id node-id)
                                                    :importance (get importance-of node-id 0)
                                                    :cycle-id (cycle-id-of node-id)
                                                    :parse-error? (boolean (some :parse-error (:files node))))
                                             (cond-> (cycle-id-of node-id)
                                               (assoc :cycle-severity (severity-of-cycle (cycle-id-of node-id))))
                                             (update :doc shortened-doc)
                                             (update :hotspots worst-hotspots)
                                             (dissoc :line-count :method-count :inbound-references))]))
                         nodes)
        cyclic? (cycles/cyclic-edge-predicate (filterv #(= :error (:severity %)) cycles))
        looped? (cycles/cyclic-edge-predicate (filterv #(= :warning (:severity %)) cycles))
        marked-edges (mapv (fn [edge]
                             (cond-> (assoc edge :cyclic? (cyclic? edge))
                               (looped? edge) (assoc :cycle-warning? true)))
                           edges)]
    {:id id
     :label label
     :time time
     :weights weights
     :namespaces namespaces
     :edges marked-edges
     :cycles cycles
     :totals (restriction/totals namespaces cycles marked-edges)}))

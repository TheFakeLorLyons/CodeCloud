(ns codecloud.model.metrics
  "Measurements of each page, which codecloud.model.importance weighs and
  color layers and arrangements may shade or rank by."
  (:require [codecloud.model.complection :as complection]
            [codecloud.model.parse :as parse]))

(def measurements
  "Each measurement of a graph node, by name. :references counts its uses
  from other namespaces plus one for each namespace requiring it."
  {:functions (fn [node]
                (+ (count (filter #(contains? parse/function-kinds (:kind %)) (:definitions node)))
                   (:method-count node 0)))
   :definitions (comp count :definitions)
   :lines (fn [node] (:line-count node 0))
   :references (fn [node] (:inbound-references node 0))
   :dependents (comp count :required-by)
   :dependencies (fn [node] (count (remove #(= % (:id node)) (:requires node))))
   :external-dependencies (comp count :external-requires)
   :complection (fn [node] (complection/rounded (reduce + 0 (map :score (:hotspots node)))))
   :flags (fn [node] (reduce + 0 (vals (:flag-counts node))))})

(defn namespace-metrics [node]
  (update-vals measurements (fn [measure] (measure node))))

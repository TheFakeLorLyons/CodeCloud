(ns codecloud.model.metrics
  "Measurements of each page, which codecloud.model.importance weighs."
  (:require [codecloud.model.complection :as complection]
            [codecloud.model.parse :as parse]))

(defn namespace-metrics
  "Measurements of one graph node. :references counts its uses from other
  namespaces plus one for each namespace requiring it."
  [node]
  {:functions (+ (count (filter #(contains? parse/function-kinds (:kind %))
                                (:definitions node)))
                 (:method-count node 0))
   :definitions (count (:definitions node))
   :lines (:line-count node 0)
   :references (:inbound-references node 0)
   :dependents (count (:required-by node))
   :dependencies (count (remove #(= % (:id node)) (:requires node)))
   :external-dependencies (count (:external-requires node))
   :complection (complection/rounded (reduce + 0 (map :score (:hotspots node))))
   :flags (reduce + 0 (vals (:flag-counts node)))})

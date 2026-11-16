(ns codecloud.agents.cloud-tools.path-between
  "The path_between tool, the chain of requires tying two pages."
  (:require [clojure.string :as string]
            [codecloud.agents.cloud-tools.common :as common]
            [codecloud.counting :refer [counted]]))

;; path_between

(defn- reached
  "Map from each page reachable from `start` along `neighbors`, itself
  included, to {:previous :steps} on a shortest chain from `start`."
  [neighbors start]
  (loop [frontier [start]
         known {start {:previous nil :steps 0}}
         steps 1]
    (if (empty? frontier)
      known
      (let [[next-frontier next-known]
            (reduce (fn [found id]
                      (reduce (fn [[ids seen] neighbor]
                                (if (contains? seen neighbor)
                                  [ids seen]
                                  [(conj ids neighbor) (assoc seen neighbor {:previous id :steps steps})]))
                              found
                              (neighbors id)))
                    [[] known]
                    frontier)]
        (recur next-frontier next-known (inc steps))))))

(defn- chain-to [known id]
  (loop [chain (list id)
         current id]
    (if-let [before (get-in known [current :previous])]
      (recur (conj chain before) before)
      (vec chain))))

(defn- chain-rows [edges chain]
  (for [[from to] (partition 2 1 chain)]
    (str "  " from " → " to " " (common/edge-note (get edges [from to])))))

(defn- steps-text [chain]
  (counted (dec (count chain)) "step"))

(defn- path-between [{:keys [snapshot] :as context} arguments]
  (let [from (common/find-page context (common/text-argument arguments "from"))
        to (common/find-page context (common/text-argument arguments "to"))]
    (cond
      (:error from) {:error (str "From: " (:error from))}
      (:error to) {:error (str "To: " (:error to))}
      (= (:id from) (:id to)) {:text (str (:id from) " is the page at both ends.")}
      :else
      (let [from-id (:id from)
            to-id (:id to)
            nodes (:namespaces snapshot)
            edges (common/edges-by-ends snapshot)
            requires-of (fn [id] (remove #{id} (get-in nodes [id :requires])))
            required-by-of (fn [id] (remove #{id} (get-in nodes [id :required-by])))
            forward (reached requires-of from-id)
            backward (reached requires-of to-id)
            nearest-shared (fn [first-known second-known]
                             (->> (keys first-known)
                                  (remove #{from-id to-id})
                                  (filter #(contains? second-known %))
                                  (sort-by (fn [id] [(+ (get-in first-known [id :steps]) (get-in second-known [id :steps])) id]))
                                  first))]
        (cond
          (contains? forward to-id)
          (let [chain (chain-to forward to-id)]
            {:text (string/join "\n" (cons (str from-id " reaches " to-id " in " (steps-text chain) ":") (chain-rows edges chain)))})

          (contains? backward from-id)
          (let [chain (chain-to backward from-id)]
            {:text (string/join "\n" (cons (str from-id " does not reach " to-id ", but " to-id " reaches " from-id " in "
                                                (steps-text chain) ":")
                                           (chain-rows edges chain)))})

          :else
          (if-let [shared (nearest-shared forward backward)]
            (let [first-chain (chain-to forward shared)
                  second-chain (chain-to backward shared)]
              {:text (string/join "\n" (concat [(str "Neither page reaches the other. Both reach " shared ":")]
                                               (chain-rows edges first-chain)
                                               (chain-rows edges second-chain)))})
            (let [from-dependents (reached required-by-of from-id)
                  to-dependents (reached required-by-of to-id)]
              (if-let [shared (nearest-shared from-dependents to-dependents)]
                {:text (string/join "\n" (concat [(str "Neither page reaches the other, and they share no requirement. "
                                                       shared " reaches both:")]
                                                 (chain-rows edges (vec (reverse (chain-to from-dependents shared))))
                                                 (chain-rows edges (vec (reverse (chain-to to-dependents shared))))))}
                {:text (str "Nothing ties " from-id " and " to-id ": neither reaches the other, they share no requirement, "
                            "and no page reaches both.")}))))))))

(def tool
  {:spec {"name" "path_between"
          "title" "How two pages are tied"
          "description" (str "The shortest chain of requires from one page to another, whichever way it runs, or else what "
                             "the two pages both depend on.")
          "inputSchema" {"type" "object"
                         "properties" {"from" (assoc common/page-property "description" "The page the chain starts at")
                                       "to" (assoc common/page-property "description" "The page the chain ends at")}
                         "required" ["from" "to"]}
          "annotations" common/read-only}
   :answer path-between
   :touched (fn [context arguments]
              (let [from (:id (common/find-page context (common/text-argument arguments "from")))
                    to (:id (common/find-page context (common/text-argument arguments "to")))]
                (when (and from to) [{:doing (str "tracing how " from " and " to " are tied")}])))})

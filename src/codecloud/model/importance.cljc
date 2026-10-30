(ns codecloud.model.importance
  "An importance score from 0 to 1 for each page, which gives the page
  strongest in functions, uses, and length a 1.")

(def default-weights
  "Relative influence of each measurement on importance."
  {:functions 1.0
   :references 1.0
   :lines 1.0})

(def weighted-metrics
  "Metric keys that importance combines, in display order."
  [:functions :references :lines])

(defn- log-scaled
  "A count on a log scale, which keeps one enormous file or heavily used
  namespace from flattening every other score."
  [value]
  (Math/log (inc value)))

(defn scores
  "Id to importance: the weighted mean of log-scaled metrics over their
  maxima, rescaled so the top scores 1. Negative weights count as zero."
  [metrics-by-id weights]
  (let [weight-of (fn [metric-key] (max 0 (get weights metric-key 0)))
        maxima (into {}
                     (map (fn [metric-key]
                            [metric-key (reduce max 0 (map #(log-scaled (get % metric-key 0))
                                                           (vals metrics-by-id)))]))
                     weighted-metrics)
        total-weight (reduce + (map weight-of weighted-metrics))
        raw-score (fn [metrics]
                    (if (pos? total-weight)
                      (/ (reduce + (map (fn [metric-key]
                                          (let [maximum (get maxima metric-key)]
                                            (if (pos? maximum)
                                              (* (weight-of metric-key)
                                                 (/ (log-scaled (get metrics metric-key 0)) maximum))
                                              0)))
                                        weighted-metrics))
                         total-weight)
                      0))
        raw-scores (into {} (map (fn [[id metrics]] [id (raw-score metrics)])) metrics-by-id)
        top-score (reduce max 0 (vals raw-scores))]
    (into {}
          (map (fn [[id score]] [id (if (pos? top-score) (/ score top-score) 0)]))
          raw-scores)))

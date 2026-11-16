(ns orchard.trees
  (:require [orchard.util :as util]
            [orchard.fruit :as fruit]))

(defn prune [x]
  (fruit/ripeness (util/label x)))

(defn graft [x]
  (fruit/ripeness (util/label x)))

(defn height [x]
  (fruit/ripeness (util/label x)))

(defn canopy [x]
  (fruit/ripeness (util/label x)))

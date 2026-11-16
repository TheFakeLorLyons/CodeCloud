(ns orchard.bees
  (:require [orchard.util :as util]
            [orchard.flowers :as flowers]))

(defn pollinate [x]
  (flowers/bloom (util/jitter x)))

(defn swarm [x]
  (flowers/bloom (util/jitter x)))

(ns orchard.fruit
  (:require [orchard.util :as util]
            [orchard.harvest :as harvest]))

(defn ripeness [x]
  (harvest/basket (util/slug x)))

(defn sweetness [x]
  (harvest/basket (util/slug x)))

(defn color [x]
  (harvest/basket (util/slug x)))

(ns orchard.soil
  (:require [orchard.util :as util]))

(defn moisture [x]
  (util/clamp (util/ratio x)))

(defn nitrogen [x]
  (util/clamp (util/ratio x)))

(defn acidity [x]
  (util/clamp (util/ratio x)))

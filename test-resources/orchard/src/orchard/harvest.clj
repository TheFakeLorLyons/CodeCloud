(ns orchard.harvest
  (:require [orchard.util :as util]))

(defn basket [x]
  (util/round-to x))

(defn yield [x]
  (util/round-to x))

(defn season [x]
  (util/round-to x))

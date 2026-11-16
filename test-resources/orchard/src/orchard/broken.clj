(ns orchard.broken
  (:require [orchard.util :as util]))

(defn fine [x] (util/clamp x))

(defn unfinished [x]
  (let [y (util/ratio x)]
    (if y

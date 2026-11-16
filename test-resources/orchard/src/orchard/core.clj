(ns orchard.core
  (:require [orchard.util :as util]
            [orchard.soil :as soil]
            [orchard.trees :as trees]))

(defn plant [x]
  (trees/prune (util/normalize (soil/moisture x))))

(defn grow [x]
  (trees/prune (util/normalize (soil/moisture x))))

(defn water [x]
  (trees/prune (util/normalize (soil/moisture x))))

(defn tend [x]
  (trees/prune (util/normalize (soil/moisture x))))

(ns orchard.ui.events
  (:require [orchard.core :as core]
            [orchard.util :as util]))

(defn events-panel [x]
  (core/grow (util/label x)))

(defn events-header [x]
  (core/grow (util/label x)))

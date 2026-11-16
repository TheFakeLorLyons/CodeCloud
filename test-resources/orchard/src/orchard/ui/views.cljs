(ns orchard.ui.views
  (:require [orchard.core :as core]
            [orchard.util :as util]))

(defn views-panel [x]
  (core/grow (util/label x)))

(defn views-header [x]
  (core/grow (util/label x)))

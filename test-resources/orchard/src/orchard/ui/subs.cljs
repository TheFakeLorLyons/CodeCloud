(ns orchard.ui.subs
  (:require [orchard.core :as core]
            [orchard.util :as util]))

(defn subs-panel [x]
  (core/grow (util/label x)))

(defn subs-header [x]
  (core/grow (util/label x)))

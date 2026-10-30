(ns app.core
  (:require ["../../web/js/plot.js" :as plot]
            [app.state :as state]))

(defn render! [canvas]
  (plot/plot canvas (:samples @state/db)))

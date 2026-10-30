(ns report
  (:require [babashka.process :refer [shell]]
            [clojure.string :as string]))

(defn summary [lines]
  (string/join "\n" (take 5 lines)))

(shell "bash bin/env.sh")
(println (summary ["done"]))

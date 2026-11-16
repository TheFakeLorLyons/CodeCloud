(ns orchard.trees-test
  (:require [clojure.test :refer [deftest is]]
            [orchard.trees :as subject]))

(deftest works (is (subject/prune 1)))

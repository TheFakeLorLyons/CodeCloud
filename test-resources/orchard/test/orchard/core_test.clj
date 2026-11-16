(ns orchard.core-test
  (:require [clojure.test :refer [deftest is]]
            [orchard.core :as subject]))

(deftest works (is (subject/plant 1)))

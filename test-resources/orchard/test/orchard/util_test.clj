(ns orchard.util-test
  (:require [clojure.test :refer [deftest is]]
            [orchard.util :as subject]))

(deftest works (is (subject/clamp 1)))

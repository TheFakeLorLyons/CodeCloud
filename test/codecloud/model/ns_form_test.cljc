(ns codecloud.model.ns-form-test
  (:require [clojure.test :refer [deftest is testing]]
            [codecloud.model.ns-form :as ns-form]))

(defn- requirement-of [declaration target]
  (some #(when (= target (:target %)) %) (:requirements declaration)))

(deftest reads-common-libspecs
  (let [declaration (ns-form/namespace-declaration
                     '(ns my.app.core
                        "The core."
                        (:require [clojure.string :as str]
                                  [my.app.util :as util :refer [helper other] :rename {other renamed}]
                                  my.app.plain
                                  [my.app.db :as-alias db]
                                  ["react" :as react]
                                  (my.app [left :as l] right))
                        (:import [java.util Date])))]
    (is (= "my.app.core" (:name declaration)))
    (is (= "The core." (:doc declaration)))
    (is (= "str" (:alias (requirement-of declaration "clojure.string"))))
    (is (= #{"helper" "other"} (:refers (requirement-of declaration "my.app.util"))))
    (is (= {"renamed" "other"} (:renames (requirement-of declaration "my.app.util"))))
    (is (some? (requirement-of declaration "my.app.plain")))
    (testing ":as-alias names a namespace without loading it"
      (is (true? (:alias-only? (requirement-of declaration "my.app.db")))))
    (testing "string requires are JavaScript modules"
      (is (true? (:js-module? (requirement-of declaration "react")))))
    (testing "prefix lists expand"
      (is (= "l" (:alias (requirement-of declaration "my.app.left"))))
      (is (some? (requirement-of declaration "my.app.right"))))
    (testing ":import is not a requirement"
      (is (nil? (requirement-of declaration "java.util"))))))

(deftest reads-use-and-macro-directives
  (let [declaration (ns-form/namespace-declaration
                     '(ns my.app.views
                        (:use my.app.everything
                              [my.app.some :only [a b]])
                        (:require-macros [my.app.macros :as macros :refer [m]])))]
    (is (true? (:refer-all? (requirement-of declaration "my.app.everything"))))
    (is (false? (:refer-all? (requirement-of declaration "my.app.some"))))
    (is (= #{"a" "b"} (:refers (requirement-of declaration "my.app.some"))))
    (is (true? (:macros? (requirement-of declaration "my.app.macros"))))))

(deftest reads-attribute-map-doc-and-flags
  (let [declaration (ns-form/namespace-declaration
                     '(ns my.app.flags
                        {:doc "From the attribute map."}
                        (:require [my.app.util] :reload)))]
    (is (= "From the attribute map." (:doc declaration)))
    (is (= ["my.app.util"] (map :target (:requirements declaration))))))

(deftest reads-in-ns-and-require-calls
  (is (= {:name "clojure.core" :doc nil :in-ns? true :requirements []}
         (ns-form/namespace-declaration '(in-ns (quote clojure.core)))))
  (is (nil? (ns-form/namespace-declaration '(defn x []))))
  (let [requirements (ns-form/require-call-requirements
                      '(require (quote [my.app.util :as util]) (quote my.app.other) :reload))]
    (is (= ["my.app.util" "my.app.other"] (map :target requirements)))
    (is (= "util" (:alias (first requirements)))))
  (testing "computed arguments cannot be read and are skipped"
    (is (= [] (ns-form/require-call-requirements '(require some-symbol))))))

(deftest alias-and-refer-tables
  (let [requirements (:requirements (ns-form/namespace-declaration
                                     '(ns a (:require [b.c :as c :refer [x y] :rename {y why}]))))]
    (is (= {"c" "b.c"} (ns-form/alias-table requirements)))
    (is (= {"x" "b.c/x" "why" "b.c/y"} (ns-form/refer-table requirements)))))

(ns codecloud.presentation.definitions-test
  (:require [clojure.test :refer [deftest is testing]]
            [codecloud.presentation.definitions :as definitions]))

(def ^:private snapshot
  {:namespaces {"main.cpp" {:files [{:path "main.cpp"}]
                            :definitions [{:name "smoothed" :line 3}]}
                "board.cpp" {:files [{:path "board.cpp"}]
                             :definitions [{:name "Board::read" :line 7} {:name "Board::read" :line 12}]}
                "board.hpp" {:files [{:path "board.hpp"}]
                             :definitions [{:name "Board" :line 4}]}
                "disk.cpp" {:files [{:path "disk.cpp"}]
                            :definitions [{:name "Disk::read" :line 9} {:name "smoothed" :line 2}]}
                "app.core" {:files [{:path "src/app/core.clj"} {:path "src/app/core.cljs"}]
                            :definitions [{:name "render!" :line 5 :path "src/app/core.cljs"}]}}
   :edges [{:from "main.cpp" :to "board.cpp" :kind :implicit :uses {"Board::read" 2}}
           {:from "main.cpp" :to "board.hpp" :kind :include :uses {"Board" 1}}]})

(defn- places [found]
  (mapv (juxt :namespace-id :line) found))

(deftest names-find-their-definitions
  (testing "a call reaches the definition on the page the caller uses, every overload of it"
    (is (= [["board.cpp" 7] ["board.cpp" 12]] (places (definitions/choices snapshot "main.cpp" "read" 10)))))
  (testing "the page's own definition comes first, and other pages are left out when it holds one"
    (is (= [["main.cpp" 3]] (places (definitions/choices snapshot "main.cpp" "smoothed" 10)))))
  (testing "a qualified name matches by its last part"
    (is (= [["board.hpp" 4]] (places (definitions/choices snapshot "main.cpp" "Board" 10)))))
  (testing "a name the page uses from nowhere falls back to every shown page"
    (is (= [["board.cpp" 7] ["board.cpp" 12] ["disk.cpp" 9]] (places (definitions/choices snapshot "board.hpp" "read" 10))))
    (is (= [["board.cpp" 7]] (places (definitions/choices snapshot "board.hpp" "read" 1))) "up to the limit"))
  (testing "a definition keeps its own file on a page of several"
    (is (= "src/app/core.cljs" (:path (first (definitions/choices snapshot "main.cpp" "app/render!" 10))))))
  (testing "a name defined nowhere finds nothing"
    (is (= [] (definitions/choices snapshot "main.cpp" "missing" 10)))))

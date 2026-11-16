(ns codecloud.presentation.pairing-test
  (:require [clojure.test :refer [deftest is testing]]
            [codecloud.model.restriction :as restriction]
            [codecloud.presentation.pairing :as pairing]
            [codecloud.presentation.scene :as scene]))

(defn- base
  "A page base of `width` by `height` for the file at `path` in
  `directory`, with the file's role and companion."
  [path directory role companion width height]
  {:id path
   :directory directory
   :files [(cond-> {:path path :file-role role} companion (assoc :companion companion))]
   :geometry {:width width :height height :radius (* 0.5 (Math/sqrt (+ (* width width) (* height height))))}})

(def ^:private sample-bases
  {"src/board.cpp" (base "src/board.cpp" "src" :source nil 60 80)
   "src/board.hpp" (base "src/board.hpp" "src" :header "src/board.cpp" 40 20)
   "src/board.tpp" (base "src/board.tpp" "src" :inline "src/board.cpp" 30 10)
   "src/filter.c" (base "src/filter.c" "src" :source nil 30 30)
   "include/filter.h" (base "include/filter.h" "include" :header "src/filter.c" 20 20)
   "include/ring.hpp" (base "include/ring.hpp" "include" :header nil 30 30)})

(def ^:private gap
  "The gap between the pages of a row, read from the row's width."
  (- (:width (pairing/row-geometry [{:width 1 :height 1} {:width 1 :height 1}])) 2))

(deftest rows-of-paired-pages
  (let [paired (pairing/paired sample-bases)]
    (testing "a header and an inline file sit with their source, declarations first"
      (is (= ["src/board.hpp" "src/board.tpp" "src/board.cpp"] (get-in paired ["src/board.cpp" :pair-members])))
      (is (= #{"src/board.cpp"} (set (keep :pair-root (vals paired))))))
    (testing "pages line up left to right with their top edges aligned"
      (let [row-width (+ 40 30 60 (* 2 gap))
            [header-x header-y] (get-in paired ["src/board.hpp" :pair-shift])
            [inline-x inline-y] (get-in paired ["src/board.tpp" :pair-shift])
            [source-x source-y] (get-in paired ["src/board.cpp" :pair-shift])]
        (is (= (+ (* -0.5 row-width) 20) header-x))
        (is (= (+ (* -0.5 row-width) 40 gap 15) inline-x))
        (is (= (- (* 0.5 row-width) 30) source-x))
        (is (= [30.0 35.0 0.0] (mapv double [header-y inline-y source-y])))))
    (testing "a header in another folder, or without a partner, stays in its own folder's cloud"
      (is (nil? (get-in paired ["include/filter.h" :pair-root])))
      (is (nil? (get-in paired ["include/ring.hpp" :pair-root]))))
    (testing "a header whose source is hidden stands alone"
      (is (nil? (get-in (pairing/paired (dissoc sample-bases "src/board.cpp")) ["src/board.hpp" :pair-root]))))
    (testing "the layout places a row as its root, sized for the whole row"
      (let [placed (pairing/layout-visuals paired)]
        (is (= #{"src/board.cpp" "src/filter.c" "include/filter.h" "include/ring.hpp"} (set (keys placed))))
        (is (= {:width (+ 40 30 60 (* 2 gap)) :height 80}
               (select-keys (get-in placed ["src/board.cpp" :geometry]) [:width :height])))))
    (testing "links run between rows, and none inside one"
      (is (= [{:from "src/board.cpp" :to "include/ring.hpp"}]
             (pairing/placed-links [{:from "src/board.hpp" :to "include/ring.hpp"}
                                    {:from "src/board.cpp" :to "src/board.hpp"}
                                    {:from "src/board.cpp" :to "include/ring.hpp"}]
                                   paired))))
    (is (= {"src/board.hpp" "src/board.cpp" "src/board.tpp" "src/board.cpp"} (pairing/pair-roots paired)))
    (testing "while an edit widens the header, the row keeps its corner and the source makes way"
      (let [widened (-> paired
                        (assoc-in ["src/board.hpp" :geometry :width] 50)
                        (assoc-in ["src/board.hpp" :base-size] [40 20])
                        pairing/reshaped)
            [header-x] (get-in paired ["src/board.hpp" :pair-shift])
            [widened-header-x] (get-in widened ["src/board.hpp" :pair-shift])
            [source-x] (get-in paired ["src/board.cpp" :pair-shift])
            [widened-source-x] (get-in widened ["src/board.cpp" :pair-shift])]
        (is (= (- header-x 20) (- widened-header-x 25)) "the header's left edge holds")
        (is (= (+ source-x 10) widened-source-x))))))

(def ^:private native-snapshot
  {:namespaces {"main.cpp" {:id "main.cpp" :files [{:path "main.cpp" :file-role :source}]
                            :requires ["board.cpp" "board.hpp"] :required-by []}
                "board.cpp" {:id "board.cpp" :files [{:path "board.cpp" :file-role :source}]
                             :definitions [{:name "Board::read"}]
                             :requires ["board.hpp"] :required-by ["main.cpp"]}
                "board.hpp" {:id "board.hpp" :files [{:path "board.hpp" :file-role :header :companion "board.cpp"}]
                             :definitions [{:name "Board"}]
                             :requires ["ring.hpp"] :required-by ["board.cpp" "main.cpp"]}
                "ring.hpp" {:id "ring.hpp" :files [{:path "ring.hpp" :file-role :header}]
                            :requires [] :required-by ["board.hpp"]}}
   :edges [{:from "board.cpp" :to "board.hpp" :kind :include :reference-count 0}
           {:from "board.hpp" :to "ring.hpp" :kind :include :reference-count 1 :uses {"Ring" 1}}
           {:from "main.cpp" :to "board.cpp" :kind :implicit :reference-count 2 :uses {"Board::read" 2}}
           {:from "main.cpp" :to "board.hpp" :kind :include :reference-count 1 :uses {"Board" 1}}]
   :cycles []})

(deftest one-line-per-dependency
  (testing "a use through a header draws as the line to the header, which is dashed"
    (is (= [["board.cpp" "board.hpp" true] ["board.hpp" "ring.hpp" true] ["main.cpp" "board.hpp" true]]
           (mapv (juxt :from :to (comp boolean :into-declaration?)) (scene/drawn-edges native-snapshot)))))
  (testing "hidden headers fold into their partners, a header without one staying"
    (let [folded (pairing/folding native-snapshot)
          shown-ids (reduce disj (set (keys (:namespaces native-snapshot))) (keys folded))
          restricted (restriction/restrict native-snapshot shown-ids folded)]
      (is (= {"board.hpp" "board.cpp"} folded))
      (is (= [["board.cpp" "ring.hpp" :include] ["main.cpp" "board.cpp" :include]]
             (mapv (juxt :from :to :kind) (:edges restricted))))
      (is (= {"Board::read" 2 "Board" 1} (:uses (second (:edges restricted)))))
      (is (= [["main.cpp" "board.cpp" false]] (mapv (juxt :from :to (comp boolean :into-declaration?))
                                                    (rest (scene/drawn-edges restricted))))
          "the folded line runs solid to the source")
      (is (= {:requires ["ring.hpp"] :required-by ["main.cpp"] :hidden-requires 0 :hidden-required-by 0}
             (select-keys (get-in restricted [:namespaces "board.cpp"])
                          [:requires :required-by :hidden-requires :hidden-required-by])))
      (is (= 2 (get-in restricted [:namespaces "board.cpp" :definitions 0 :inbound-count]))))))

(deftest header-modes
  (is (= :alone (pairing/header-mode :alone)))
  (is (= :beside (pairing/header-mode :sideways)) "an unknown mode falls back to beside")
  (is (pairing/pairs? :folders :beside))
  (is (pairing/pairs? :relations :beside))
  (is (not (pairing/pairs? :lines :beside)) "ranked walls rank each page alone")
  (is (not (pairing/pairs? :folders :alone)))
  (is (not (pairing/pairs? :folders :hidden)) "hidden headers have folded into their partners")
  (is (pairing/offers-header-modes? native-snapshot))
  (is (pairing/declaration? (get-in native-snapshot [:namespaces "ring.hpp"])))
  (is (not (pairing/declaration? (get-in native-snapshot [:namespaces "main.cpp"])))))

(ns codecloud.presentation.arrangement-test
  (:require [clojure.test :refer [deftest is testing]]
            [codecloud.presentation.arrangement :as arrangement]
            [codecloud.presentation.layout :as layout]))

(defn- distance [[from-x from-y from-z] [to-x to-y to-z]]
  (Math/sqrt (+ (Math/pow (- to-x from-x) 2) (Math/pow (- to-y from-y) 2) (Math/pow (- (or to-z 0) (or from-z 0)) 2))))

(deftest lays-ranked-pages-out-in-reading-order
  (let [sizes {"a" [30 40] "b" [20 20] "c" [25 10] "d" [10 30] "e" [40 25]}
        order ["e" "a" "c" "b" "d"]
        gap 2
        slots (arrangement/shelf-slots order sizes {:aspect 1.8 :gap gap})
        box (fn [id]
              (let [[x y] (get slots id)
                    [width height] (get sizes id)]
                {:left (- x (/ width 2)) :right (+ x (/ width 2)) :top (+ y (/ height 2)) :bottom (- y (/ height 2))}))]
    (is (= (set order) (set (keys slots))))
    (testing "no two pages overlap"
      (doseq [first-id order
              second-id order
              :when (neg? (compare first-id second-id))]
        (let [first-box (box first-id)
              second-box (box second-id)]
          (is (or (<= (:right first-box) (:left second-box)) (<= (:right second-box) (:left first-box))
                  (<= (:top first-box) (:bottom second-box)) (<= (:top second-box) (:bottom first-box)))
              (str first-id " and " second-id " overlap")))))
    (testing "reading order runs left to right, then down"
      (doseq [[earlier later] (partition 2 1 order)]
        (let [[earlier-x earlier-y] (get slots earlier)
              [later-x later-y] (get slots later)]
          (is (or (< earlier-x later-x) (> earlier-y later-y))))))))

(deftest ranks-by-a-measurement
  (let [snapshot {:namespaces {"a" {:name "a" :metrics {:lines 10}}
                               "b" {:name "b" :metrics {:lines 30}}
                               "c" {:name "c" :metrics {:lines 30}}}}]
    (is (= ["b" "c" "a"] (arrangement/ranked-ids snapshot ["a" "b" "c"] :lines))))
  (is (arrangement/ranked? :complection))
  (is (not (arrangement/ranked? :folders)))
  (is (= :folders (:key (arrangement/arrangement :no-such-thing)))))

(deftest gives-folders-room-of-their-own
  (let [page-geometry (fn [radius] {:radius radius :width radius :height radius})
        visuals (into {}
                      (for [[folder page-count] [["src/a" 6] ["src/b" 3] ["test/a" 4] ["dev" 1]]
                            index (range page-count)]
                        [(str folder "/" index) {:directory folder :geometry (page-geometry (+ 8 index))}]))
        edges [{:from "src/a/0" :to "src/b/1"} {:from "test/a/2" :to "src/a/3"}]
        plan (arrangement/folder-plan visuals edges :radius)]
    (is (= #{"src/a" "src/b" "test/a" "dev"} (set (keys plan))))
    (testing "folder clouds never overlap"
      (doseq [first-folder (keys plan)
              second-folder (keys plan)
              :when (neg? (compare first-folder second-folder))]
        (let [{first-center :center first-radius :radius} (get plan first-folder)
              {second-center :center second-radius :radius} (get plan second-folder)]
          (is (>= (distance first-center second-center) (* 0.98 (+ first-radius second-radius)))
              (str first-folder " and " second-folder " overlap")))))
    (testing "pages stay inside their folder once laid out"
      (let [request (arrangement/layout-request :folders {} visuals edges :radius)
            nodes (mapv (fn [[id {:keys [geometry directory]}]] {:id id :radius (:radius geometry) :group directory})
                        (sort-by key visuals))
            simulation (layout/settle! (layout/create-simulation nodes edges nil (:options request)) 900)]
        (doseq [[id {:keys [directory]}] visuals]
          (let [{:keys [center radius]} (get plan directory)]
            (is (<= (distance center (layout/position simulation id)) (* 1.1 radius))
                (str id " strayed from " directory))))))))

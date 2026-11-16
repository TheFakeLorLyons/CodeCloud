(ns codecloud.presentation.environments-test
  (:require [clojure.test :refer [deftest is testing]]
            [codecloud.presentation.environments :as environments]))

(def ^:private greenhouse
  {"name" "Greenhouse" "layout" "room" "light" "day" "folders" "discs"
   "walls" {"color" "#dbe9d3" "pattern" "grid" "patternColor" "#9dbb94" "patternScale" 2}
   "floor" {"color" "#6d5a45" "pattern" "checks"}
   "lamps" "#fff"})

(deftest colors-from-hex
  (is (= [1.0 1.0 1.0] (environments/hex-rgb "#ffffff") (environments/hex-rgb "#FFF") (environments/hex-rgb " fff ")))
  (is (= [0.0 (/ 128 255.0) 1.0] (environments/hex-rgb "#0080ff")))
  (is (nil? (environments/hex-rgb "#12345")))
  (is (nil? (environments/hex-rgb "blue")))
  (is (nil? (environments/hex-rgb 12))))

(deftest built-in-environments
  (testing "the first look is the lightest to draw"
    (is (= :simple environments/default-id))
    (is (= {:backdrop :plain :folders :discs :frames-per-second 30}
           (select-keys (environments/environment-of :simple []) [:backdrop :folders :frames-per-second]))))
  (testing "the skies keep their names and lights"
    (is (= [:theme :day :twilight :night :clock]
           (map (comp :light #(environments/environment-of % [])) [:auto :day :twilight :night :clock]))))
  (testing "the backrooms are a maze of rooms with day pages"
    (let [backrooms (environments/environment-of :backrooms [])]
      (is (= [:room :day :maze :wallpaper :carpet :tiles]
             [(:backdrop backrooms) (:light backrooms) (get-in backrooms [:space :layout])
              (get-in backrooms [:space :walls :pattern]) (get-in backrooms [:space :floor :pattern])
              (get-in backrooms [:space :ceiling :pattern])]))))
  (testing "every built-in pattern is one the room shader numbers"
    (is (every? (set environments/patterns)
                (for [{:keys [space]} environments/built-in
                      :when space
                      surface [:walls :floor :ceiling]]
                  (get-in space [surface :pattern]))))))

(deftest spaces-from-the-setting
  (let [[space] (environments/spaces-from-setting [greenhouse])]
    (testing "a described space keeps what it says"
      (is (= ["Greenhouse" :room :room :day :discs]
             [(:id space) (:backdrop space) (get-in space [:space :layout]) (:light space) (:folders space)]))
      (is (= {:color (environments/hex-rgb "#dbe9d3") :accent (environments/hex-rgb "#9dbb94") :pattern :grid :scale 2.0}
             (get-in space [:space :walls])))
      (is (= [1.0 1.0 1.0] (get-in space [:space :lamps]))))
    (testing "and takes the plain room's parts for what it leaves out"
      (is (= :checks (get-in space [:space :floor :pattern])))
      (is (some? (get-in space [:space :floor :accent])))
      (is (some? (get-in space [:space :ceiling :color])))
      (is (some? (get-in space [:space :haze])))))
  (testing "values a space cannot use fall back"
    (let [[space] (environments/spaces-from-setting [{"name" "Odd" "layout" "spiral" "light" "noon" "folders" "fog"
                                                       "walls" {"color" "teal" "pattern" "zigzag" "patternScale" -3}
                                                       "lamps" false}])]
      (is (= [:room :day :clouds] [(get-in space [:space :layout]) (:light space) (:folders space)]))
      (is (= [:plain 1.0] [(get-in space [:space :walls :pattern]) (get-in space [:space :walls :scale])]))
      (is (vector? (get-in space [:space :walls :color])))
      (is (nil? (get-in space [:space :lamps])))))
  (testing "pattern scales stay within bounds"
    (is (= 8.0 (get-in (first (environments/spaces-from-setting [{"name" "Big" "walls" {"patternScale" 40}}]))
                       [:space :walls :scale]))))
  (testing "names that are missing, built in, or used before leave their entries out"
    (is (= ["Greenhouse" "Attic"]
           (map :id (environments/spaces-from-setting [greenhouse {"name" ""} {"layout" "maze"} {"name" "Backrooms"}
                                                       {"name" "simple"} {"name" "greenhouse"} {"name" " Attic "}
                                                       "not a space"])))))
  (testing "a setting that is not a list describes no spaces"
    (is (= [] (environments/spaces-from-setting {"name" "Greenhouse"}) (environments/spaces-from-setting nil)))))

(deftest choosing-environments
  (let [spaces (environments/spaces-from-setting [greenhouse])]
    (testing "a setting's text names a built-in environment or a space"
      (is (= :night (environments/chosen-id "night" spaces)))
      (is (= "Greenhouse" (environments/chosen-id "Greenhouse" spaces)))
      (is (= :simple (environments/chosen-id "Cellar" spaces) (environments/chosen-id nil spaces))))
    (testing "an id no environment goes by shows the default"
      (is (= :simple (:id (environments/environment-of "Cellar" spaces))))
      (is (= "Greenhouse" (:id (environments/environment-of "Greenhouse" spaces)))))
    (testing "the sky button cycles through the built-in environments, then the spaces"
      (is (= [:auto :day :twilight :night :clock :backrooms "Greenhouse" :simple]
             (map #(environments/next-id % spaces) [:simple :auto :day :twilight :night :clock :backrooms "Greenhouse"])))
      (is (= :simple (environments/next-id :backrooms [])))
      (is (= :simple (environments/next-id "Gone" spaces))))))

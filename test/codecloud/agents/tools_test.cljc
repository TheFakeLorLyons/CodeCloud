(ns codecloud.agents.tools-test
  (:require [clojure.string :as string]
            [clojure.test :refer [deftest is testing]]
            [codecloud.agents.tools :as tools]))

(def ^:private around-a-line
  {"center" 4
   "clamped" false
   "bands" {"fine" {"range" [0 14] "content" "(ns app.core)\n\n(defn greet [name]"}
            "medium" {"range" [0 40] "content" "[0] (ns app.core)\n[2] (defn greet [name]"}
            "coarse" {"range" [0 80] "content" "[0-80] (ns app.core)"}}
   "waveletPeaks" [{"position" 0 "coefficient" 1.2 "scale" 2 "label" "ns"}
                   {"position" 2 "coefficient" 0.9 "scale" 2 "label" "defn greet"}]})

(deftest the-code-around-a-line-keeps-its-fine-band-alone
  (is (= {"center" 4
          "clamped" false
          "bands" {"fine" {"range" [0 14] "content" "(ns app.core)\n\n(defn greet [name]"}}}
         (tools/fine-band-only around-a-line)))
  (is (= {"positions" [] "truncated" false} (tools/fine-band-only {"positions" [] "truncated" false}))
      "an answer without bands comes back as it is"))

(deftest the-listed-tools-describe-the-bands-they-return
  (is (= tools/wavescope-tools (tools/listed-wavescope-tools :all)))
  (let [listed (tools/listed-wavescope-tools :fine)
        by-name (into {} (map (juxt #(get % "name") identity)) listed)
        originals (into {} (map (juxt #(get % "name") identity)) tools/wavescope-tools)]
    (is (= (map #(get % "name") tools/wavescope-tools) (map #(get % "name") listed)))
    (doseq [tool-name tools/band-tools]
      (testing tool-name
        (is (string/includes? (get-in by-name [tool-name "description"]) "exact lines"))
        (is (= (dissoc (get originals tool-name) "description") (dissoc (get by-name tool-name) "description"))
            "only the description changes")))
    (doseq [tool-name (remove tools/band-tools (keys originals))]
      (is (= (get originals tool-name) (get by-name tool-name)) tool-name))))

(deftest a-summary-lists-its-lines-in-order
  (testing "peaks listed strongest first, with gaps marked in that order, as wavescope gives them"
    (is (= (string/join "\n" ["[220-227] ..."
                              "[228] (defn plant [x]"
                              "[229-256] ..."
                              "[257] (defn water [x]"
                              "[258-279] ..."
                              "[280] (defn tend [x]"
                              "[281-298] ..."
                              "[299] (defn grow [x]"
                              "[300-321] ..."
                              "[322] (defn prune [x]"
                              "[323-330] ..."])
           (tools/summary-by-line (string/join "\n" ["[220-227] ..."
                                                     "[228] (defn plant [x]"
                                                     "[229-321] ..."
                                                     "[322] (defn prune [x]"
                                                     "[257] (defn water [x]"
                                                     "[258-298] ..."
                                                     "[299] (defn grow [x]"
                                                     "[280] (defn tend [x]"
                                                     "[281-330] ..."])))))
  (testing "a summary already in order, with a peak on each end of its range and one on an empty line"
    (let [ordered (string/join "\n" ["[0] (ns app.core)" "[1-3] ..." "[4] " "[5-6] ..." "[7] (defn greet [name]"])]
      (is (= ordered (tools/summary-by-line ordered)))))
  (testing "text without peaks comes back as it is"
    (is (= "[0-40] (code region)" (tools/summary-by-line "[0-40] (code region)")))
    (is (= "" (tools/summary-by-line "")))))

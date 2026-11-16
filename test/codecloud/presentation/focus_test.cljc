(ns codecloud.presentation.focus-test
  "Checks the report of what the person is looking at, which the view
  sends the host for agents."
  (:require [clojure.test :refer [deftest is testing]]
            [codecloud.presentation.focus :as focus]
            [codecloud.presentation.ui-state :as ui-state]))

(deftest the-report-holds-the-caret-and-the-view
  (let [state (-> ui-state/initial-state
                  (assoc :snapshot {:id :working-tree :label "Working tree"}
                         :focused-id "app.core"
                         :visible-ids #{"app.core" "app.db"}
                         :editing {:namespace-id "app.core" :path "src/app/core.clj"}
                         :buffers {"src/app/core.clj" {:lines ["(ns app.core)" "(defn run [] 1)"] :caret [1 6] :anchor [1 0]}}))
        report (focus/report state (map #(str "page-" %) (range 50)))]
    (is (= {:namespace-id "app.core" :path "src/app/core.clj" :line 1 :column 6 :anchor-line 1 :anchor-column 0}
           (:editing report)))
    (is (= [:working-tree "Working tree" "app.core" 2 :folders]
           ((juxt :snapshot-id :snapshot-label :lifted :visible-count :arrangement) report)))
    (is (= 40 (count (:in-view report))) "the pages on screen are listed up to a limit, the largest first")
    (is (= "page-0" (first (:in-view report)))))
  (testing "without a page being edited there is no caret"
    (is (nil? (:editing (focus/report (assoc ui-state/initial-state :snapshot {:id :working-tree}) []))))))

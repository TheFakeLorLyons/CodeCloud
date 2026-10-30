(ns codecloud.model.workspace-test
  (:require [clojure.test :refer [deftest is testing]]
            [codecloud.model.workspace :as workspace]))

(deftest finding-the-cloud-for-a-file
  (let [clouds ["c:/work/app/codecloud.cld" "c:/work/app/ui.cld" "c:/work/app/src/ui/codecloud.cld"
                "c:/work/other/codecloud.cld"]]
    (is (= "c:/work/app/src/ui/codecloud.cld"
           (workspace/nearest-cloud-file clouds "c:/work/app/src/ui/views.cljs" "codecloud.cld")))
    (testing "within one folder the default name wins"
      (is (= "c:/work/app/codecloud.cld"
             (workspace/nearest-cloud-file clouds "c:/work/app/src/core.clj" "codecloud.cld"))))
    (is (nil? (workspace/nearest-cloud-file clouds "c:/elsewhere/x.clj" "codecloud.cld")))
    (is (nil? (workspace/nearest-cloud-file clouds "c:/work/apple/x.clj" "codecloud.cld"))
        "a folder whose name merely starts the same does not enclose")))

(deftest choosing-a-home-for-a-new-cloud
  (is (= ["c:/w/app/src/x" "c:/w/app/src" "c:/w/app" "c:/w"]
         (workspace/ancestor-directories "c:/w" "c:/w/app/src/x/y.clj")))
  (is (= [] (workspace/ancestor-directories "c:/w" "d:/elsewhere/y.clj")))
  (is (= "c:/w/app" (workspace/cloud-home "c:/w" "c:/w/app/src/x/y.clj" #{"c:/w/app"})))
  (is (= "c:/w" (workspace/cloud-home "c:/w" "c:/w/app/src/x/y.clj" #{}))))

(deftest arrival-lines-and-names
  (is (= 40 (workspace/focus-line 40 20 60)))
  (is (= 40 (workspace/focus-line 300 20 60)) "an off-screen cursor gives way to the middle of the view")
  (is (= "app" (workspace/cloud-name "c:/w/app/codecloud.cld" "codecloud.cld")))
  (is (= "ui" (workspace/cloud-name "c:/w/app/ui.cld" "codecloud.cld"))))

(ns codecloud.io.theme-files-test
  (:require [cljs.test :refer [async deftest is]]
            [codecloud.io.task :as task]
            [codecloud.io.theme-files :as theme-files]))

(deftest reads-a-theme-through-its-includes
  (async done
    (-> (task/all [(theme-files/read-theme "test-resources/themes/night.json")
                   (theme-files/read-theme "test-resources/themes/split.json")
                   (theme-files/read-theme "test-resources/themes/missing.json")])
        (task/then (fn [[night split missing]]
                     (is (= {"editor.background" "#011627" "editor.foreground" "#d4d4d4"} (:colors night))
                         "the including file's colors win")
                     (is (= ["comment" "string" "keyword"] (mapv #(get % "scope") (:token-colors night)))
                         "included rules come first")
                     (is (= [{"scope" "comment" "settings" {"foreground" "#93a1a1" "fontStyle" "italic"}}]
                            (:token-colors split))
                         "rules may live in a file of their own")
                     (is (nil? missing))))
        (task/recover (fn [error] (is (nil? error))))
        (task/then (fn [_] (done))))))

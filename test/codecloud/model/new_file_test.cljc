(ns codecloud.model.new-file-test
  (:require [clojure.test :refer [deftest is testing]]
            [codecloud.model.new-file :as new-file]))

(def ^:private snapshot
  {:namespaces {"app.core" {:name "app.core" :files [{:path "src/app/core.clj"}]}
                "app.my-util" {:name "app.my-util" :files [{:path "src/app/my_util.cljc"}]}
                "app.core-test" {:name "app.core-test" :files [{:path "test/app/core_test.clj"}]}
                "lib.deep" {:name "lib.deep" :files [{:path "modules/lib/src/main/lib/deep.clj"}]}
                "web/page.js" {:name "web/page.js" :kind :file :files [{:path "web/page.js"}]}
                "pkg.mod" {:name "pkg.mod" :files [{:path "pkg/mod.py"}]}}})

(deftest names-stay-inside-the-cloud
  (is (nil? (new-file/name-problem "src/app/extra.clj")))
  (is (= "Name the file." (new-file/name-problem "  ")))
  (is (= "Give a path inside the cloud's folder." (new-file/name-problem "/etc/passwd")))
  (is (= "Give a path inside the cloud's folder." (new-file/name-problem "src/../../x.clj")))
  (is (= "Give a path inside the cloud's folder." (new-file/name-problem "C:\\x.clj")))
  (is (= "End the path with the file's name." (new-file/name-problem "src/app/"))))

(deftest source-roots-come-from-the-namespaces
  (is (= ["modules/lib/src/main/" "test/" "src/"] (new-file/source-roots snapshot)))
  (testing "files of other languages say nothing of Clojure's roots"
    (is (not-any? #{""} (new-file/source-roots snapshot)))))

(deftest new-namespaces-are-named-by-their-place
  (is (= "app.extra" (new-file/namespace-name snapshot "src/app/extra.clj")))
  (is (= "app.new-thing" (new-file/namespace-name snapshot "src/app/new_thing.cljs")))
  (is (= "app.extra-test" (new-file/namespace-name snapshot "test/app/extra_test.clj")))
  (is (= "lib.more" (new-file/namespace-name snapshot "modules/lib/src/main/lib/more.clj")))
  (testing "without a root that fits, a top src, test, or dev folder is one"
    (is (= "tools.build" (new-file/namespace-name {} "dev/tools/build.clj")))
    (is (= "scratch.echo-test" (new-file/namespace-name {} "scratch/echo_test.clj")))))

(deftest new-files-start-ready-to-type
  (is (= ["(ns app.extra)\n\n\n" 2 0] (new-file/starting-text snapshot "src/app/extra.clj")))
  (is (= ["" 0 0] (new-file/starting-text snapshot "notes/todo.md")))
  (is (= ["" 0 0] (new-file/starting-text snapshot "deps.edn"))))

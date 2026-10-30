(ns codecloud.model.ignore-test
  (:require [clojure.test :refer [deftest is testing]]
            [codecloud.model.ignore :as ignore]))

(defn- ignored-by? [patterns path]
  (ignore/ignored? [(ignore/rule-set "" patterns)] path))

(deftest basic-patterns
  (testing "a name without a slash matches at any depth"
    (is (ignored-by? ["*.log"] "debug.log"))
    (is (ignored-by? ["*.log"] "deep/down/debug.log"))
    (is (not (ignored-by? ["*.log"] "debug.log.clj"))))
  (testing "a trailing slash matches directories and everything inside"
    (is (ignored-by? ["out/"] "out/cljs/core.cljs"))
    (is (ignored-by? ["out/"] "sub/out/x.clj"))
    (is (not (ignored-by? ["out/"] "out"))))
  (testing "a leading or middle slash anchors to the base"
    (is (ignored-by? ["/target"] "target/classes/a.clj"))
    (is (not (ignored-by? ["/target"] "sub/target/a.clj")))
    (is (ignored-by? ["dev/scratch"] "dev/scratch/a.clj"))
    (is (not (ignored-by? ["dev/scratch"] "src/dev/scratch/a.clj"))))
  (testing "comments and blank lines are not rules"
    (is (nil? (ignore/compile-pattern "# a comment")))
    (is (nil? (ignore/compile-pattern "   ")))))

(deftest double-star-patterns
  (is (ignored-by? ["**/generated"] "generated/a.clj"))
  (is (ignored-by? ["**/generated"] "src/x/generated/a.clj"))
  (is (ignored-by? ["docs/**"] "docs/a/b/c.clj"))
  (is (not (ignored-by? ["docs/**"] "src/docs.clj")))
  (is (ignored-by? ["a/**/b.clj"] "a/b.clj"))
  (is (ignored-by? ["a/**/b.clj"] "a/x/y/b.clj"))
  (is (not (ignored-by? ["a/**/b.clj"] "c/a/b.clj"))))

(deftest negation-and-precedence
  (is (not (ignored-by? ["*.clj" "!keep.clj"] "src/keep.clj")))
  (is (ignored-by? ["*.clj" "!keep.clj"] "src/drop.clj"))
  (testing "the last matching rule wins"
    (is (ignored-by? ["!keep.clj" "*.clj"] "keep.clj")))
  (testing "a file inside an ignored directory cannot be re-included"
    (is (ignored-by? ["build/" "!build/keep.clj"] "build/keep.clj")))
  (testing "re-including a directory whose parent is only partly ignored works"
    (is (not (ignored-by? ["build/*" "!build/keep/"] "build/keep/a.clj")))
    (is (ignored-by? ["build/*" "!build/keep/"] "build/other/a.clj"))))

(deftest character-classes-and-escapes
  (is (ignored-by? ["file[0-9].clj"] "file3.clj"))
  (is (not (ignored-by? ["file[!0-9].clj"] "file3.clj")))
  (is (ignored-by? ["file[!0-9].clj"] "fileA.clj"))
  (is (ignored-by? ["\\#notes.clj"] "#notes.clj"))
  (is (ignored-by? ["what?.clj"] "whatX.clj"))
  (is (ignored-by? ["a.b"] "a.b"))
  (is (not (ignored-by? ["a.b"] "aXb")))
  (is (ignored-by? ["[unclosed"] "[unclosed")))

(deftest nested-rule-sets-apply-under-their-base
  (let [rule-sets [(ignore/rule-set "" ["*.tmp"])
                   (ignore/rule-set-from-text "modules/web" "dist/\n!important.tmp\n")]]
    (is (ignore/ignored? rule-sets "modules/web/dist/app.cljs"))
    (is (not (ignore/ignored? rule-sets "dist/app.cljs")))
    (is (not (ignore/ignored? rule-sets "modules/web/important.tmp")))
    (is (ignore/ignored? rule-sets "other.tmp"))
    (is (ignore/directory-ignored? rule-sets "modules/web/dist"))
    (is (= ["src/a.clj" "modules/web/src/b.cljs"]
           (remove #(ignore/ignored? rule-sets %) ["src/a.clj" "x.tmp" "modules/web/dist/c.cljs" "modules/web/src/b.cljs"])))))

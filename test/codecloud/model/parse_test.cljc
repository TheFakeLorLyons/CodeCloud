(ns codecloud.model.parse-test
  (:require [clojure.string :as string]
            [clojure.test :refer [deftest is testing]]
            [codecloud.model.parse :as parse]
            [codecloud.model.source :as source]))

(def core-source
  (string/join
   "\n"
   ["(ns my.app.core"
    "  \"Wires the app together.\""
    "  (:require [clojure.string :as str]"
    "            [my.app.util :as util :refer [helper]]"
    "            #?(:cljs [my.app.browser :as browser])))"
    ""
    "(defn start [config]"
    "  (util/prepare config)"
    "  (helper (str/trim \"x\"))"
    "  (util/prepare config))"
    ""
    "(defn- stop [] (start {}))"
    "(def ^:private retries 3)"
    "(def handler (fn [request] request))"
    "(defmacro with-app [& body] `(util/wrap ~@body))"
    "(defmulti render :kind)"
    "(defmethod render :circle [shape] shape)"
    "(defprotocol Shape (area [shape]) (perimeter [shape]))"
    "(defrecord Circle [radius])"
    "(comment (util/never-counted))"
    "(my.app.other/direct-call)"
    ""]))

(deftest analyzes-a-namespace
  (let [facts (parse/analyze-file "src/my/app/core.clj" core-source)
        definition-named (fn [definition-name]
                           (some #(when (= definition-name (:name %)) %) (:definitions facts)))]
    (is (= "my.app.core" (:namespace facts)))
    (is (= "Wires the app together." (:doc facts)))
    (is (= 21 (:line-count facts)))
    (is (nil? (:parse-error facts)))
    (testing "the .clj file reads only the :clj side of reader conditionals"
      (is (= #{"clojure.string" "my.app.util"} (set (map :target (:requirements facts))))))
    (testing "definition kinds"
      (is (= :function (:kind (definition-named "start"))))
      (is (true? (:private? (definition-named "stop"))))
      (is (= :value (:kind (definition-named "retries"))))
      (is (true? (:private? (definition-named "retries"))))
      (is (= :function (:kind (definition-named "handler"))))
      (is (= :macro (:kind (definition-named "with-app"))))
      (is (= :multimethod (:kind (definition-named "render"))))
      (is (= :protocol (:kind (definition-named "Shape"))))
      (is (= :protocol-method (:kind (definition-named "area"))))
      (is (= :type (:kind (definition-named "Circle"))))
      (is (= 1 (:method-count facts))))
    (testing "definition lines"
      (is (= 7 (:line (definition-named "start"))))
      (is (= 10 (:end-line (definition-named "start")))))
    (testing "references resolve through aliases, refers, and syntax-quote"
      (is (= 2 (get-in facts [:references "my.app.util/prepare"])))
      (is (= 1 (get-in facts [:references "my.app.util/helper"])))
      (is (= 1 (get-in facts [:references "my.app.util/wrap"])))
      (is (= 1 (get-in facts [:references "clojure.string/trim"])))
      (is (= 1 (get-in facts [:references "my.app.other/direct-call"]))))
    (testing "rich comment blocks do not count"
      (is (nil? (get-in facts [:references "my.app.util/never-counted"]))))
    (testing "local references count uses of the file's own definitions"
      (is (= 1 (get-in facts [:local-references "start"]))))
    (testing "top-level forms keep editor spans"
      (is (= {:line 7 :column 1 :end-line 10 :end-column 25 :head "defn" :name "start"}
             (second (:top-level-forms facts)))))))

(deftest cljc-files-read-both-platforms
  (let [facts (parse/analyze-file "src/my/app/core.cljc" core-source)]
    (is (= #{"clojure.string" "my.app.util" "my.app.browser"}
           (set (map :target (:requirements facts)))))
    (testing "shared code is not counted twice"
      (is (= 2 (get-in facts [:references "my.app.util/prepare"]))))))

(deftest tolerates-what-editors-tolerate
  (testing "byte order mark, CRLF endings, and a shebang"
    (let [facts (parse/analyze-file "script/run.bb"
                                    (str "﻿#!/usr/bin/env bb\r\n(ns run.script\r\n  (:require [babashka.fs :as fs]))\r\n(defn go [] (fs/exists? \".\"))\r\n"))]
      (is (= "run.script" (:namespace facts)))
      (is (= 4 (:line-count facts)))
      (is (= 1 (get-in facts [:references "babashka.fs/exists?"])))))
  (testing "Java-only regex syntax and unknown tags"
    (let [facts (parse/analyze-file "src/a.clj" "(ns a) (def pattern #\"(?i)\\Qx\\E++\") (def stamp #my/tag {:x 1}) ::unknown/kw")]
      (is (nil? (:parse-error facts)))
      (is (= #{"pattern" "stamp"} (set (map :name (:definitions facts))))))))

(deftest keeps-facts-read-before-a-syntax-error
  (let [facts (parse/analyze-file "src/broken.clj" "(ns broken (:require [ok.lib :as ok]))\n(defn fine [] (ok/x))\n(defn broken [] (let [a 1]\n")]
    (is (= "broken" (:namespace facts)))
    (is (= ["fine"] (map :name (:definitions facts))))
    (is (some? (:parse-error facts)))
    (is (string? (get-in facts [:parse-error :message])))))

(deftest scripts-without-namespaces
  (let [facts (parse/analyze-file "build.clj" "(require '[my.app.core :as core])\n(core/start {})\n")]
    (is (nil? (:namespace facts)))
    (is (= ["my.app.core"] (map :target (:requirements facts))))
    (is (= 1 (get-in facts [:references "my.app.core/start"])))))

(deftest refer-all-names-are-kept-for-the-graph
  (let [facts (parse/analyze-file "src/a.clj" "(ns a (:use b))\n(defn x [] (thing) (thing) (x))\n")]
    (is (= {"thing" 2} (:unresolved facts)))
    (is (= {"x" 1} (:local-references facts)))))

(deftest source-helpers
  (is (= "a\nb\nc" (source/normalize-text "﻿a\r\nb\rc")))
  (is (= ["a" "b"] (source/split-lines "a\nb\n")))
  (is (= ["" ""] (source/split-lines "\n\n")))
  (is (= [] (source/split-lines "")))
  (is (= 3 (source/line-count "a\nb\nc")))
  (is (= "cljs" (source/file-extension "src/a/b.CLJS")))
  (is (nil? (source/file-extension "src/.gitignore")))
  (is (nil? (source/file-extension "Makefile"))))

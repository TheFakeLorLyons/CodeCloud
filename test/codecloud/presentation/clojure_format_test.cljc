(ns codecloud.presentation.clojure-format-test
  (:require [clojure.string :as string]
            [clojure.test :refer [deftest is testing]]
            [codecloud.presentation.clojure-format :as clojure-format]))

(defn- reindented
  "`text` with every line indented as cljfmt indents it."
  [text]
  (let [lines (string/split text #"\n" -1)]
    (string/join "\n" (clojure-format/reindent lines 0 0 (dec (count lines))))))

(deftest bodies-indent-two-spaces
  (is (= "(defn f [x]\n  (inc x))" (reindented "(defn f [x]\n(inc x))")))
  (is (= "(fn [x]\n  x)" (reindented "(fn [x]\n     x)")))
  (is (= "(if x\n  y\n  z)" (reindented "(if x\ny\nz)")))
  (is (= "(cond\n  a b\n  c d)" (reindented "(cond\na b\nc d)")))
  (is (= "(comment\n  (foo))" (reindented "(comment\n(foo))")))
  (is (= "(defprotocol P\n  (foo [this]\n    \"doc\"))" (reindented "(defprotocol P\n(foo [this]\n\"doc\"))"))))

(deftest bindings-and-arguments-line-up
  (is (= "(let [a 1\n      b 2]\n  (+ a b))" (reindented "(let [a 1\nb 2]\n(+ a b))")))
  (is (= "(ns foo\n  (:require [a.b :as b]\n            [c.d :as d]))" (reindented "(ns foo\n(:require [a.b :as b]\n[c.d :as d]))")))
  (is (= "(foo bar\n     baz)" (reindented "(foo bar\nbaz)")))
  (is (= "(foo\n bar)" (reindented "(foo\n    bar)")))
  (is (= "(-> x\n    (f)\n    (g))" (reindented "(-> x\n(f)\n(g))")))
  (is (= "#(foo\n  %)" (reindented "#(foo\n%)"))))

(deftest collections-line-up-inside-the-bracket
  (is (= "{:a 1\n :b 2}" (reindented "{:a 1\n:b 2}")))
  (is (= "[1\n 2]" (reindented "[1\n   2]")))
  (is (= "#{1\n  2}" (reindented "#{1\n2}"))))

(deftest strings-keep-their-lines
  (is (= "(def s \"a\n   b\")" (reindented "(def s \"a\n   b\")")))
  (is (= "(defn f\n  \"Doc\n  more.\"\n  [])" (reindented "(defn f\n\"Doc\n  more.\"\n[])"))))

(deftest trailing-spaces-go
  (is (= "(defn f []\n\n  1)" (reindented "(defn f []   \n   \n1)"))))

(deftest new-lines-begin-where-cljfmt-puts-them
  (is (= 2 (clojure-format/line-indentation ["(defn f [x]" ""] 1)))
  (is (= 6 (clojure-format/line-indentation ["(let [a 1" ""] 1)))
  (is (= 5 (clojure-format/line-indentation ["(foo bar" ""] 1)))
  (is (= 0 (clojure-format/line-indentation ["(def a 1)" ""] 1)))
  (is (nil? (clojure-format/line-indentation ["(def s \"abc" ""] 1)))
  (testing "a form begun at the first column of a line above starts the reading"
    (is (= 2 (clojure-format/line-indentation ["(def a 1)" "(defn g []" ""] 2)))))

(deftest the-caret-stays-on-its-code
  (testing "after code on its line, just after the same character"
    (is (= [1 6] (clojure-format/caret-after-change ["(defn f [x]" "(inc x))"] ["(defn f [x]" "  (inc x))"] [1 4])))
    (is (= [0 5] (clojure-format/caret-after-change ["(foo)   " "(bar)"] ["(foo)" "(bar)"] [0 8]))))
  (testing "in a line's indentation, just before its first character"
    (is (= [1 2] (clojure-format/caret-after-change ["(defn f [x]" "      (inc x))"] ["(defn f [x]" "  (inc x))"] [1 3]))))
  (testing "on a blank line, on the same blank line"
    (is (= [1 0] (clojure-format/caret-after-change ["(defn f [x]" "   " "  x)"] ["(defn f [x]" "" "  x)"] [1 3])))))

(deftest top-level-forms-are-found-by-their-brackets
  (let [lines ["(defn f [x]" "(let [a 1" "b 2]" "(+ a b)))" "(def y 1)"]]
    (is (= [0 3] (clojure-format/top-level-lines lines 2 2)))
    (is (= [0 4] (clojure-format/top-level-lines lines 3 4)))
    (is (= [4 4] (clojure-format/top-level-lines lines 4 4))))
  (is (= [1 1] (clojure-format/top-level-lines ["(def a 1)" "" "(def b 2)"] 1 1)))
  (testing "with a bracket left open, a line opening a form at its first column begins one"
    (is (= [1 2] (clojure-format/top-level-lines ["(defn f [x]" "(let [a 1" "b 2]" "(+ a b))" "(def y 1)"] 2 2)))))

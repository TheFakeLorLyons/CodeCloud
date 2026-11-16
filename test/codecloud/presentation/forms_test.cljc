(ns codecloud.presentation.forms-test
  (:require [clojure.test :refer [deftest is testing]]
            [codecloud.presentation.forms :as forms]))

(defn- text-at
  "The text of the form `locate` finds at `position` in `lines`, or nil."
  [locate lines position]
  (some->> (locate (forms/read-forms lines) position) (forms/form-text lines)))

(def ^:private sample
  ["(ns app.core"
   "  (:require [clojure.string :as string]))"
   ""
   "(defn add [a b]"
   "  (+ a b))   "
   ""
   "(comment"
   "  (add 1 2)"
   "  (add 3 4))"])

(deftest the-form-at-the-caret
  (testing "the form just closed before the caret"
    (is (= "(+ a b)" (text-at forms/form-at sample [4 9])))
    (is (= "(defn add [a b]\n  (+ a b))" (text-at forms/form-at sample [4 10]))))
  (testing "a symbol the caret is in or beside"
    (is (= "b" (text-at forms/form-at sample [4 7])))
    (is (= "b" (text-at forms/form-at sample [4 8])) "the caret right after b, before the closing bracket"))
  (testing "between elements, the collection holding the caret"
    (is (= "(defn add [a b]\n  (+ a b))" (text-at forms/form-at sample [4 1]))))
  (testing "nothing away from every form"
    (is (nil? (text-at forms/form-at sample [2 0])))
    (is (nil? (text-at forms/form-at sample [4 12])))))

(deftest the-top-level-form
  (is (= "(defn add [a b]\n  (+ a b))" (text-at forms/top-level-at sample [4 7])))
  (is (= "(defn add [a b]\n  (+ a b))" (text-at forms/top-level-at sample [4 12]))
      "past a form's end on its line, that form")
  (testing "inside a rich comment, the form at the caret within it"
    (is (= "(add 3 4)" (text-at forms/top-level-at sample [8 5])))
    (is (= "(add 1 2)" (text-at forms/top-level-at sample [7 11])))
    (is (= "(comment\n  (add 1 2)\n  (add 3 4))" (text-at forms/top-level-at sample [7 0]))
        "between the forms of a comment, the comment itself"))
  (is (nil? (text-at forms/top-level-at sample [5 0]))))

(deftest reader-prefixes-belong-to-their-forms
  (let [lines ["#?(:clj 1 :cljs 2) '(1 2) @state #_(skipped) ^:private secret #inst \"2020\""]]
    (is (= "#?(:clj 1 :cljs 2)" (text-at forms/form-at lines [0 18])))
    (is (= "'(1 2)" (text-at forms/form-at lines [0 25])))
    (is (= "@state" (text-at forms/form-at lines [0 30])))
    (is (= "(skipped)" (text-at forms/form-at lines [0 44])) "a discarded form still evaluates by hand")
    (is (= "^:private secret" (text-at forms/form-at lines [0 61])) "metadata comes with the form it marks")
    (is (= "#inst \"2020\"" (text-at forms/form-at lines [0 74])))))

(deftest strings-hold-no-brackets
  (let [lines ["(str \"(not\" \\( \")\")"]]
    (is (= ["(str \"(not\" \\( \")\")"] (map #(forms/form-text lines %) (forms/read-forms lines))))
    (is (= "\"(not\"" (text-at forms/form-at lines [0 7])))))

(deftest a-string-runs-across-lines
  (let [lines ["(def doc \"first line" "second (line\" 2)"]]
    (is (= "\"first line\nsecond (line\"" (text-at forms/form-at lines [0 12])))
    (is (= "(def doc \"first line\nsecond (line\" 2)" (text-at forms/top-level-at lines [1 3]))))
  (testing "a quote opening a line inside a string closes it"
    (let [lines ["(def text \"abc" "\" 1)"]]
      (is (= [true] (map :closed? (forms/read-forms lines))))
      (is (= "(def text \"abc\n\" 1)" (text-at forms/top-level-at lines [1 4]))))))

(deftest open-forms
  (let [lines ["(defn f [x]" "  (inc x)"]
        [form] (forms/read-forms lines)]
    (is (false? (:closed? form)))
    (is (= [1 9] (:end form)) "an open form runs to its last element"))
  (is (not (forms/complete? ["(defn f [x]"])))
  (is (not (forms/complete? ["\"open"])))
  (is (forms/complete? ["(str \")\")"]))
  (is (forms/complete? ["(+ 1 2) :done"]))
  (is (forms/complete? [")"]) "a stray closing bracket is the REPL's to report"))

(deftest the-namespace-for-code
  (let [forms (forms/read-forms sample)]
    (is (= "app.core" (forms/namespace-at forms [4 3])))
    (is (= "app.core" (forms/namespace-at forms [0 0])) "the first namespace for code above it"))
  (let [lines ["(ns ^:no-doc app.meta)" "(in-ns 'app.other)" "(f)"]
        forms (forms/read-forms lines)]
    (is (= "app.meta" (forms/namespace-at forms [0 22])))
    (is (= "app.other" (forms/namespace-at forms [2 1]))))
  (is (nil? (forms/namespace-at (forms/read-forms ["(+ 1 2)"]) [0 0]))))

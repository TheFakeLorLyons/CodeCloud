(ns codecloud.presentation.forms-state-test
  (:require [clojure.test :refer [deftest is testing]]
            [codecloud.presentation.forms-state :as forms-state]))

(defn- text-at
  "The text of the form `locate` finds at `position` in `lines`, or nil."
  [locate lines position]
  (some->> (locate (forms-state/read-forms lines) position) (forms-state/form-text lines)))

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
    (is (= "(+ a b)" (text-at forms-state/form-at sample [4 9])))
    (is (= "(defn add [a b]\n  (+ a b))" (text-at forms-state/form-at sample [4 10]))))
  (testing "a symbol the caret is in or beside"
    (is (= "b" (text-at forms-state/form-at sample [4 7])))
    (is (= "b" (text-at forms-state/form-at sample [4 8])) "the caret right after b, before the closing bracket"))
  (testing "between elements, the collection holding the caret"
    (is (= "(defn add [a b]\n  (+ a b))" (text-at forms-state/form-at sample [4 1]))))
  (testing "nothing away from every form"
    (is (nil? (text-at forms-state/form-at sample [2 0])))
    (is (nil? (text-at forms-state/form-at sample [4 12])))))

(deftest the-top-level-form
  (is (= "(defn add [a b]\n  (+ a b))" (text-at forms-state/top-level-at sample [4 7])))
  (is (= "(defn add [a b]\n  (+ a b))" (text-at forms-state/top-level-at sample [4 12]))
      "past a form's end on its line, that form")
  (testing "inside a rich comment, the form at the caret within it"
    (is (= "(add 3 4)" (text-at forms-state/top-level-at sample [8 5])))
    (is (= "(add 1 2)" (text-at forms-state/top-level-at sample [7 10])))
    (is (= "(comment\n  (add 1 2)\n  (add 3 4))" (text-at forms-state/top-level-at sample [7 0]))
        "between the forms of a comment, the comment itself"))
  (is (nil? (text-at forms-state/top-level-at sample [5 0]))))

(deftest the-enclosing-form
  (let [lines ["(-> rooms (get :kitchen) (vec))" "[1 [2 3]]"]]
    (is (= "(-> rooms (get :kitchen) (vec))" (text-at forms-state/enclosing-at lines [0 24]))
        "after a form inside a list, the list holding it")
    (is (= "(get :kitchen)" (text-at forms-state/enclosing-at lines [0 14])))
    (is (= "[2 3]" (text-at forms-state/enclosing-at lines [1 6])))
    (is (nil? (text-at forms-state/enclosing-at lines [0 31])) "past the last bracket, nothing encloses the caret")
    (is (nil? (text-at forms-state/enclosing-at lines [1 0])))))

(deftest brackets-closing-the-code-up-to-the-caret
  (let [lines ["(defn plan [rooms]" "  (-> rooms (get :kitchen) (vec)))"]
        read (forms-state/read-forms lines)]
    (is (= ")" (forms-state/closing-brackets read [1 2] [1 27])) "the list around the caret, from its start")
    (is (= "))" (forms-state/closing-brackets read [0 0] [1 27])) "the top-level form, from its start")
    (is (= "])" (forms-state/closing-brackets read [0 0] [0 12])) "innermost first")
    (is (= "" (forms-state/closing-brackets read [0 0] [1 34])) "nothing is open after the form")))

(deftest reader-prefixes-belong-to-their-forms
  (let [lines ["#?(:clj 1 :cljs 2) '(1 2) @state #_(skipped) ^:private secret #inst \"2020\""]]
    (is (= "#?(:clj 1 :cljs 2)" (text-at forms-state/form-at lines [0 18])))
    (is (= "'(1 2)" (text-at forms-state/form-at lines [0 25])))
    (is (= "@state" (text-at forms-state/form-at lines [0 30])))
    (is (= "(skipped)" (text-at forms-state/form-at lines [0 44])) "a discarded form still evaluates by hand")
    (is (= "^:private secret" (text-at forms-state/form-at lines [0 61])) "metadata comes with the form it marks")
    (is (= "#inst \"2020\"" (text-at forms-state/form-at lines [0 74])))))

(deftest strings-hold-no-brackets
  (let [lines ["(str \"(not\" \\( \")\")"]]
    (is (= ["(str \"(not\" \\( \")\")"] (map #(forms-state/form-text lines %) (forms-state/read-forms lines))))
    (is (= "\"(not\"" (text-at forms-state/form-at lines [0 7])))))

(deftest a-string-runs-across-lines
  (let [lines ["(def doc \"first line" "second (line\" 2)"]]
    (is (= "\"first line\nsecond (line\"" (text-at forms-state/form-at lines [0 12])))
    (is (= "(def doc \"first line\nsecond (line\" 2)" (text-at forms-state/top-level-at lines [1 3]))))
  (testing "a quote opening a line inside a string closes it"
    (let [lines ["(def text \"abc" "\" 1)"]]
      (is (= [true] (map :closed? (forms-state/read-forms lines))))
      (is (= "(def text \"abc\n\" 1)" (text-at forms-state/top-level-at lines [1 4]))))))

(deftest open-forms
  (let [lines ["(defn f [x]" "  (inc x)"]
        [form] (forms-state/read-forms lines)]
    (is (false? (:closed? form)))
    (is (= [1 9] (:end form)) "an open form runs to its last element"))
  (is (not (forms-state/complete? ["(defn f [x]"])))
  (is (not (forms-state/complete? ["\"open"])))
  (is (forms-state/complete? ["(str \")\")"]))
  (is (forms-state/complete? ["(+ 1 2) :done"]))
  (is (forms-state/complete? [")"]) "a stray closing bracket is the REPL's to report"))

(deftest an-unclosed-form-leaves-the-forms-below-whole
  (let [lines ["(defn broken [x]" "  (inc x" "" "(defn fine [y]" "  y)"]
        [broken fine :as read] (forms-state/read-forms lines)]
    (is (= 2 (count read)))
    (is (false? (:closed? broken)))
    (is (= [1 8] (:end broken)) "it runs to its last element, above the next top-level form")
    (is (true? (:closed? fine)))
    (is (= "(defn fine [y]\n  y)" (text-at forms-state/top-level-at lines [4 3])))
    (is (= "y" (text-at forms-state/form-at lines [4 3])))
    (is (not (forms-state/complete? lines)) "the text as a whole still reads as unclosed")))

(deftest the-namespace-for-code
  (let [forms (forms-state/read-forms sample)]
    (is (= "app.core" (forms-state/namespace-at forms [4 3])))
    (is (= "app.core" (forms-state/namespace-at forms [0 0])) "the first namespace for code above it"))
  (let [lines ["(ns ^:no-doc app.meta)" "(in-ns 'app.other)" "(f)"]
        forms (forms-state/read-forms lines)]
    (is (= "app.meta" (forms-state/namespace-at forms [0 22])))
    (is (= "app.other" (forms-state/namespace-at forms [2 1]))))
  (is (nil? (forms-state/namespace-at (forms-state/read-forms ["(+ 1 2)"]) [0 0]))))

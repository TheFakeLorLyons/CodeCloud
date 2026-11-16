(ns codecloud.presentation.editing-state-test
  (:require [clojure.string :as string]
            [clojure.test :refer [deftest is testing]]
            [codecloud.languages.lexer :as lexer]
            [codecloud.languages.lexicons :as lexicons]
            [codecloud.presentation.clojure-format :as clojure-format]
            [codecloud.presentation.editing-state :as editing-state]))

(def ^:private options {:language :clojure :tab-size 4 :page-lines 3})

(defn- typed
  "The buffer after typing `text` one character at a time."
  [buffer text]
  (reduce (fn [current character] (editing-state/insert current (str character) :typing)) buffer text))

(defn- text-of [buffer] (string/join "\n" (:lines buffer)))

(defn- applied
  "`lines` with a change from `change-between` made, as the host would."
  [lines {[line column end-line end-column] :range :keys [text]}]
  (let [joined (string/join "\n" lines)
        offset (fn [target-line target-column]
                 (+ target-column (reduce + 0 (map #(inc (count %)) (take target-line lines)))))]
    (string/split (str (subs joined 0 (offset line column)) text (subs joined (offset end-line end-column))) #"\n" -1)))

(deftest buffers-start-at-the-top
  (is (= [""] (:lines (editing-state/buffer ""))))
  (is (= ["a" "b"] (:lines (editing-state/buffer "a\r\nb\n"))))
  (is (= [0 0] (:caret (editing-state/buffer "a")))))

(deftest typing-and-deleting
  (let [typed-buffer (typed (editing-state/place (editing-state/buffer "(defn f [])") [0 9] false) "x y")]
    (is (= "(defn f [x y])" (text-of typed-buffer)))
    (is (= [0 12] (:caret typed-buffer)))
    (testing "a run of typing undoes as one change"
      (is (= "(defn f [])" (text-of (editing-state/undo typed-buffer))))
      (is (= "(defn f [x y])" (text-of (editing-state/redo (editing-state/undo typed-buffer))))))
    (testing "moving the caret starts a new run"
      (let [split-run (-> typed-buffer (editing-state/move :left false options) (editing-state/move :right false options) (typed "z"))]
        (is (= "(defn f [x yz])" (text-of split-run)))
        (is (= "(defn f [x y])" (text-of (editing-state/undo split-run))))))
    (testing "deleting takes the selection, or a character, or a word"
      (is (= "(defn f [x ])" (text-of (editing-state/delete typed-buffer :left options))))
      (is (= "(defn f [x ])" (text-of (editing-state/delete typed-buffer :word-left options))))
      (is (= "(defn f [x y" (text-of (editing-state/delete (editing-state/select-range typed-buffer [0 12] [0 14]) :left options)))))))

(deftest moving-the-caret
  (let [buffer (editing-state/buffer "(ns demo.core)\n  (def answer 42)\n\tx")]
    (is (= [0 1] (:caret (editing-state/move buffer :right false options))))
    (is (= [0 14] (:caret (editing-state/move buffer :line-end false options))))
    (is (= [1 0] (:caret (editing-state/move (editing-state/place buffer [0 14] false) :right false options))))
    (testing "words in Lisp hold their punctuation"
      (is (= [0 1] (:caret (editing-state/move buffer :word-right false options))))
      (is (= [0 3] (:caret (editing-state/move (editing-state/place buffer [0 1] false) :word-right false options))))
      (is (= [0 13] (:caret (editing-state/move (editing-state/place buffer [0 3] false) :word-right false options))))
      (is (= [[0 4] [0 13]] (editing-state/word-range buffer [0 6] :clojure))))
    (testing "home goes to the indentation first, then the margin"
      (let [indented (editing-state/place buffer [1 9] false)]
        (is (= [1 2] (:caret (editing-state/move indented :line-start false options))))
        (is (= [1 0] (:caret (editing-state/move (editing-state/move indented :line-start false options) :line-start false options))))))
    (testing "up and down keep the screen column, counting tabs"
      (let [from-number (editing-state/place buffer [1 6] false)
            down (editing-state/move from-number :down false options)]
        (is (= [2 2] (:caret down)))
        (is (= [1 6] (:caret (editing-state/move down :up false options))))))
    (testing "shift extends and a plain move collapses the selection"
      (let [extended (editing-state/move (editing-state/move buffer :right true options) :right true options)]
        (is (= "(n" (editing-state/selected-text extended)))
        (is (= [0 0] (:caret (editing-state/move extended :left false options))))))
    (is (= [[0 0] [2 2]] (editing-state/ordered (editing-state/select-all buffer))))))

(deftest characters-outside-the-basic-plane-move-whole
  (let [buffer (editing-state/buffer "a😀b")]
    (is (= [0 3] (:caret (editing-state/move (editing-state/place buffer [0 1] false) :right false options))))
    (is (= "ab" (text-of (editing-state/delete (editing-state/place buffer [0 3] false) :left options))))))

(deftest breaking-lines-indents-them
  (testing "Lisp lines line up under the open bracket"
    (is (= "(defn f [xs]\n  )" (text-of (editing-state/break-line (editing-state/place (editing-state/buffer "(defn f [xs])") [0 12] false) :clojure "  "))))
    (is (= "{:a 1\n :b 2}" (text-of (typed (editing-state/break-line (editing-state/place (editing-state/buffer "{:a 1}") [0 5] false) :clojure "  ") ":b 2"))))
    (is (= "(str \"(\"\n     )" (text-of (editing-state/break-line (editing-state/place (editing-state/buffer "(str \"(\")") [0 8] false) :clojure "  ")))))
  (testing "Clojure lines begin where cljfmt indents them, the spaces around the break gone"
    (is (= "(let [a 1\n      b 2]" (text-of (editing-state/break-line (editing-state/place (editing-state/buffer "(let [a 1 b 2]") [0 9] false) :clojure "  "))))
    (is (= "(foo bar\n     baz)" (text-of (editing-state/break-line (editing-state/place (editing-state/buffer "(foo bar baz)") [0 8] false) :clojure "  ")))))
  (testing "inside a string the new line lines up under its opening quote, and its spaces stay"
    (let [broken (editing-state/break-line (editing-state/place (editing-state/buffer "(defn f\n  \"Does a thing") [1 15] false) :clojure "  ")]
      (is (= "(defn f\n  \"Does a thing\n  " (text-of broken)))
      (is (= [2 2] (:caret broken))))
    (is (= "(str \"hi\n      there\")" (text-of (editing-state/break-line (editing-state/place (editing-state/buffer "(str \"hi there\")") [0 8] false) :clojure "  ")))))
  (testing "other languages keep the line's indentation, and step in after an opening bracket"
    (let [broken (editing-state/break-line (editing-state/place (editing-state/buffer "  if (x) {}") [0 10] false) :javascript "  ")]
      (is (= "  if (x) {\n    \n  }" (text-of broken)))
      (is (= [1 4] (:caret broken))))
    (is (= "def f():\n    " (text-of (editing-state/break-line (editing-state/place (editing-state/buffer "def f():") [0 8] false) :python "    "))))))

(deftest indenting-and-commenting
  (let [buffer (editing-state/select-range (editing-state/buffer "a\nb\nc") [0 0] [1 1])]
    (is (= "  a\n  b\nc" (text-of (editing-state/indent buffer "  " 4))))
    (is (= "a\nb\nc" (text-of (editing-state/outdent (editing-state/indent buffer "  " 4) "  "))))
    (is (= ";; a\n;; b\nc" (text-of (editing-state/toggle-comment buffer ";;"))))
    (is (= "a\nb\nc" (text-of (editing-state/toggle-comment (editing-state/toggle-comment buffer ";;") ";;"))))
    (is (= "  x" (text-of (editing-state/indent (editing-state/buffer "x") "  " 4)))))
  (is (= "  " (editing-state/indent-unit ["(ns a)" "  (:require)" "    [b]"] :clojure)))
  (is (= "\t" (editing-state/indent-unit ["a" "\tb" "\t\tc"] :javascript)))
  (is (= "    " (editing-state/indent-unit ["x = 1"] :python))))

(deftest changes-are-the-least-replacement
  (is (nil? (editing-state/change-between ["a" "b"] ["a" "b"])))
  (is (= {:range [0 1 0 1] :text "x" :replaced ""} (editing-state/change-between ["ab"] ["axb"])))
  (is (= {:range [0 1 0 1] :text "\n" :replaced ""} (editing-state/change-between ["ab"] ["a" "b"])))
  (is (= {:range [0 1 1 1] :text "" :replaced "\nb"} (editing-state/change-between ["a" "b"] ["a"])))
  (is (= {:range [0 0 0 0] :text "new\n" :replaced ""} (editing-state/change-between ["a"] ["new" "a"])))
  (testing "applying the change to the text before gives the text after"
    (doseq [[before after] [[["(defn f [x]" "  x)"] ["(defn f [x y]" "  (+ x y))"]]
                            [["a" "b" "c" "d"] ["a" "c" "d"]]
                            [["one" "two"] ["one" "two" "three"]]
                            [["same" "same" "same"] ["same" "same"]]
                            [[""] ["filled" "in"]]]]
      (is (= after (applied before (editing-state/change-between before after)))))))

(deftest a-change-from-elsewhere-keeps-the-caret-near
  (let [rebased (editing-state/rebase (typed (editing-state/place (editing-state/buffer "abc\ndef") [1 3] false) "g") "abc")]
    (is (= ["abc"] (:lines rebased)))
    (is (= [0 3] (:caret rebased)))
    (is (empty? (:undo rebased)))))

(deftest tabs-expand-where-columns-count-them
  (is (= 4 (editing-state/visual-column "\tx" 1 4)))
  (is (= 1 (editing-state/column-at-visual "\tx" 4 4)))
  (is (= "a   b" (editing-state/expand-tabs "a\tb" 0 4)))
  (is (= "  b" (editing-state/expand-tabs "\tb" 2 4))))

(deftest lexing-again-reads-only-what-changed
  (let [lexicon (lexicons/lexicon-for :clojure)
        lines ["(ns demo)" "(def text \"one" "two\")" "(defn f [x]" "  ; note" "  x)"]
        before (lexer/tokenized lexicon lines)]
    (is (= (lexer/tokenize-lines lexicon lines) (:tokens before)))
    (doseq [after [(assoc lines 3 "(defn g [x]")
                   (assoc lines 1 "(def text \"one\")")
                   (into (subvec lines 0 2) (subvec lines 3))
                   (conj lines "(def more 1)")
                   (into ["(comment"] lines)
                   ["(ns demo)"]]]
      (let [again (lexer/retokenized before after)]
        (is (= (lexer/tokenize-lines lexicon after) (:tokens again)) (pr-str after))
        (is (= (:starts (lexer/tokenized lexicon after)) (:starts again)) (pr-str after))))))

(defn- caret-at [text position]
  (editing-state/place (editing-state/buffer text) position false))

(defn- typed-in [buffer text]
  (reduce (fn [current character] (editing-state/type-text current (str character) :clojure)) buffer text))

(deftest brackets-and-quotes-come-in-pairs
  (testing "an opening bracket before a space, a closer, or the end of the line brings its partner"
    (let [paired (editing-state/type-text (caret-at "(foo )" [0 5]) "(" :clojure)]
      (is (= "(foo ())" (text-of paired)))
      (is (= [0 6] (:caret paired))))
    (is (= "(foo (bar)" (text-of (editing-state/type-text (caret-at "(foo bar)" [0 5]) "(" :clojure))))
    (is (= "[]" (text-of (editing-state/type-text (caret-at "" [0 0]) "[" :clojure))))
    (is (= "{}" (text-of (editing-state/type-text (caret-at "" [0 0]) "{" :clojure)))))
  (testing "strings, comments, and the character after a backslash take brackets as typed"
    (is (= "\"ab( \"" (text-of (editing-state/type-text (caret-at "\"ab \"" [0 3]) "(" :clojure))))
    (is (= "; ab (" (text-of (editing-state/type-text (caret-at "; ab " [0 5]) "(" :clojure))))
    (is (= "\\(" (text-of (editing-state/type-text (caret-at "\\" [0 1]) "(" :clojure))))
    (is (= "(def s \"a\nb(\")" (text-of (editing-state/type-text (caret-at "(def s \"a\nb\")" [1 1]) "(" :clojure)))))
  (testing "a closer typed where the same one stands steps over it"
    (let [stepped (editing-state/type-text (caret-at "(foo)" [0 4]) ")" :clojure)]
      (is (= "(foo)" (text-of stepped)))
      (is (= [0 5] (:caret stepped))))
    (is (= "(foo])" (text-of (editing-state/type-text (caret-at "(foo)" [0 4]) "]" :clojure))))
    (is (= "(f))" (text-of (editing-state/type-text (editing-state/select-range (editing-state/buffer "(foo)") [0 2] [0 4]) ")" :clojure)))
        "over a selection, a closer takes the selection's place"))
  (testing "a quote pairs, and steps out of the string it ends"
    (is (= "(str \"\")" (text-of (editing-state/type-text (caret-at "(str )" [0 5]) "\"" :clojure))))
    (is (= [0 9] (:caret (editing-state/type-text (caret-at "(str \"ab\")" [0 8]) "\"" :clojure))))
    (is (= "(str \"a\\\"\")" (text-of (editing-state/type-text (caret-at "(str \"a\\\")" [0 8]) "\"" :clojure))))
    (is (= "(str \"ab\"" (text-of (editing-state/type-text (caret-at "(str \"ab" [0 8]) "\"" :clojure))))
    (is (= "don\"" (text-of (editing-state/type-text (caret-at "don" [0 3]) "\"" :javascript)))))
  (testing "typed over a selection, a bracket or quote wraps it"
    (let [wrapped (editing-state/type-text (editing-state/select-range (editing-state/buffer "(inc x y)") [0 5] [0 6]) "(" :clojure)]
      (is (= "(inc (x) y)" (text-of wrapped)))
      (is (= [[0 6] [0 7]] [(:anchor wrapped) (:caret wrapped)])))
    (is (= "a \"b\" c" (text-of (editing-state/type-text (editing-state/select-range (editing-state/buffer "a b c") [0 2] [0 3]) "\"" :clojure)))))
  (testing "typing a whole form leaves it balanced, and undo takes it back at once"
    (let [written (typed-in (caret-at "" [0 0]) "(inc [x])")]
      (is (= "(inc [x])" (text-of written)))
      (is (= "" (text-of (editing-state/undo written)))))))

(deftest deleting-an-empty-pair-takes-both
  (is (= "(foo )" (text-of (editing-state/delete (caret-at "(foo ())" [0 6]) :left {}))))
  (is (= "(str )" (text-of (editing-state/delete (caret-at "(str \"\")" [0 6]) :left {}))))
  (is (= "(fo)" (text-of (editing-state/delete (caret-at "(foo)" [0 4]) :left {}))))
  (is (= "(foo (" (text-of (editing-state/delete (caret-at "(foo ()" [0 7]) :left {})))))

(defn- formatted
  "The buffer after its top-level forms at the caret are formatted, as the
  host's formatter without Calva formats them."
  [{:keys [lines] :as buffer}]
  (let [[start end] (editing-state/format-range buffer)]
    (editing-state/reformatted buffer (clojure-format/reindent lines start start end) :clojure)))

(deftest forms-format-as-one-change
  (let [buffer (caret-at "(defn f [x]\n(let [a 1\nb 2]\n(+ a b)))\n(def y 1)" [2 0])
        after (formatted buffer)]
    (is (= [0 3] (editing-state/format-range buffer)) "the range is the top-level form, found by its brackets")
    (is (= "(defn f [x]\n  (let [a 1\n        b 2]\n    (+ a b)))\n(def y 1)" (text-of after)))
    (is (= [2 8] (:caret after)))
    (is (= (text-of buffer) (text-of (editing-state/undo after)))))
  (let [formatted-already (caret-at "(def a 1)" [0 0])]
    (is (= formatted-already (formatted formatted-already))))
  (testing "a caret on a blank line in a form goes where cljfmt begins the line"
    (let [after (formatted (caret-at "(defn f [x]\n\n  x)" [1 0]))]
      (is (= "(defn f [x]\n  \n  x)" (text-of after)))
      (is (= [1 2] (:caret after))))))

(defn- token-lines-of [text]
  (lexer/tokenize-lines (lexicons/lexicon-for :clojure) (string/split text #"\n" -1)))

(deftest brackets-find-their-partners
  (is (= {:open [0 0] :open-width 1 :close [0 8] :matched? true}
         (editing-state/bracket-pair (token-lines-of "(foo [x])") [0 1] 50)))
  (is (= {:open [0 0] :open-width 1 :close [1 4] :matched? true}
         (editing-state/bracket-pair (token-lines-of "(foo\n  [])") [1 5] 50)))
  (is (= {:open [0 0] :open-width 2 :close [0 3] :matched? true}
         (editing-state/bracket-pair (token-lines-of "#{a}") [0 0] 50)))
  (testing "brackets in strings and comments are passed over"
    (is (= [1 0] (:close (editing-state/bracket-pair (token-lines-of "(f \")\" ;)\n)") [0 0] 50)))))
  (testing "a pair of different kinds, or a bracket with no partner, is not matched"
    (is (false? (:matched? (editing-state/bracket-pair (token-lines-of "(foo]") [0 5] 50))))
    (is (= {:open [0 0] :open-width 1 :close nil :matched? false} (editing-state/bracket-pair (token-lines-of "(foo") [0 1] 50)))
    (is (= {:open nil :open-width nil :close [0 3] :matched? false} (editing-state/bracket-pair (token-lines-of "foo)") [0 4] 50))))
  (testing "nothing beside the caret, or a partner further than the reach, finds nothing"
    (is (nil? (editing-state/bracket-pair (token-lines-of "(foo bar)") [0 3] 50)))
    (is (nil? (editing-state/bracket-pair (token-lines-of "(a\nb\nc)") [0 0] 1)))))

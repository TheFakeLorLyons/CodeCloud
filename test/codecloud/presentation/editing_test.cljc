(ns codecloud.presentation.editing-test
  (:require [clojure.string :as string]
            [clojure.test :refer [deftest is testing]]
            [codecloud.languages.lexer :as lexer]
            [codecloud.languages.lexicons :as lexicons]
            [codecloud.presentation.editing :as editing]))

(def ^:private options {:language :clojure :tab-size 4 :page-lines 3})

(defn- typed
  "The buffer after typing `text` one character at a time."
  [buffer text]
  (reduce (fn [current character] (editing/insert current (str character) :typing)) buffer text))

(defn- text-of [buffer] (string/join "\n" (:lines buffer)))

(defn- applied
  "`lines` with a change from `change-between` made, as the host would."
  [lines {[line column end-line end-column] :range :keys [text]}]
  (let [joined (string/join "\n" lines)
        offset (fn [target-line target-column]
                 (+ target-column (reduce + 0 (map #(inc (count %)) (take target-line lines)))))]
    (string/split (str (subs joined 0 (offset line column)) text (subs joined (offset end-line end-column))) #"\n" -1)))

(deftest buffers-start-at-the-top
  (is (= [""] (:lines (editing/buffer ""))))
  (is (= ["a" "b"] (:lines (editing/buffer "a\r\nb\n"))))
  (is (= [0 0] (:caret (editing/buffer "a")))))

(deftest typing-and-deleting
  (let [typed-buffer (typed (editing/place (editing/buffer "(defn f [])") [0 9] false) "x y")]
    (is (= "(defn f [x y])" (text-of typed-buffer)))
    (is (= [0 12] (:caret typed-buffer)))
    (testing "a run of typing undoes as one change"
      (is (= "(defn f [])" (text-of (editing/undo typed-buffer))))
      (is (= "(defn f [x y])" (text-of (editing/redo (editing/undo typed-buffer))))))
    (testing "moving the caret starts a new run"
      (let [split-run (-> typed-buffer (editing/move :left false options) (editing/move :right false options) (typed "z"))]
        (is (= "(defn f [x yz])" (text-of split-run)))
        (is (= "(defn f [x y])" (text-of (editing/undo split-run))))))
    (testing "deleting takes the selection, or a character, or a word"
      (is (= "(defn f [x ])" (text-of (editing/delete typed-buffer :left options))))
      (is (= "(defn f [x ])" (text-of (editing/delete typed-buffer :word-left options))))
      (is (= "(defn f [x y" (text-of (editing/delete (editing/select-range typed-buffer [0 12] [0 14]) :left options)))))))

(deftest moving-the-caret
  (let [buffer (editing/buffer "(ns demo.core)\n  (def answer 42)\n\tx")]
    (is (= [0 1] (:caret (editing/move buffer :right false options))))
    (is (= [0 14] (:caret (editing/move buffer :line-end false options))))
    (is (= [1 0] (:caret (editing/move (editing/place buffer [0 14] false) :right false options))))
    (testing "words in Lisp hold their punctuation"
      (is (= [0 1] (:caret (editing/move buffer :word-right false options))))
      (is (= [0 3] (:caret (editing/move (editing/place buffer [0 1] false) :word-right false options))))
      (is (= [0 13] (:caret (editing/move (editing/place buffer [0 3] false) :word-right false options))))
      (is (= [[0 4] [0 13]] (editing/word-range buffer [0 6] :clojure))))
    (testing "home goes to the indentation first, then the margin"
      (let [indented (editing/place buffer [1 9] false)]
        (is (= [1 2] (:caret (editing/move indented :line-start false options))))
        (is (= [1 0] (:caret (editing/move (editing/move indented :line-start false options) :line-start false options))))))
    (testing "up and down keep the screen column, counting tabs"
      (let [from-number (editing/place buffer [1 6] false)
            down (editing/move from-number :down false options)]
        (is (= [2 2] (:caret down)))
        (is (= [1 6] (:caret (editing/move down :up false options))))))
    (testing "shift extends and a plain move collapses the selection"
      (let [extended (editing/move (editing/move buffer :right true options) :right true options)]
        (is (= "(n" (editing/selected-text extended)))
        (is (= [0 0] (:caret (editing/move extended :left false options))))))
    (is (= [[0 0] [2 2]] (editing/ordered (editing/select-all buffer))))))

(deftest characters-outside-the-basic-plane-move-whole
  (let [buffer (editing/buffer "a😀b")]
    (is (= [0 3] (:caret (editing/move (editing/place buffer [0 1] false) :right false options))))
    (is (= "ab" (text-of (editing/delete (editing/place buffer [0 3] false) :left options))))))

(deftest breaking-lines-indents-them
  (testing "Lisp lines line up under the open bracket"
    (is (= "(defn f [x]\n  )" (text-of (editing/break-line (editing/place (editing/buffer "(defn f [x])") [0 11] false) :clojure "  "))))
    (is (= "{:a 1\n :b 2}" (text-of (typed (editing/break-line (editing/place (editing/buffer "{:a 1}") [0 5] false) :clojure "  ") ":b 2"))))
    (is (= "(str \"(\"\n  )" (text-of (editing/break-line (editing/place (editing/buffer "(str \"(\")") [0 8] false) :clojure "  ")))))
  (testing "other languages keep the line's indentation, and step in after an opening bracket"
    (let [broken (editing/break-line (editing/place (editing/buffer "  if (x) {}") [0 10] false) :javascript "  ")]
      (is (= "  if (x) {\n    \n  }" (text-of broken)))
      (is (= [1 4] (:caret broken))))
    (is (= "def f():\n    " (text-of (editing/break-line (editing/place (editing/buffer "def f():") [0 8] false) :python "    "))))))

(deftest indenting-and-commenting
  (let [buffer (editing/select-range (editing/buffer "a\nb\nc") [0 0] [1 1])]
    (is (= "  a\n  b\nc" (text-of (editing/indent buffer "  " 4))))
    (is (= "a\nb\nc" (text-of (editing/outdent (editing/indent buffer "  " 4) "  "))))
    (is (= ";; a\n;; b\nc" (text-of (editing/toggle-comment buffer ";;"))))
    (is (= "a\nb\nc" (text-of (editing/toggle-comment (editing/toggle-comment buffer ";;") ";;"))))
    (is (= "  x" (text-of (editing/indent (editing/buffer "x") "  " 4)))))
  (is (= "  " (editing/indent-unit ["(ns a)" "  (:require)" "    [b]"] :clojure)))
  (is (= "\t" (editing/indent-unit ["a" "\tb" "\t\tc"] :javascript)))
  (is (= "    " (editing/indent-unit ["x = 1"] :python))))

(deftest changes-are-the-least-replacement
  (is (nil? (editing/change-between ["a" "b"] ["a" "b"])))
  (is (= {:range [0 1 0 1] :text "x" :replaced ""} (editing/change-between ["ab"] ["axb"])))
  (is (= {:range [0 1 0 1] :text "\n" :replaced ""} (editing/change-between ["ab"] ["a" "b"])))
  (is (= {:range [0 1 1 1] :text "" :replaced "\nb"} (editing/change-between ["a" "b"] ["a"])))
  (is (= {:range [0 0 0 0] :text "new\n" :replaced ""} (editing/change-between ["a"] ["new" "a"])))
  (testing "applying the change to the text before gives the text after"
    (doseq [[before after] [[["(defn f [x]" "  x)"] ["(defn f [x y]" "  (+ x y))"]]
                            [["a" "b" "c" "d"] ["a" "c" "d"]]
                            [["one" "two"] ["one" "two" "three"]]
                            [["same" "same" "same"] ["same" "same"]]
                            [[""] ["filled" "in"]]]]
      (is (= after (applied before (editing/change-between before after)))))))

(deftest a-change-from-elsewhere-keeps-the-caret-near
  (let [rebased (editing/rebase (typed (editing/place (editing/buffer "abc\ndef") [1 3] false) "g") "abc")]
    (is (= ["abc"] (:lines rebased)))
    (is (= [0 3] (:caret rebased)))
    (is (empty? (:undo rebased)))))

(deftest tabs-expand-where-columns-count-them
  (is (= 4 (editing/visual-column "\tx" 1 4)))
  (is (= 1 (editing/column-at-visual "\tx" 4 4)))
  (is (= "a   b" (editing/expand-tabs "a\tb" 0 4)))
  (is (= "  b" (editing/expand-tabs "\tb" 2 4))))

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

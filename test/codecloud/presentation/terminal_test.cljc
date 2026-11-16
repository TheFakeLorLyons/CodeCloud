(ns codecloud.presentation.terminal-test
  "Checks that terminal output becomes plain lines, its escape sequences
  removed even when split between chunks, with lines redrawn by carriage
  returns, backspaces, and lines still waiting for their end."
  (:require [clojure.test :refer [deftest is testing]]
            [codecloud.presentation.terminal :as terminal]))

(defn- read-all
  "The lines `chunks` complete, then the line left over."
  [chunks]
  (let [{:keys [state lines]} (reduce (fn [{:keys [state lines]} chunk]
                                        (let [fed (terminal/feed state chunk)]
                                          {:state (:state fed) :lines (str lines (:lines fed))}))
                                      {:state terminal/empty-state :lines ""}
                                      chunks)]
    [lines (:lines (terminal/flush-line state))]))

(deftest escape-sequences-go
  (is (= ["red and plain\n" ""] (read-all ["\u001b[31mred\u001b[0m and plain\n"])))
  (is (= ["done\n" ""] (read-all ["\u001b]633;C\u0007done\n\u001b]633;D;0\u0007"]))
      "shell integration marks vanish")
  (is (= ["title gone\n" ""] (read-all ["\u001b]0;my title\u001b\\title gone\n"])))
  (is (= ["Claude\n" ""] (read-all ["\u001b7\u001b[?25lClaude\u001b8\n"]))
      "saving and restoring the cursor leave nothing behind")
  (is (= ["kept\n" ""] (read-all ["\u001bP+q544e\u001b\\kept\u001b=\u001b>\n"]))
      "device strings and keypad modes vanish")
  (is (= ["Welcome to Claude Code  v2\n" ""]
         (read-all ["\u001b[38;5;174mWelcome\u001b[9Gto\u001b[12GClaude\u001b[19GCode\u001b[2Cv2\u001b[39m\r\n"]))
      "words placed by column keep their spaces")
  (is (= ["after an odd escape\n" ""] (read-all ["\u001b\u0001after an odd escape\n"]))
      "an escape of no known kind holds nothing back")
  (is (= ["kept whole\n" ""] (read-all ["\u001b" "7kept whole\u001b" "8\n"])) "a two-character escape split between chunks")
  (is (= ["ab  c\n" ""] (read-all ["ab\u001b[2Cc\n"])) "moving forward leaves spaces")
  (is (= ["abcdef\n" ""] (read-all ["abcdef\u001b[3G\n"])) "moving back to a column takes nothing away")
  (is (= ["col\n" ""] (read-all ["\u001b[G" "col\n"])) "a column move without a number goes to the first")
  (testing "a sequence split between chunks still goes"
    (is (= ["green\n" ""] (read-all ["\u001b[3" "2mgreen\u001b[" "0m\n"])))
    (is (= ["after\n" ""] (read-all ["\u001b]633;P;Cwd=/wo" "rk\u0007after\n"])))
    (is (= ["titled\n" ""] (read-all ["\u001b]0;a title\u001b" "\\titled\n"])) "even inside its terminator")))

(deftest lines-redraw-in-place
  (is (= ["100%\n" ""] (read-all ["10%\r50%\r100%\n"])) "a progress bar keeps its last state")
  (is (= ["one\ntwo\n" ""] (read-all ["one\r\ntwo\r\n"])) "Windows line ends are line ends")
  (is (= ["kept\n" ""] (read-all ["kept\r\r\n"])) "as are line ends a terminal doubled the return of")
  (is (= ["cat\n" ""] (read-all ["cab\bt\n"])) "a backspace takes the character before it")
  (is (= ["a\tb\n" ""] (read-all ["a\tb\u0007\n"])) "tabs stay and bells go"))

(deftest drawing-over-lines-above-is-noticed
  (let [redraws? #(:redraws? (terminal/feed terminal/empty-state %))]
    (is (redraws? "\u001b[2K\u001b[1A\u001b[2K> prompt") "moving up to draw a frame again")
    (is (redraws? "\u001b[?1049h") "the alternate screen")
    (is (not (redraws? "\u001b[31mred\u001b[0m\r50%")) "colors and progress drawn with returns are plain lines")))

(deftest a-line-waits-for-its-end
  (is (= ["" "Password: "] (read-all ["Pass" "word: "])))
  (is (= ["first\n" "second"] (read-all ["first\nsec" "ond"]))))

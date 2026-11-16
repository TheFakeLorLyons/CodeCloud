(ns codecloud.model.uses-test
  "Checks finding where a definition is used, through the aliases and
  refers of Clojure files and by name in other languages."
  (:require [clojure.test :refer [deftest is testing]]
            [codecloud.model.parse :as parse]
            [codecloud.model.uses :as uses]))

(defn- lines-of [places]
  (mapv :line places))

(deftest clojure-uses-read-through-requires
  (testing "an alias, a renamed refer, and :refer :all all reach the var"
    (let [text (str "(ns app.ui\n"
                    "  (:require [app.db :as db]\n"
                    "            [app.util :refer [save!] :rename {save! keep!}]\n"
                    "            [app.text :refer :all]))\n"
                    "(defn show [x] (db/save! x) (keep! x))\n"
                    "(defn label [x] (shout x))\n")]
      (is (= [{:line 5 :column 17}] (parse/uses-of "src/app/ui.clj" text "app.db" "save!")))
      (is (= [{:line 3 :column 31 :declaration? true} {:line 3 :column 47 :declaration? true} {:line 5 :column 30}]
             (parse/uses-of "src/app/ui.clj" text "app.util" "save!"))
          "the refer and the rename declare the name, which a rename of the var changes")
      (is (= [{:line 6 :column 18}] (parse/uses-of "src/app/ui.clj" text "app.text" "shout")))
      (is (= [] (parse/uses-of "src/app/ui.clj" text "app.text" "whisper")))))
  (testing "in its own namespace a var is used by name, qualified or not, and its definition is no use"
    (is (= [{:line 2 :column 17} {:line 3 :column 12}]
           (parse/uses-of "src/app/count.clj" "(ns app.count)\n(defn tick [n] (tick (dec n)))\n(def once (app.count/tick 1))\n"
                          "app.count" "tick"))))
  (testing "a use inside a comment form says so"
    (is (= [{:line 3 :column 4 :comment? true}]
           (parse/uses-of "dev/user.clj" "(ns user (:require [app.db :as db]))\n(comment\n  (db/save! {}))\n" "app.db" "save!"))))
  (testing "a .cljc file counts each use once, whichever platforms read it"
    (is (= [2 2] (lines-of (parse/uses-of "src/app/both.cljc"
                                          "(ns app.both (:require [app.db :as db]))\n(defn go [] #?(:clj (db/save! 1) :cljs (db/save! 2)))\n"
                                          "app.db" "save!"))))))

(deftest other-languages-use-a-name
  (testing "names in strings and comments are no uses"
    (is (= [{:line 1 :column 1} {:line 2 :column 30}]
           (uses/named-places :javascript "plot(1); // plot again\nconst label = \"plot\"; series.plot(2);\n" "plot"))))
  (testing "Python, with its imports"
    (is (= [1 3] (lines-of (uses/named-places :python "from eeg.filters import bandpass\n# bandpass\nbandpass(raw)\n" "bandpass")))))
  (testing "a Clojure file reaching a module through an alias names it after the alias"
    (is (= [{:line 2 :column 2}]
           (uses/named-places :clojure "(ns app.core (:require [\"./plot.js\" :as chart]))\n(chart/plot 1)\n" "plot")))))

(deftest places-leave-out-the-declaration
  (let [page {:id "web/js/plot.js" :kind :module :language :javascript :files [{:path "web/js/plot.js"}]}
        definition {:name "WIDTH" :kind :value :line 1 :path "web/js/plot.js"}]
    (is (= [{:line 2 :column 14}]
           (uses/places {:path "web/js/plot.js" :language :javascript :text "export const WIDTH = 800;\nconst half = WIDTH / 2;\n"}
                        page definition))
        "the line declaring the name in its own file is left out")
    (is (= [{:line 1 :column 10} {:line 2 :column 8}]
           (uses/places {:path "web/js/colors.js" :language :javascript
                         :text "import { WIDTH } from \"./plot.js\";\nreturn WIDTH / 2;\n"}
                        page definition))))
  (testing "a Clojure file requiring a module by a relative string reaches its exports through the alias"
    (let [page {:id "web/js/plot.js" :kind :module :language :javascript :files [{:path "web/js/plot.js"}]}]
      (is (= [{:line 3 :column 2}]
             (uses/places {:path "src/app/core.cljs"
                           :text "(ns app.core\n  (:require [\"../../web/js/plot.js\" :as chart]))\n(chart/plot 1)\n"}
                          page {:name "plot" :kind :function :line 5 :path "web/js/plot.js"}))))))

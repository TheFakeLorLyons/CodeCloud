(ns codecloud.presentation.markdown-test
  "Checks the reading of the assistant's Markdown, its blocks, its inline
  marks, links to places in the codebase and to the web, and text that
  stays text."
  (:require [clojure.test :refer [deftest is testing]]
            [codecloud.presentation.markdown :as markdown]))

(def ^:private places
  {"src/core.clj:12" {:path "src/core.clj" :line 12}
   "src/core.clj" {:path "src/core.clj" :line nil}})

(def ^:private options
  {:place-for #(get places %)
   :place-link (fn [place children] (into [:place place] children))})

(deftest blocks-read-as-the-assistant-writes-them
  (is (= [{:kind :heading :level 2 :text "Where it starts"}
          {:kind :paragraph :text "One line\nand the next"}
          {:kind :list :ordered? false :start nil
           :items [[{:kind :paragraph :text "first"}]
                   [{:kind :paragraph :text "second"}
                    {:kind :list :ordered? false :start nil :items [[{:kind :paragraph :text "inner"}]]}]]}
          {:kind :list :ordered? true :start 3 :items [[{:kind :paragraph :text "three"}] [{:kind :paragraph :text "four"}]]}
          {:kind :code :language "clojure" :text "(+ 1 2)"}
          {:kind :quote :blocks [{:kind :paragraph :text "said\ntwice"}]}
          {:kind :rule}]
         (markdown/read-blocks (str "## Where it starts\n\nOne line\nand the next\n\n- first\n- second\n  - inner\n\n"
                                    "3. three\n4. four\n\n```clojure\n(+ 1 2)\n```\n\n> said\n> twice\n\n---"))))
  (testing "a list right under a paragraph, and a line continuing an item"
    (is (= [{:kind :paragraph :text "Steps:"}
            {:kind :list :ordered? false :start nil :items [[{:kind :paragraph :text "one\ncontinued"}] [{:kind :paragraph :text "two"}]]}]
           (markdown/read-blocks "Steps:\n- one\ncontinued\n- two"))))
  (testing "code inside a list item"
    (is (= [{:kind :list :ordered? true :start 1
             :items [[{:kind :paragraph :text "Run:"} {:kind :code :language "sh" :text "make"}]]}]
           (markdown/read-blocks "1. Run:\n   ```sh\n   make\n   ```"))))
  (testing "a fence still open while the reply streams"
    (is (= [{:kind :paragraph :text "So:"} {:kind :code :language nil :text "(unfinished\n  form"}]
           (markdown/read-blocks "So:\n```\n(unfinished\n  form"))))
  (testing "a table, its rows as wide as its header"
    (is (= [{:kind :table :alignments ["left" "right"] :header ["Name" "Line"] :rows [["plant" "6"] ["grow" ""]]}]
           (markdown/read-blocks "| Name | Line |\n|:-----|-----:|\n| plant | 6 |\n| grow |")))))

(deftest inline-marks-become-hiccup
  (is (= [[:p "See " [:code "plant"] " at " [:place {:path "src/core.clj" :line 12} "src/core.clj:12"] ", "
           [:strong "then"] " " [:em "more"] "."]]
         (markdown/hiccup "See `plant` at src/core.clj:12, **then** *more*." options)))
  (is (= [[:p [:strong "bold " [:code "code"] " inside"] " and " [:del "old"]]]
         (markdown/hiccup "**bold `code` inside** and ~~old~~" options)))
  (is (= [[:p "one" [:br] "two"]] (markdown/hiccup "one\ntwo" options)) "a line break stays")
  (is (= [[:p "keep my_var_name and *not em*"]] (markdown/hiccup "keep my_var_name and \\*not em\\*" options))
      "an underscore inside a word and an escaped star mark nothing"))

(deftest places-become-links
  (is (= [[:p [:place {:path "src/core.clj" :line 12} [:code "src/core.clj:12"]]]]
         (markdown/hiccup "`src/core.clj:12`" options))
      "code naming a place")
  (is (= [[:p "read " [:place {:path "src/core.clj" :line 12} "the core"]]]
         (markdown/hiccup "read [the core](src/core.clj#L12)" options))
      "a link to a line, written as an anchor")
  (is (= [[:p "Open " [:place {:path "src/core.clj" :line nil} "src/core.clj"] " first."]]
         (markdown/hiccup "Open src/core.clj first." options))
      "a path alone")
  (is (= [[:p "see notes.txt:4 and e.g. this"]] (markdown/hiccup "see notes.txt:4 and e.g. this" options))
      "a reference to no file in the cloud stays text")
  (is (= [[:p "src/core.clj:12"]] (markdown/hiccup "src/core.clj:12" {}))
      "without a way to find places, references stay text"))

(deftest only-web-pages-are-linked
  (let [address "https://example.com/a"]
    (is (= [[:p [:a {:href address :target "_blank" :rel "noopener noreferrer" :title address} "docs"]]]
           (markdown/hiccup "[docs](https://example.com/a)" options)))
    (is (= [[:p "at " [:a {:href "https://example.com/x" :target "_blank" :rel "noopener noreferrer" :title "https://example.com/x"}
                       "https://example.com/x"] "."]]
           (markdown/hiccup "at https://example.com/x." options))
        "a bare address, without the stop after it"))
  (is (= [[:p [:span {:title "javascript:void"} "click"]]] (markdown/hiccup "[click](javascript:void)" options))
      "any other link shows its words alone")
  (is (= [[:p "<b>bold</b> & <script>x</script>"]] (markdown/hiccup "<b>bold</b> & <script>x</script>" options))
      "markup stays text"))

(deftest blocks-become-hiccup
  (is (= [[:h3.md-heading "Title"] [:h4.md-heading "Small"]] (markdown/hiccup "## Title\n#### Small" options)))
  (is (= [[:ul {} [:li "one"] [:li "two"]] [:ol {:start 3} [:li "three"]]]
         (markdown/hiccup "- one\n- two\n\n3. three" options)))
  (is (= [[:ul {} [:li [:p "Run:"] [:pre.md-code {:data-language "sh"} [:code "make"]]]]]
         (markdown/hiccup "- Run:\n  ```sh\n  make\n  ```" options))
      "an item holding more than a paragraph keeps its blocks")
  (is (= [[:div.md-table
           [:table
            [:thead [:tr [:th {:style {"text-align" "left"}} "Name"] [:th {} "Line"]]]
            [:tbody [:tr [:td {:style {"text-align" "left"}} [:code "plant"]] [:td {} "6"]]]]]]
         (markdown/hiccup "| Name | Line |\n|:--|--|\n| `plant` | 6 |" options)))
  (is (= [[:blockquote [:p "quoted"]] [:hr]] (markdown/hiccup "> quoted\n\n***" options))))

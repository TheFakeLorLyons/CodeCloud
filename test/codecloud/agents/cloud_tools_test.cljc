(ns codecloud.agents.cloud-tools-test
  "Checks the answers of the cloud's tools for agents on the orchard and
  polyglot fixtures."
  (:require [clojure.string :as string]
            [clojure.test :refer [deftest is testing]]
            [codecloud.agents.cloud-tools :as cloud-tools]
            [codecloud.agents.presence :as presence]
            [codecloud.agents.tools :as tools]
            [codecloud.fixtures :as fixtures]))

(def ^:private orchard
  (delay {:snapshot (fixtures/snapshot-of fixtures/orchard-root)
          :texts (fixtures/file-texts fixtures/orchard-root)
          :cloud-name "orchard"
          :root "/work/orchard"}))

(def ^:private polyglot
  (delay {:snapshot (fixtures/snapshot-of fixtures/polyglot-root)
          :texts (fixtures/file-texts fixtures/polyglot-root)
          :cloud-name "polyglot"
          :root "/work/polyglot"}))

(def ^:private minute
  "A minute in milliseconds."
  60000)

(defn- context-of
  "A context for `fixture`, its cloud the workspace's only folder, open
  and in sight, with `overrides` merged in."
  ([fixture] (context-of fixture {}))
  ([fixture overrides]
   (let [{:keys [snapshot cloud-name root]} @fixture]
     (merge {:snapshot snapshot
             :cloud-name cloud-name
             :root root
             :workspace-roots [root]
             :named-views {}
             :focus nil
             :open? true
             :visible? true
             :carets nil
             :now (* 30 minute)}
            overrides))))

(defn- ask
  "The text of the answer `tool-name` gives to `arguments` in `context`,
  with the texts of the files it asks to read, or the error led by
  ERROR."
  [fixture context tool-name arguments]
  (let [paths (cloud-tools/paths-to-read context tool-name arguments)
        answered (cloud-tools/answer (assoc context :texts (select-keys (:texts @fixture) paths)) tool-name arguments)]
    (or (:text answered) (str "ERROR " (:error answered)))))

(defn- answer-lines [text]
  (set (string/split-lines text)))

(deftest the-overview-gives-the-codebase's-shape
  (let [text (ask orchard (context-of orchard) "cloud_overview" {})
        lines (answer-lines text)]
    (is (string/starts-with? text (str "orchard: 17 pages in 17 files, 171 lines, 42 definitions, and 24 dependencies "
                                       "between pages, all Clojure.")))
    (is (contains? lines "  src/orchard: 10 pages, 125 lines"))
    (is (contains? lines "  orchard.util (src/orchard/util.clj): 26 lines, 8 functions, required by 12 pages, importance 1.00"))
    (is (contains? lines "  orchard.bees (src/orchard/bees.clj): 9 lines, 2 functions, required by 1 page, importance 0.52, in a cycle"))
    (is (contains? lines "Cycles that fail to load (1):"))
    (is (contains? lines "  orchard.bees → orchard.flowers → orchard.bees"))
    (is (contains? lines "  orchard.broken (src/orchard/broken.clj): EOF while reading, expected ) to match ( at [8,5], at line 9")))
  (testing "a limit on the pages listed"
    (let [text (ask orchard (context-of orchard) "cloud_overview" {"limit" 2})
          listed (->> (string/split-lines text)
                      (drop-while #(not= "Most important pages:" %))
                      rest
                      (take-while #(string/starts-with? % "  ")))]
      (is (= ["orchard.util" "orchard.core"] (map #(second (re-find #"^  (\S+)" %)) listed)))))
  (testing "one folder"
    (is (string/starts-with? (ask orchard (context-of orchard) "cloud_overview" {"folder" "src/orchard/ui/"})
                             "orchard, under src/orchard/ui: 3 of 17 pages, 27 lines, all Clojure."))
    (is (= "ERROR No page lies under lib." (ask orchard (context-of orchard) "cloud_overview" {"folder" "lib"}))))
  (testing "a codebase in several languages, with loops its languages allow"
    (let [text (ask polyglot (context-of polyglot) "cloud_overview" {})]
      (is (string/starts-with? text "polyglot: 33 pages in 33 files"))
      (is (string/includes? text "in C++ 9, CSS 5, Python 5, Clojure 3, JavaScript 3, Shell 3, C 2, HTML 2, and 1 more."))
      (is (contains? (answer-lines text) "Loops their languages allow (2):")))))

(deftest page-details-describe-one-page
  (let [text (ask orchard (context-of orchard) "page_details" {"page" "orchard.core"})
        lines (answer-lines text)]
    (is (string/starts-with? text "orchard.core: Clojure namespace in src/orchard/core.clj, 16 lines.\n"))
    (is (contains? lines "Importance 0.76, 4 functions, 4 definitions, 12 uses from other pages."))
    (is (contains? lines "Requires 3 pages:"))
    (is (contains? lines "  orchard.trees (load, 4 uses)"))
    (is (contains? lines "Required by 5 pages:"))
    (is (contains? lines "  user (load)"))
    (is (contains? lines "  grow (function, lines 9-10): 6 uses from other pages"))
    (is (contains? lines "  water (function, lines 12-13): no uses from other pages"))
    (testing "the same page by its path, or by the end of its name"
      (is (= text (ask orchard (context-of orchard) "page_details" {"page" "src/orchard/core.clj"})))
      (is (= text (ask orchard (context-of orchard) "page_details" {"page" "/work/orchard/src/orchard/core.clj"})))
      (is (= text (ask orchard (context-of orchard) "page_details" {"page" "core"})))))
  (testing "a file that does not parse"
    (is (contains? (answer-lines (ask orchard (context-of orchard) "page_details" {"page" "orchard.broken"}))
                   "Does not parse: EOF while reading, expected ) to match ( at [8,5], at line 9.")))
  (testing "pages in other languages, with includes, tangles, and requires from outside"
    (let [board (answer-lines (ask polyglot (context-of polyglot) "page_details" {"page" "native/cpp/Board.cpp"}))]
      (is (contains? board "native/cpp/Board.cpp: C++ file, 20 lines."))
      (is (contains? board "  native/cpp/Board.hpp (include)"))
      (is (contains? board "  Board::read (function, lines 7-13): 2 uses from other pages"))
      (is (contains? board "  Board::stream (lines 15-18): complection 1.00, from 2 asynchronous forms")))
    (is (contains? (answer-lines (ask polyglot (context-of polyglot) "page_details" {"page" "src/eeg/filters.py"}))
                   "Requires from outside the cloud: math, numpy.")))
  (testing "a page that does not exist, with pages whose names hold the text"
    (is (= "ERROR No page is named orch or holds a file at that path. Pages whose names hold it: orchard.bees, orchard.broken, orchard.core, orchard.core-test, orchard.flowers, orchard.fruit, orchard.harvest, orchard.soil, orchard.trees, orchard.trees-test, orchard.ui.events, orchard.ui.subs, and 4 more."
           (ask orchard (context-of orchard) "page_details" {"page" "orch"})))))

(deftest uses-come-through-aliases-and-by-name
  (let [clamp (ask orchard (context-of orchard) "uses_of" {"name" "orchard.util/clamp"})
        lines (answer-lines clamp)]
    (is (string/starts-with? clamp "orchard.util/clamp: function in src/orchard/util.clj, lines 4-5.\nFound 5 uses, all in other pages:\n"))
    (is (contains? lines "  src/orchard/broken.clj:4 in fine: (defn fine [x] (util/clamp x))"))
    (is (contains? lines "  src/orchard/soil.clj:5 in moisture: (util/clamp (util/ratio x)))"))
    (is (contains? lines "  test/orchard/util_test.clj:5 in works: (deftest works (is (subject/clamp 1)))")
        "a test reaches the var through its own alias"))
  (testing "the files read are those of the page and of the pages requiring it"
    (let [read (set (cloud-tools/paths-to-read (context-of orchard) "uses_of" {"name" "orchard.util/clamp"}))]
      (is (every? read ["src/orchard/util.clj" "src/orchard/soil.clj" "test/orchard/util_test.clj"]))
      (is (not (contains? read "dev/user.clj")))))
  (testing "a bare name one page defines, and a use in a comment form"
    (is (contains? (answer-lines (ask orchard (context-of orchard) "uses_of" {"name" "plant"}))
                   "  dev/user.clj:4 in a comment form: (comment (core/plant 1))")))
  (testing "a bare name with its page"
    (is (string/includes? (ask orchard (context-of orchard) "uses_of" {"name" "grow" "page" "orchard.core"})
                          "Found 6 uses, all in other pages:")))
  (testing "names several pages define, or none"
    (is (= (str "ERROR 3 pages define works: orchard.core-test/works, orchard.trees-test/works, orchard.util-test/works. "
                "Name it with its page.")
           (ask orchard (context-of orchard) "uses_of" {"name" "works"})))
    (is (= (str "ERROR No page defines panel. Definitions whose names hold it: orchard.ui.events/events-panel, "
                "orchard.ui.subs/subs-panel, orchard.ui.views/views-panel.")
           (ask orchard (context-of orchard) "uses_of" {"name" "panel"}))))
  (testing "a JavaScript export, used from JavaScript and from ClojureScript through a string require"
    (let [plot (ask polyglot (context-of polyglot) "uses_of" {"name" "web/js/plot.js/plot"})
          lines (answer-lines plot)]
      (is (string/starts-with? plot "plot in web/js/plot.js: function, lines 5-17.\nFound 3 uses, all in other pages:\n"))
      (is (contains? lines "  src/app/core.cljs:6 in render!: (plot/plot canvas (:samples @state/db)))"))
      (is (contains? lines "  web/js/app.js:1 at the top level: import { plot } from \"./plot.js\";"))
      (is (string/includes? plot "found by name"))))
  (testing "two uses on one line share it"
    (is (contains? (answer-lines (ask polyglot (context-of polyglot) "uses_of" {"name" "WIDTH"}))
                   "  web/js/colors.js:6 in scaleFor, 2 times: return sample > 0 ? WIDTH / 1000 : WIDTH / 2000;")))
  (testing "uses within the defining page count apart"
    (let [bandpass (ask polyglot (context-of polyglot) "uses_of" {"name" "bandpass"})]
      (is (string/includes? bandpass "Found 4 uses: 3 in other pages and 1 within its own page:"))
      (is (contains? (answer-lines bandpass) "  src/eeg/filters.py:29 in adaptive: result = bandpass(result, 0.5, 40, rate)")))))

(deftest chains-tie-pages
  (is (= (str "orchard.core reaches orchard.harvest in 3 steps:\n"
              "  orchard.core → orchard.trees (load, 4 uses)\n"
              "  orchard.trees → orchard.fruit (load, 4 uses)\n"
              "  orchard.fruit → orchard.harvest (load, 3 uses)")
         (ask orchard (context-of orchard) "path_between" {"from" "orchard.core" "to" "orchard.harvest"})))
  (is (string/starts-with? (ask orchard (context-of orchard) "path_between" {"from" "orchard.harvest" "to" "orchard.core"})
                           "orchard.harvest does not reach orchard.core, but orchard.core reaches orchard.harvest in 3 steps:"))
  (is (= (str "Neither page reaches the other. Both reach orchard.util:\n"
              "  orchard.soil → orchard.util (load, 6 uses)\n"
              "  orchard.harvest → orchard.util (load, 3 uses)")
         (ask orchard (context-of orchard) "path_between" {"from" "orchard.soil" "to" "orchard.harvest"})))
  (is (string/includes? (ask polyglot (context-of polyglot) "path_between" {"from" "web/index.html" "to" "web/js/colors.js"})
                        "  web/index.html → web/js/plot.js (script, across languages)"))
  (is (= "orchard.core is the page at both ends."
         (ask orchard (context-of orchard) "path_between" {"from" "orchard.core" "to" "src/orchard/core.clj"})))
  (is (string/starts-with? (ask orchard (context-of orchard) "path_between" {"from" "orchard.nowhere" "to" "orchard.core"})
                           "ERROR From: No page is named orchard.nowhere")))

(def ^:private focused
  "A focus report with orchard.core lifted and edited, lines 6 and 7
  selected, and the interface folder hidden."
  {:snapshot-id :working-tree
   :snapshot-label "Working tree"
   :lifted "orchard.core"
   :editing {:namespace-id "orchard.core" :path "src/orchard/core.clj" :line 6 :column 4 :anchor-line 5 :anchor-column 0}
   :in-view ["orchard.core" "orchard.util" "orchard.trees"]
   :view-filter {:active-views #{} :only [] :hidden ["/src/orchard/ui/"] :hidden-languages #{}}
   :visible-count 14
   :arrangement :folders
   :at (* 27 minute)})

(def ^:private caret-in-core
  {:file "/work/orchard/src/orchard/core.clj" :line 6 :column 4 :anchor-line 5 :anchor-column 0 :source :cloud
   :at (* 29 minute)})

(deftest the-view-shows-what-the-filter-leaves
  (is (= (str "The cloud shows 14 of 17 pages, with /src/orchard/ui/ hidden.\n"
              "Shown pages by folder:\n"
              "  dev (1): user\n"
              "  src/orchard (10): orchard.bees, orchard.broken, orchard.core, orchard.flowers, orchard.fruit, orchard.harvest, "
              "orchard.soil, orchard.trees, orchard.util, orchard.weather\n"
              "  test/orchard (3): orchard.core-test, orchard.trees-test, orchard.util-test")
         (ask orchard (context-of orchard {:focus focused}) "cloud_view" {})))
  (testing "a named view, and a hidden language"
    (is (string/starts-with? (ask orchard (context-of orchard {:focus (assoc focused :view-filter {:active-views #{"Tests"}})
                                                              :named-views {"Tests" {:include ["/test/"]}}})
                                  "cloud_view" {})
                             "The cloud shows 3 of 17 pages, with the view Tests on."))
    (is (string/starts-with? (ask polyglot (context-of polyglot {:focus {:view-filter {:hidden-languages #{:python}}}})
                                  "cloud_view" {})
                             "The cloud shows 28 of 33 pages, with Python hidden.")))
  (testing "a commit on show, and no cloud open"
    (is (string/includes? (ask orchard (context-of orchard {:focus (assoc focused :snapshot-id "a153f8c"
                                                                          :snapshot-label "a153f8c Break the cycle")})
                               "cloud_view" {})
                          "The cloud shows a153f8c Break the cycle rather than the working tree; these tools read the working tree."))
    (is (= "No cloud is open in VS Code, so no filter applies: all 17 pages count. cloud_overview describes them."
           (ask orchard (context-of orchard {:open? false}) "cloud_view" {})))))

(deftest the-focus-follows-the-caret
  (is (= (str "The cloud is open and in sight.\n"
              "Their caret is in the cloud, in src/orchard/core.clj (orchard.core), at line 7, column 5, inside plant "
              "(lines 6-7), a moment ago.\n"
              "Selected from line 6, column 1 to line 7, column 5:\n"
              "  6  (defn plant [x]\n"
              "  7    (trees/prune (util/normalize (soil/moisture x))))\n"
              "Lifted page: orchard.core (src/orchard/core.clj).\n"
              "On screen, largest first, as of 3 minutes ago: orchard.core, orchard.util, orchard.trees.")
         (ask orchard (context-of orchard {:focus focused :carets {:latest caret-in-core}}) "viewer_focus" {})))
  (is (= ["src/orchard/core.clj"]
         (cloud-tools/paths-to-read (context-of orchard {:carets {:latest caret-in-core}}) "viewer_focus" {}))
      "the caret's file is read for its code")
  (testing "a caret moved since in a VS Code editor, on a line holding code"
    (let [text (ask orchard (context-of orchard {:focus focused
                                                 :carets {:latest {:file "/work/orchard/src/orchard/trees.clj" :line 5
                                                                   :column 2 :anchor-line 5 :anchor-column 2 :source :editor
                                                                   :at (* 20 minute)}}})
                    "viewer_focus" {})
          lines (answer-lines text)]
      (is (contains? lines (str "Their caret is in a VS Code editor, in src/orchard/trees.clj (orchard.trees), at line 6, "
                                "column 3, inside prune (lines 5-6), 10 minutes ago.")))
      (is (contains? lines "Line 6: (fruit/ripeness (util/label x)))"))
      (is (contains? lines "In the cloud, src/orchard/core.clj is open for editing."))))
  (testing "a caret outside the cloud, a hidden cloud, and no caret"
    (is (string/includes? (ask orchard (context-of orchard {:carets {:latest {:file "/notes/todo.txt" :line 0 :column 0
                                                                              :source :editor :at (* 29 minute)}}})
                               "viewer_focus" {})
                          "in /notes/todo.txt, outside the cloud, at line 1, column 1"))
    (is (string/starts-with? (ask orchard (context-of orchard {:visible? false}) "viewer_focus" {})
                             "The cloud is open, out of sight behind another tab.\nThe person has not placed a caret"))
    (is (= "No cloud is open in VS Code.\nThe person has not placed a caret in a file since VS Code started CodeCloud."
           (ask orchard (context-of orchard {:open? false}) "viewer_focus" {})))))

(deftest answers-say-where-paths-start
  (let [inside (context-of orchard {:workspace-roots ["/work"]})]
    (is (string/ends-with? (ask orchard inside "cloud_overview" {"limit" 1})
                           "\n\nPaths are relative to orchard, the cloud's folder within the workspace."))
    (is (string/starts-with? (ask orchard inside "page_details" {"page" "orchard/src/orchard/fruit.clj"})
                             "orchard.fruit: Clojure namespace in src/orchard/fruit.clj, 12 lines.")
        "a path relative to the workspace folder reaches the page"))
  (is (string/ends-with? (ask orchard (context-of orchard {:workspace-roots ["/elsewhere"]}) "cloud_overview" {"limit" 1})
                         "\n\nPaths are relative to /work/orchard, the cloud's folder.")))

(deftest calls-that-cannot-be-answered
  (is (= {:error "The cloud has no tool named edit_page."} (cloud-tools/answer (context-of orchard) "edit_page" {})))
  (is (= {:error "The cloud has not read the codebase yet. Ask again in a moment."}
         (cloud-tools/answer (context-of orchard {:snapshot nil}) "cloud_overview" {})))
  (is (every? cloud-tools/tool? ["cloud_overview" "page_details" "uses_of" "path_between" "cloud_view" "viewer_focus"
                                 "announce_work" "point_out" "coworkers"])))

(deftest agents-announce-work-and-point-to-lines
  (let [context (context-of orchard)]
    (testing "announce_work names a file or a namespace, and what the work is"
      (is (= {:effect {:kind :announce :path "src/orchard/core.clj" :line 7 :intent "renaming plant" :name nil}
              :text "The person sees this agent working on renaming plant, at src/orchard/core.clj, line 7."}
             (cloud-tools/answer context "announce_work" {"file" "orchard.core" "line" 7 "intent" "renaming plant."})))
      (is (= "The person sees explorer working on mapping the tests."
             (ask orchard context "announce_work" {"intent" "mapping the tests" "name" "explorer"}))
          "a subagent names itself, and work need not name a file")
      (is (= {:kind :announce :path "src/orchard/orchards.clj" :line nil :intent "writing a page" :name nil}
             (:effect (cloud-tools/answer context "announce_work" {"file" "src/orchard/orchards.clj" "intent" "writing a page"})))
          "work may start on a file no page holds yet")
      (is (string/starts-with? (ask orchard context "announce_work" {"file" "orchard.cor" "intent" "fixing"})
                               "ERROR No page is named orchard.cor")
          "but a mistyped namespace is no new file")
      (is (= "ERROR Say what the work is, as intent." (ask orchard context "announce_work" {"file" "orchard.core"}))))
    (testing "point_out pins a note to a line of a page"
      (is (= {:effect {:kind :pin :path "src/orchard/util.clj" :line 4 :note "clamp only trims"}
              :text "Pinned the note to src/orchard/util.clj, line 4. The person clicks the pin to fly there."}
             (cloud-tools/answer context "point_out" {"file" "/work/orchard/src/orchard/util.clj" "line" 4 "note" "clamp only trims"})))
      (is (= "ERROR src/orchard/util.clj has 26 lines, so it has no line 40."
             (ask orchard context "point_out" {"file" "orchard.util" "line" 40 "note" "past the end"})))
      (is (= "ERROR src/orchard/orchards.clj has no page in the cloud, so the person could not see a pin there."
             (ask orchard context "point_out" {"file" "src/orchard/orchards.clj" "line" 1 "note" "here"})))
      (is (= "ERROR Name the line to point to, counting from 1."
             (ask orchard context "point_out" {"file" "orchard.util" "note" "somewhere"}))))))

(deftest calls-say-what-they-read
  (let [context (context-of orchard)]
    (is (= [{:path "src/orchard/util.clj" :start 4 :end 5 :doing "finding uses of clamp"}]
           (cloud-tools/touched-places context "uses_of" {"name" "orchard.util/clamp"}))
        "the definition's lines")
    (is (= [{:path "src/orchard/core.clj" :doing "studying orchard.core"}]
           (cloud-tools/touched-places context "page_details" {"page" "orchard.core"}))
        "a page studied whole")
    (is (= [{:doing "surveying the codebase"}] (cloud-tools/touched-places context "cloud_overview" {})))
    (is (= [{:doing "tracing how orchard.core and orchard.harvest are tied"}]
           (cloud-tools/touched-places context "path_between" {"from" "orchard.core" "to" "orchard.harvest"})))
    (is (nil? (cloud-tools/touched-places context "page_details" {"page" "nowhere"})) "a failed call read nothing")))

(deftest coworkers-list-the-agents-at-work
  (let [core "/work/orchard/src/orchard/core.clj"
        util "/work/orchard/src/orchard/util.clj"
        claude {:id "session-1" :name "Claude Code" :client "claude-code"}
        opencode {:id "session-2" :name "opencode" :client "opencode"}
        worked (-> presence/empty-presence
                   (presence/read-by claude {:path util :start 4 :end 5 :doing "finding uses of clamp"} (* 27 minute))
                   (presence/announced opencode {:path core :line 7 :intent "renaming plant"} (* 26 minute))
                   (presence/changed {:path core :start 6 :end 7 :added 2 :removed 1} (* 28 minute))
                   (presence/pinned opencode {:path core :line 9 :note "grow repeats plant"} (* 29 minute)))
        asked-by (fn [caller present-sessions]
                   (ask orchard
                        (context-of orchard {:presence (presence/summary worked (* 30 minute) present-sessions) :caller caller})
                        "coworkers" {}))
        opencode-row (str "  opencode: at src/orchard/core.clj line 6; intends renaming plant; changing core.clj; "
                          "changed src/orchard/core.clj, lines 6-7, +2 −1, 2 minutes ago, by CodeCloud's guess.")
        pins (str "Pins waiting for the person to click: 1.\n"
                  "  src/orchard/core.clj, line 9, from opencode: grow repeats plant")]
    (is (= (str "Agents working here, the latest active first:\n"
                opencode-row "\n"
                "  Claude Code, this session: at src/orchard/util.clj line 4; finding uses of clamp; "
                "read src/orchard/util.clj, lines 4-5, 3 minutes ago.\n"
                pins)
           (asked-by "session-1" #{"session-1" "session-2"})))
    (testing "an agent that left shows while its work does"
      (is (string/includes? (asked-by "session-2" #{"session-2"}) "  Claude Code, no longer connected: at src/orchard/util.clj")))
    (testing "alone, an agent hears so"
      (is (= "No other agent works here now."
             (ask orchard (context-of orchard {:presence (presence/summary (presence/read-by presence/empty-presence claude {:doing "surveying"} minute)
                                                                           (* 2 minute))
                                               :caller "session-1"})
                  "coworkers" {}))))))

(deftest quieter-reporting-asks-agents-for-less
  (let [names (fn [level] (mapv #(get % "name") (cloud-tools/listed-tools level)))
        announce (fn [level] (some #(when (= "announce_work" (get % "name")) %) (cloud-tools/listed-tools level)))
        properties (fn [level] (set (keys (get-in (announce level) ["inputSchema" "properties"]))))]
    (testing "at full, every tool as the cloud defines it"
      (is (= (mapv #(get % "name") cloud-tools/tools) (names :full)))
      (is (= #{"intent" "file" "line" "name"} (properties :full))))
    (testing "for agents only, a subagent's name goes"
      (is (= (names :full) (names :agents)))
      (is (= #{"intent" "file" "line"} (properties :agents)))
      (is (string/includes? (get (announce :agents) "description") "A subagent leaves it to the agent that started it")))
    (testing "brief withdraws pins and coworkers, and asks for one report a task"
      (is (= ["cloud_overview" "page_details" "uses_of" "path_between" "cloud_view" "viewer_focus" "announce_work"] (names :brief)))
      (is (= #{"intent" "file"} (properties :brief)))
      (is (string/includes? (get (announce :brief) "description") "once per task")))
    (testing "off withdraws every reporting tool"
      (is (= ["cloud_overview" "page_details" "uses_of" "path_between" "cloud_view" "viewer_focus"] (names :off)))))
  (testing "the instructions say only what the level asks"
    (is (string/includes? (tools/instructions :full) "a subagent passes its own name"))
    (is (string/includes? (tools/instructions :agents) "A subagent leaves announce_work to the agent that started it"))
    (is (string/includes? (tools/instructions :brief) "once per task, as the task starts"))
    (is (not-any? #(string/includes? (tools/instructions :off) %) ["announce_work" "point_out" "coworkers"]))
    (is (every? #(string/includes? (tools/instructions %) "cloud_overview gives the codebase's shape") tools/reporting-levels))))

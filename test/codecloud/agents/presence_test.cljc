(ns codecloud.agents.presence-test
  "Checks the bookkeeping of where agents work, from reads, announcements,
  and pins to changes credited to the agent that touched a file last,
  expiry, line diffs, and the rectangles covering lines on a page."
  (:require [clojure.test :refer [deftest is testing]]
            [codecloud.agents.presence :as presence]
            [codecloud.agents.tools :as tools]
            [codecloud.presentation.page :as page]))

(def ^:private minute
  "A minute in milliseconds."
  60000)

(def ^:private claude {:id "session-1" :name "Claude Code" :client "claude-code"})

(def ^:private opencode {:id "session-2" :name "opencode" :client "opencode"})

(def ^:private core "/work/orchard/src/orchard/core.clj")

(def ^:private util "/work/orchard/src/orchard/util.clj")

(defn- agent-of [presence agent-id]
  (get-in presence [:agents agent-id]))

(deftest reads-move-the-badge-and-leave-bands
  (let [read-twice (-> presence/empty-presence
                       (presence/read-by claude {:path core :start 1 :end 40 :doing "reading core.clj"} minute)
                       (presence/read-by claude {:path util :start 4 :end 5 :doing "finding uses of clamp"} (* 2 minute)))
        agent (agent-of read-twice "session-1")]
    (is (= {:path util :line 4} (:place agent)) "the badge sits where the agent read last")
    (is (= "finding uses of clamp" (:doing agent)))
    (is (= [[core 1 40] [util 4 5]] (mapv (juxt :path :start :end) (:reads agent))))
    (is (= ["CC" 235] ((juxt :initials :hue) agent)) "the first agent takes the first hue")
    (testing "a read past its lifetime is gone from the summary"
      (is (= [util] (map :path (:reads (first (:agents (presence/summary read-twice (+ minute presence/read-lifetime 1)))))))))
    (testing "a call reading no file only says what the agent is doing"
      (is (= [core util] (map :path (:reads (agent-of (presence/read-by read-twice claude {:doing "surveying the codebase"} (* 3 minute))
                                                     "session-1"))))))
    (testing "a file read whole says so, since no line is where the agent reads"
      (is (= {:path core :line 1 :whole? true}
             (:place (agent-of (presence/read-by read-twice claude {:path core :doing "studying core"} (* 3 minute)) "session-1")))))
    (testing "a read may place the badge on a line of its own"
      (is (= {:path core :line 9} (:place (agent-of (presence/read-by read-twice claude {:path core :line 9} (* 3 minute)) "session-1")))))
    (testing "the summary says which agents are still connected"
      (is (= [true] (map :present? (:agents (presence/summary read-twice (* 2 minute) #{"session-1"})))))
      (is (= [false] (map :present? (:agents (presence/summary read-twice (* 2 minute) #{}))))))
    (testing "the next agent takes the next hue, and names make initials"
      (is (= ["Op" 150] ((juxt :initials :hue) (agent-of (presence/read-by read-twice opencode {:path core} (* 2 minute)) "session-2"))))
      (is (= ["C" "?" "VC"] (map presence/initials ["C" "" "VS Code"]))))))

(deftest announcements-and-pins
  (let [announced (presence/announced presence/empty-presence claude {:path core :line 7 :intent "renaming plant"} minute)
        subagent {:id "session-1/explorer" :name "explorer" :parent "session-1"}
        with-subagent (presence/announced announced subagent {:intent "mapping the tests"} (* 2 minute))]
    (is (= ["renaming plant" "renaming plant" {:path core :line 7}]
           ((juxt :intent :doing :place) (agent-of announced "session-1"))))
    (is (= ["explorer" "session-1" nil] ((juxt :name :parent :place) (agent-of with-subagent "session-1/explorer")))
        "a subagent appears on its own, without a place until it names a file")
    (let [pinned (-> with-subagent
                     (presence/pinned claude {:path core :line 9 :note "grow repeats plant"} (* 3 minute))
                     (presence/pinned claude {:path util :line 4 :note "clamp only trims"} (* 4 minute)))]
      (is (= [["pin-1" core 9] ["pin-2" util 4]] (mapv (juxt :id :path :line) (:pins pinned))))
      (is (= ["pin-2"] (mapv :id (:pins (presence/unpinned pinned "pin-1")))) "the person dismisses a pin"))))

(deftest changes-go-to-the-agent-that-touched-the-file-last
  (let [worked (-> presence/empty-presence
                   (presence/read-by claude {:path core :start 1 :end 20} minute)
                   (presence/announced opencode {:path core :intent "tidying core"} (* 2 minute)))
        changed (presence/changed worked {:path core :start 6 :end 7 :added 2 :removed 1} (* 3 minute))]
    (is (= "session-2" (presence/credited worked core (* 3 minute))) "the later touch wins")
    (is (= [{:path core :start 6 :end 7 :added 2 :removed 1 :at (* 3 minute) :credited? true}]
           (:changes (agent-of changed "session-2"))))
    (is (= {:path core :line 6} (:place (agent-of changed "session-2"))))
    (testing "a file nobody touched lately goes to an agent without a name"
      (let [unnamed (presence/changed worked {:path util :start 4 :end 4 :added 1 :removed 1} (* 3 minute))
            agent (agent-of unnamed presence/unknown-agent-id)]
        (is (= ["An agent" "?" false] [(:name agent) (:initials agent) (:credited? (first (:changes agent)))]))))
    (testing "touches older than ten minutes credit no one"
      (is (nil? (presence/credited worked core (* 13 minute)))))))

(deftest agents-that-left-go
  (let [worked (-> presence/empty-presence
                   (presence/read-by claude {:path core :start 1 :end 20} minute)
                   (presence/read-by opencode {:path util :start 1 :end 5} minute)
                   (presence/pinned opencode {:path util :line 4 :note "look"} minute))
        later (+ minute presence/read-lifetime 1)]
    (is (= #{"session-1" "session-2"} (set (keys (:agents (presence/expired worked #{"session-1"} minute))))))
    (is (= #{"session-1" "session-2"} (set (keys (:agents (presence/expired worked #{"session-1"} later)))))
        "an agent still present stays, and one that left stays while its pin waits")
    (is (= #{"session-1"} (set (keys (:agents (presence/expired (presence/unpinned worked "pin-1") #{"session-1"} later))))))))

(deftest line-changes
  (is (nil? (presence/line-change ["a" "b"] ["a" "b"])))
  (is (= {:start 2 :end 2 :added 1 :removed 1} (presence/line-change ["a" "b" "c"] ["a" "B" "c"])))
  (is (= {:start 2 :end 4 :added 3 :removed 0} (presence/line-change ["a" "c"] ["a" "x" "y" "z" "c"])))
  (is (= {:start 2 :end 2 :added 0 :removed 2} (presence/line-change ["a" "x" "y" "c"] ["a" "c"])))
  (is (= {:start 1 :end 1 :added 1 :removed 0} (presence/line-change [] ["a"]))))

(deftest spans-cover-lines-across-columns
  (let [files [{:path "src/a.clj" :line-count 40 :longest-line 30}]
        geometry (assoc (page/geometry files 0.6) :columns 2 :rows 20)
        spans (page/line-spans geometry files "src/a.clj" 18 23)]
    (is (= 2 (count spans)) "lines 18 to 23 cross from the first column into the second")
    (is (= [3.0 3.0] (map :height spans)))
    (is (every? #(= (:column-width geometry) (:width %)) spans))
    (is (< (:x (first spans)) (:x (second spans))))
    (is (empty? (page/line-spans geometry files "src/b.clj" 1 2)) "a file the page lacks has none")))

(deftest wavescope-calls-say-what-they-read
  (is (= {:path core :start 1 :end 25 :line 5 :doing "reading core.clj around line 5"}
         (tools/wavescope-read "query_wavelet_context" {"file" core "center" 4} [0 24]))
      "the lines the answer showed in full, counted from 1")
  (is (= {:path core :start 1 :end 65 :line 5 :doing "reading core.clj around line 5"}
         (tools/wavescope-read "query_wavelet_context" {"file" core "center" 4} nil))
      "or else a fifth of the radius on either side of the center")
  (is (= {:path core :start 31 :end 51 :line 41 :doing "reading core.clj around line 41"}
         (tools/wavescope-read "query_wavelet_context" {"file" core "center" 40 "radius" 20} nil))
      "and at least ten lines")
  (is (= {:path core :start 3 :end 9 :doing "summarizing core.clj, lines 3-9"}
         (tools/wavescope-read "get_summary_at_scale" {"file" core "start" 2 "end" 8 "scale" 4} nil)))
  (is (= {:path core :start 1 :end 21 :line 9 :doing "reading around the person's caret"}
         (tools/wavescope-read "get_cursor_context" {"file" core "center" 8} [0 20])))
  (is (= {:path core :line 9 :doing "finding landmarks near the person's caret"}
         (tools/wavescope-read "get_cursor_important_positions" {"file" core "center" 8 "limit" 4} nil)))
  (is (= {:doing "finding landmarks across orchard"}
         (tools/wavescope-read "get_important_positions" {"directory" "/work/orchard" "limit" 3} nil)))
  (is (= {:path util :doing "finding the dense code in util.clj"}
         (tools/wavescope-read "get_complexity_heatmap" {"file" util} nil)))
  (is (= {:path util :doing "comparing util.clj with HEAD~1"}
         (tools/wavescope-read "diff_wavelet_context" {"file" util "baseRef" "HEAD~1"} nil))))

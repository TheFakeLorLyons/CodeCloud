(ns codecloud.agents.roster-test
  "Checks the hub's bookkeeping of connected agents."
  (:require [clojure.test :refer [deftest is testing]]
            [codecloud.agents.roster :as roster]))

(def ^:private minute (* 60 1000))

(deftest agents-are-called-by-familiar-names
  (is (= "Claude Code" (roster/agent-name {"name" "claude-code" "version" "2.1.0"})))
  (is (= "Codex" (roster/agent-name {"name" "codex-mcp-client" "title" "Codex"})))
  (is (= "VS Code" (roster/agent-name {"name" "Visual Studio Code"})))
  (is (= "Fleet Captain" (roster/agent-name {"name" "fleet" "title" "Fleet Captain"})) "a title beats a bare name")
  (is (= "fleet" (roster/agent-name {"name" "fleet"})))
  (is (= "An agent" (roster/agent-name nil)))
  (is (= "An agent" (roster/agent-name {"name" ""}))))

(deftest sessions-come-and-go
  (let [opened (-> {}
                   (roster/opened "first" {"name" "claude-code"} "2025-06-18" 0)
                   (roster/opened "second" {"name" "opencode"} "2025-03-26" (* 3 minute)))]
    (is (= ["Claude Code" "opencode"] (map :name (roster/present-agents opened (* 3 minute)))))
    (is (= {:session "first" :name "Claude Code" :since 0 :calls 0}
           (first (roster/present-agents opened (* 3 minute)))))
    (testing "silence ages an agent out unless it listens on a stream"
      (is (= ["opencode"] (map :name (roster/present-agents opened (* 12 minute)))))
      (let [listening (roster/listening opened "first" (* 1 minute))]
        (is (= ["Claude Code" "opencode"] (map :name (roster/present-agents listening (* 12 minute)))))
        (is (= ["opencode"] (map :name (roster/present-agents (roster/stopped-listening listening "first" (* 1 minute))
                                                              (* 12 minute))))
            "a closed stream counts as speaking last when it closed")))
    (testing "calls keep an agent present and are counted"
      (let [called (-> opened (roster/called "first" (* 9 minute)) (roster/called "first" (* 10 minute)))]
        (is (= 2 (:calls (first (roster/present-agents called (* 15 minute))))))))
    (testing "an ended session leaves, and changes to unknown sessions change nothing"
      (is (= ["opencode"] (map :name (roster/present-agents (roster/closed opened "first") (* 3 minute)))))
      (is (= opened (roster/seen opened "missing" 5)))
      (is (= opened (roster/stopped-listening opened "missing" 5))))
    (testing "streams never count below zero"
      (is (zero? (:streams (get (roster/stopped-listening opened "first" 1) "first")))))))

(deftest agents-with-one-name-are-numbered
  (let [twins (-> {}
                  (roster/opened "b" {"name" "claude-code"} "2025-06-18" 5)
                  (roster/opened "a" {"name" "claude-code"} "2025-06-18" 1)
                  (roster/opened "c" {"name" "opencode"} "2025-06-18" 3))]
    (is (= [["a" "Claude Code"] ["c" "opencode"] ["b" "Claude Code 2"]]
           (map (juxt :session :name) (roster/present-agents twins 10))))))

(deftest a-day-of-silence-forgets-a-session
  (let [day (* 24 60 minute)
        sessions (-> {}
                     (roster/opened "quiet" {"name" "claude-code"} "2025-06-18" 0)
                     (roster/opened "listening" {"name" "opencode"} "2025-06-18" 0)
                     (roster/listening "listening" 0))]
    (is (= #{"quiet" "listening"} (set (keys (roster/forgotten sessions day)))))
    (is (= #{"listening"} (set (keys (roster/forgotten sessions (inc day))))))))

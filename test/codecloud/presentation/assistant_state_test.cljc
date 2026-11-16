(ns codecloud.presentation.assistant-state-test
  "Checks the assistant pane's state as it opens beside the other side
  panes, asks with and without a model, takes the conversation the host
  sends, and finds the places an answer names among the cloud's files."
  (:require [clojure.test :refer [deftest is testing]]
            [codecloud.presentation.assistant-state :as assistant-state]
            [codecloud.presentation.ui-state :as ui-state]))

(defn- run-events [state events]
  (ui-state/handle-all state events))

(defn- sent [effects]
  (keep (fn [[effect-kind message]] (when (= :send effect-kind) message)) effects))

(def ^:private ready-state
  (assoc ui-state/initial-state :assistant (assoc assistant-state/initial :open? true :ready? true :model "m from P")))

(deftest the-pane-shares-the-dock
  (let [{:keys [state]} (run-events (assoc ui-state/initial-state :views-open? true) [[:toggle-assistant]])]
    (is (true? (get-in state [:assistant :open?])))
    (is (true? (:views-open? state)) "the views panel stays open beside it")
    (let [{coworking :state} (run-events state [[:toggle-coworkers]])]
      (is (true? (get-in coworking [:assistant :open?])) "the Coworkers pane opens beside it")
      (is (true? (get-in coworking [:presence :pane-open?]))))
    (is (true? (get-in (:state (run-events state [[:toggle-arrange-panel]])) [:assistant :open?])))
    (is (false? (get-in (:state (run-events (assoc state :views-open? false) [[:escape]])) [:assistant :open?])) "Escape closes it")
    (is (false? (get-in (:state (run-events state [[:toggle-assistant]])) [:assistant :open?])) "its key closes it again")))

(deftest questions-go-to-the-host
  (let [{:keys [state effects]} (run-events ready-state [[:assistant-ask "  How big is this?  "]])]
    (is (= [{:type :view/assistant-ask :text "How big is this?"}] (sent effects)))
    (is (true? (get-in state [:assistant :running?])) "the pane waits at once, so a question goes once")
    (is (empty? (sent (:effects (run-events state [[:assistant-ask "Again?"]]))))
        "nothing more goes while a question runs"))
  (is (empty? (sent (:effects (run-events ready-state [[:assistant-ask "   "]])))) "a blank question goes nowhere")
  (testing "without a model the picker opens"
    (let [{:keys [state effects]} (run-events (assoc-in ready-state [:assistant :ready?] false) [[:assistant-ask "Hello?"]])]
      (is (= [{:type :view/assistant-model}] (sent effects)))
      (is (false? (get-in state [:assistant :running?]))))))

(deftest the-host-sends-the-conversation
  (let [entries [{:id 1 :version 1 :kind :question :text "How big?"}
                 {:id 2 :version 3 :kind :answer :text "Seven pages." :streaming? false}]
        {:keys [state]} (run-events ready-state [[:assistant-ask "How big?"]
                                                 [:host {:type :host/assistant :entries entries :running? false
                                                         :model "claude-test from Anthropic" :ready? true :hub? true}]])]
    (is (= (assoc assistant-state/initial :open? true :entries entries :model "claude-test from Anthropic" :ready? true :hub? true)
           (:assistant state)))
    (let [{started-over :state :keys [effects]} (run-events state [[:assistant-new]])]
      (is (= [] (get-in started-over [:assistant :entries])))
      (is (= [{:type :view/assistant-new}] (sent effects))))
    (is (= [{:type :view/assistant-stop}] (sent (:effects (run-events state [[:assistant-stop]])))))
    (is (= [{:type :view/assistant-model}] (sent (:effects (run-events state [[:assistant-model]])))))))

(deftest answers-name-places-among-the-cloud's-files
  (let [find-place (assistant-state/place-finder ["src/app/core.clj" "src/app/db.clj" "test/app/core.clj" "README.md"])]
    (is (= {:path "src/app/db.clj" :line 4} (find-place "src/app/db.clj:4")))
    (is (= {:path "src/app/db.clj" :line 4} (find-place "./src/app/db.clj:4")))
    (is (= {:path "src/app/db.clj" :line 4} (find-place "db.clj:4")) "a file's name alone, when one file has it")
    (is (nil? (find-place "core.clj:3")) "two files share that name")
    (is (= {:path "src/app/core.clj" :line 3} (find-place "src/app/core.clj:3-9")) "a range starts at its first line")
    (is (= {:path "src/app/core.clj" :line 3} (find-place "/home/someone/app/src/app/core.clj:3"))
        "an absolute path ending in a file of the cloud")
    (is (= {:path "README.md" :line nil} (find-place "README.md")))
    (is (nil? (find-place "notes.txt:1")))))

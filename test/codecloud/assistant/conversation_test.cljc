(ns codecloud.assistant.conversation-test
  "Checks a conversation's turns and entries as a question goes through a
  reply, tool calls, and a final answer."
  (:require [clojure.string :as string]
            [clojure.test :refer [deftest is testing]]
            [codecloud.assistant.conversation :as conversation]))

(deftest a-question-goes-through-tools-to-an-answer
  (let [asked (conversation/asked conversation/empty-conversation "What uses clamp?")
        [streaming answer-id] (conversation/reply-begun asked)
        partway (conversation/reply-streamed streaming answer-id "Let me")
        call {:id "call-a" :name "uses_of" :input {"name" "orchard.util/clamp"} :input-json "{}"}
        replied (conversation/replied partway answer-id {:text "Let me look." :tool-calls [call]})
        [running tool-id] (conversation/tool-begun replied call "Uses of a definition")
        ended (conversation/tool-ended running tool-id {:text "5 uses" :error? false})
        answered (conversation/tools-answered ended [{:id "call-a" :name "uses_of" :text "5 uses" :error? false}])]
    (is (= [:question] (map :kind (:entries asked))))
    (is (= {:id 2 :version 2 :kind :answer :text "Let me" :streaming? true} (second (:entries partway))))
    (is (= [[:user 0] [:assistant 1] [:tool 0]]
           (map (juxt :role (comp count :tool-calls)) (:turns answered)))
        "the turns hold the question, the reply with its call, and the tool's answer")
    (is (= [:question :answer :tool] (map :kind (:entries answered))))
    (is (= {:id 3 :version 2 :kind :tool :name "uses_of" :title "Uses of a definition" :target "orchard.util/clamp"
            :status :done :result "5 uses"}
           (nth (:entries answered) 2)))
    (testing "a reply that says nothing leaves no entry"
      (let [[silent quiet-id] (conversation/reply-begun answered)]
        (is (= 3 (count (:entries (conversation/replied silent quiet-id {:text "" :tool-calls []})))))
        (is (= 3 (count (:turns (conversation/replied silent quiet-id {:text "" :tool-calls []})))) "nor any turn")))))

(deftest targets-name-what-a-call-is-about
  (is (= "src/app/core.clj:13" (conversation/tool-target {"file" "src/app/core.clj" "center" 12})))
  (is (= "src/app/core.clj:4" (conversation/tool-target {"file" "src/app/core.clj" "line" 4 "note" "here"})))
  (is (= "app.core to app.db" (conversation/tool-target {"from" "app.core" "to" "app.db"})))
  (is (nil? (conversation/tool-target {"limit" 3}))))

(deftest results-and-prompts
  (is (string/ends-with? (conversation/clipped-result (apply str (repeat (inc conversation/most-result-characters) "x")))
                         "[The result went on; ask for less to see the rest.]"))
  (is (= "short" (conversation/clipped-result "short")))
  (is (= [{:name "uses_of" :title "Uses" :description "Every use" :schema {"type" "object"}}]
         (conversation/tool-definitions [{"name" "uses_of" "title" "Uses" "description" "Every use" "inputSchema" {"type" "object"}
                                          "annotations" {"readOnlyHint" true}}])))
  (let [prompt (conversation/system-prompt "Use cloud_overview first." "orchard" true)]
    (is (string/includes? prompt "called orchard"))
    (is (string/includes? prompt "Use cloud_overview first."))
    (is (string/includes? prompt "path:line"))
    (is (string/includes? prompt "point_out")))
  (is (not (string/includes? (conversation/system-prompt "Use cloud_overview first." "orchard" false) "point_out"))
      "without pins offered, the prompt leaves point_out out")
  (let [shown-result (:result (first (conversation/shown {:entries [{:id 1 :kind :tool :result (apply str (repeat 4001 "y"))}]})))]
    (is (and (string/ends-with? shown-result "\n…") (= 4002 (count shown-result))) "a long result shows cut short")))

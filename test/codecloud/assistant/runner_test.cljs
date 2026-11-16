(ns codecloud.assistant.runner-test
  "Runs questions through the assistant against stand-in Anthropic and
  OpenAI-style servers, with a stand-in for the hub's tools, and reads
  the models each lists."
  (:require [cljs.test :refer [async deftest is testing]]
            [codecloud.assistant.conversation :as conversation]
            [codecloud.assistant.providers :as providers]
            [codecloud.assistant.runner :as runner]
            [codecloud.assistant.stand-ins :as stand-ins]))

(defn- provider-at [port kind]
  {:kind kind
   :base-url (case kind
               :anthropic (str "http://127.0.0.1:" port "/anthropic")
               :openai-chat (str "http://127.0.0.1:" port "/openai"))})

(def ^:private tools
  [{:name "cloud_overview" :title "The codebase at a glance" :description "The codebase's shape" :schema {"type" "object"}}])

(defn- question-run
  "Resolves to {:conversation :tool-calls :changes} after `question` ran
  against `provider` with `api-key`, the tool answering with the
  orchard's first line. `stop-after` stops the question once that many
  changes were heard."
  [provider api-key model question stop-after]
  (let [held (atom conversation/empty-conversation)
        changes (atom 0)
        tool-calls (atom [])
        usages (atom [])]
    (.then (runner/run-question!
            {:conversation held
             :on-change (fn [_] (swap! changes inc))
             :ask! (fn [turns on-progress]
                     (.then (runner/stream-reply! provider
                                                  (providers/reply-request provider api-key model
                                                                           {:system "Answer from the tools." :turns turns :tools tools
                                                                            :max-tokens conversation/reply-token-cap})
                                                  nil
                                                  on-progress)
                            (fn [reply] (swap! usages conj (:usage reply)) reply)))
             :run-tool! (fn [tool-name input]
                          (swap! tool-calls conj [tool-name input])
                          (js/Promise.resolve {"content" [{"type" "text" "text" "orchard: 17 pages in 17 files\nMore lines."}]}))
             :titles {"cloud_overview" "The codebase at a glance"}
             :stopped? #(and stop-after (>= @changes stop-after))}
            question)
           (fn [_] {:conversation @held :tool-calls @tool-calls :changes @changes :usages @usages}))))

(defn- entry-summary [entries]
  (mapv (fn [{entry-name :name :keys [kind text status]}]
          (case kind
            :tool [kind status entry-name]
            [kind text]))
        entries))

(deftest questions-go-through-tools-with-either-kind-of-provider
  (async done
    (let [stand-in (atom nil)]
      (-> (stand-ins/start!)
          (.then (fn [started]
                   (reset! stand-in started)
                   (question-run (provider-at (:port started) :anthropic) stand-ins/test-key "claude-test-large" "How big is this?" nil)))
          (.then (fn [{:keys [conversation tool-calls changes usages]}]
                   (testing "Anthropic"
                     (is (= [[:question "How big is this?"]
                             [:answer "Let me look."]
                             [:tool :done "cloud_overview"]
                             [:answer "The overview says: orchard: 17 pages in 17 files"]]
                            (entry-summary (:entries conversation))))
                     (is (= [["cloud_overview" {"limit" 3}]] tool-calls) "the tool runs with the arguments streamed in pieces")
                     (is (= [:user :assistant :tool :assistant] (map :role (:turns conversation))))
                     (is (< 6 changes) "the pane hears the reply as it streams")
                     (let [{:keys [headers body]} (last @(:requests @stand-in))]
                       (is (= "test-key" (get headers "x-api-key")))
                       (is (= [{"type" "tool_result" "tool_use_id" "toolu_1" "content" "orchard: 17 pages in 17 files\nMore lines."}]
                              (get-in body ["messages" 2 "content"]))
                           "the tool's answer goes back under the tool use's id")
                       (is (= {"type" "ephemeral"} (get body "cache_control")) "each request asks for the conversation to be cached"))
                     (is (= [[600 0] [0 600]] (map (juxt #(get % "cache_creation_input_tokens") #(get % "cache_read_input_tokens")) usages))
                         "the first turn is written to the cache and the next reads it back"))
                   (question-run (provider-at (:port @stand-in) :openai-chat) stand-ins/test-key "test-chat" "How big is this?" nil)))
          (.then (fn [{:keys [conversation tool-calls]}]
                   (testing "OpenAI's chat completions"
                     (is (= ["Let me look." "The overview says: orchard: 17 pages in 17 files"]
                            (map :text (filter #(= :answer (:kind %)) (:entries conversation)))))
                     (is (= [["cloud_overview" {"limit" 3}]] tool-calls))
                     (let [{:keys [body]} (last @(:requests @stand-in))]
                       (is (= {"role" "tool" "tool_call_id" "call_1" "content" "orchard: 17 pages in 17 files\nMore lines."}
                              (last (get body "messages"))))
                       (is (= "{\"limit\": 3}" (get-in body ["messages" 2 "tool_calls" 0 "function" "arguments"]))
                           "the arguments go back as streamed")))
                   (question-run (provider-at (:port @stand-in) :openai-chat) "wrong-key" "test-chat" "Hello?" nil)))
          (.then (fn [{:keys [conversation]}]
                   (is (= [:notice "The provider answered 401, refusing the key: Incorrect API key provided"]
                          (last (entry-summary (:entries conversation))))
                       "a refused key says so in the pane")
                   (question-run (provider-at (:port @stand-in) :anthropic) stand-ins/test-key "claude-test-large" "Stop soon." 3)))
          (.then (fn [{:keys [conversation]}]
                   (is (= [:notice "Stopped."] (last (entry-summary (:entries conversation)))) "the person stops a question")
                   (is (= :tool (:role (last (:turns conversation)))) "every tool call it made still has an answer")
                   (js/Promise.all #js [(runner/list-models! (provider-at (:port @stand-in) :anthropic) stand-ins/test-key)
                                        (runner/list-models! (provider-at (:port @stand-in) :openai-chat) stand-ins/test-key)])))
          (.then (fn [^js lists]
                   (is (= [{:id "claude-test-large" :label "Claude Test Large"} {:id "claude-test-small" :label "Claude Test Small"}]
                          (aget lists 0)))
                   (is (= ["test-chat" "test-mini"] (map :id (aget lists 1))) "an embedding model is no chat model")
                   (runner/list-models! {:kind :openai-chat :base-url "http://127.0.0.1:9/v1"} nil)))
          (.then (fn [_] (is false "an unreachable server should fail")))
          (.catch (fn [^js error]
                    (is (re-find #"^CodeCloud could not reach http://127\.0\.0\.1:9/v1/models" (.-message error)) (.-message error))))
          (.finally (fn []
                      (if-let [running @stand-in]
                        (.finally (stand-ins/stop! running) done)
                        (done))))))))

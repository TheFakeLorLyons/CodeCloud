(ns codecloud.assistant.providers-test
  "Checks the requests to both kinds of provider, the turns in each API's
  shape, server-sent events, and streamed replies folded into text and
  tool calls."
  (:require [clojure.test :refer [deftest is testing]]
            [codecloud.assistant.providers :as providers]))

(def ^:private anthropic {:kind :anthropic :base-url "https://api.anthropic.com/"})

(def ^:private ollama {:kind :openai-chat :base-url "http://localhost:1234/v1"})

(def ^:private turns
  [{:role :user :text "What does clamp do?"}
   {:role :assistant :text "Let me look."
    :tool-calls [{:id "call-a" :name "uses_of" :input {"name" "clamp"} :input-json "{\"name\":\"clamp\"}"}]}
   {:role :tool :results [{:id "call-a" :name "uses_of" :text "5 uses" :error? false}]}
   {:role :assistant :text "" :tool-calls []}])

(def ^:private tools [{:name "uses_of" :description "Every use" :schema {"type" "object"}}])

(deftest models-are-listed-by-each-api
  (is (= {:method "GET" :url "https://api.anthropic.com/v1/models?limit=100"
          :headers {"anthropic-version" "2023-06-01" "content-type" "application/json" "x-api-key" "secret"}}
         (providers/models-request anthropic "secret")))
  (is (= {:method "GET" :url "http://localhost:1234/v1/models" :headers {"content-type" "application/json"}}
         (providers/models-request ollama nil))
      "a local server needs no key")
  (is (= [{:id "claude-sonnet-4-5" :label "Claude Sonnet 4.5"} {:id "claude-haiku-4-5" :label "claude-haiku-4-5"}]
         (providers/model-list anthropic {"data" [{"id" "claude-sonnet-4-5" "display_name" "Claude Sonnet 4.5"}
                                                  {"id" "claude-haiku-4-5"}]})))
  (is (= ["gpt-4o" "llama3.2"]
         (map :id (providers/model-list ollama {"data" [{"id" "text-embedding-3-small"} {"id" "llama3.2"}
                                                       {"id" "whisper-1"} {"id" "gpt-4o"} {"id" "dall-e-3"}]})))
      "an OpenAI-style list keeps its chat models, by name"))

(deftest replies-are-asked-in-each-shape
  (testing "Anthropic"
    (let [{:keys [url headers body]} (providers/reply-request anthropic "secret" "claude-sonnet-4-5"
                                                             {:system "Be brief." :turns turns :tools tools :max-tokens 4096})]
      (is (= "https://api.anthropic.com/v1/messages" url))
      (is (= "secret" (get headers "x-api-key")))
      (is (= ["claude-sonnet-4-5" 4096 "Be brief." true] (map #(get body %) ["model" "max_tokens" "system" "stream"])))
      (is (= [{"role" "user" "content" "What does clamp do?"}
              {"role" "assistant" "content" [{"type" "text" "text" "Let me look."}
                                             {"type" "tool_use" "id" "call-a" "name" "uses_of" "input" {"name" "clamp"}}]}
              {"role" "user" "content" [{"type" "tool_result" "tool_use_id" "call-a" "content" "5 uses"}]}]
             (get body "messages"))
          "tool results travel in a user message, and an empty assistant turn stays out")
      (is (= [{"name" "uses_of" "description" "Every use" "input_schema" {"type" "object"}}] (get body "tools")))
      (is (= {"type" "ephemeral"} (get body "cache_control")) "the conversation is cached for five minutes")
      (is (= {"type" "ephemeral" "ttl" "1h"}
             (get-in (providers/reply-request anthropic "secret" "claude-sonnet-4-5"
                                              {:system "Be brief." :turns turns :tools tools :max-tokens 4096 :cache-lifetime "1h"})
                     [:body "cache_control"]))
          "or for an hour when asked")))
  (testing "OpenAI's chat completions"
    (let [{:keys [url headers body]} (providers/reply-request ollama "key" "llama3.2" {:system "Be brief." :turns turns :tools tools})]
      (is (= "http://localhost:1234/v1/chat/completions" url))
      (is (= "Bearer key" (get headers "authorization")))
      (is (= [{"role" "system" "content" "Be brief."}
              {"role" "user" "content" "What does clamp do?"}
              {"role" "assistant" "content" "Let me look."
               "tool_calls" [{"id" "call-a" "type" "function" "function" {"name" "uses_of" "arguments" "{\"name\":\"clamp\"}"}}]}
              {"role" "tool" "tool_call_id" "call-a" "content" "5 uses"}]
             (get body "messages"))
          "the arguments go back as the model wrote them")
      (is (= [{"type" "function" "function" {"name" "uses_of" "description" "Every use" "parameters" {"type" "object"}}}]
             (get body "tools")))
      (is (not (contains? (:body (providers/reply-request ollama nil "llama3.2" {:system "" :turns [] :tools []})) "tools"))
          "no tools, no tools key"))))

(deftest server-sent-events-split-where-chunks-fall
  (let [[events rest-text] (providers/sse-events "event: ping\ndata: {}\n\ndata: {\"a\":\r\ndata: 1}\n\ndata: {\"b\"")]
    (is (= [{:event "ping" :data "{}"} {:event nil :data "{\"a\":\n1}"}] events))
    (is (= "data: {\"b\"" rest-text) "a partial event waits for the next chunk")
    (is (= [[{:event nil :data "{\"b\":2}"}] ""] (providers/sse-events (str rest-text ":2}\n\n"))))
    (is (= [[] ": comment only"] (providers/sse-events ": comment only")))))

(deftest anthropic-streams-fold-into-text-and-tool-calls
  (let [events [{"type" "message_start" "message" {"id" "m1" "usage" {"input_tokens" 12 "cache_read_input_tokens" 4096
                                                                     "cache_creation_input_tokens" 0 "output_tokens" 1}}}
                {"type" "content_block_start" "index" 0 "content_block" {"type" "text" "text" ""}}
                {"type" "content_block_delta" "index" 0 "delta" {"type" "text_delta" "text" "Let me"}}
                {"type" "content_block_delta" "index" 0 "delta" {"type" "text_delta" "text" " look."}}
                {"type" "content_block_stop" "index" 0}
                {"type" "content_block_start" "index" 1 "content_block" {"type" "tool_use" "id" "toolu_1" "name" "uses_of" "input" {}}}
                {"type" "content_block_delta" "index" 1 "delta" {"type" "input_json_delta" "partial_json" "{\"name\": "}}
                {"type" "content_block_delta" "index" 1 "delta" {"type" "input_json_delta" "partial_json" "\"clamp\"}"}}
                {"type" "content_block_stop" "index" 1}
                {"type" "content_block_start" "index" 2 "content_block" {"type" "tool_use" "id" "toolu_2" "name" "cloud_overview" "input" {}}}
                {"type" "content_block_stop" "index" 2}
                {"type" "message_delta" "delta" {"stop_reason" "tool_use"} "usage" {"output_tokens" 40}}
                {"type" "message_stop"}]
        reply (providers/completed (reduce providers/anthropic-event providers/empty-reply events))]
    (is (= {:text "Let me look."
            :tool-calls [{:id "toolu_1" :name "uses_of" :input-json "{\"name\": \"clamp\"}"}
                         {:id "toolu_2" :name "cloud_overview" :input-json "{}"}]
            :stop "tool_use"
            :error nil}
           (dissoc reply :usage))
        "a tool use with no arguments gets an empty object")
    (is (= "4096 input tokens read from the cache, 0 written to it, 12 uncached, and 40 out"
           (providers/usage-words (:usage reply)))
        "the tokens counted at the start and the end of the stream"))
  (is (= "First.\n\nThen more."
         (:text (reduce providers/anthropic-event providers/empty-reply
                        [{"type" "content_block_start" "index" 0 "content_block" {"type" "text" "text" "First."}}
                         {"type" "content_block_start" "index" 1 "content_block" {"type" "text" "text" ""}}
                         {"type" "content_block_delta" "index" 1 "delta" {"type" "text_delta" "text" "Then more."}}])))
      "blocks of text are set apart")
  (is (= "Overloaded" (:error (providers/anthropic-event providers/empty-reply {"type" "error" "error" {"message" "Overloaded"}})))))

(deftest openai-streams-fold-into-text-and-tool-calls
  (let [chunks [{"choices" [{"delta" {"role" "assistant" "content" ""}}]}
                {"choices" [{"delta" {"content" "Let me"}}]}
                {"choices" [{"delta" {"content" " look."}}]}
                {"choices" [{"delta" {"tool_calls" [{"index" 0 "id" "call_1" "type" "function" "function" {"name" "uses_of" "arguments" ""}}]}}]}
                {"choices" [{"delta" {"tool_calls" [{"index" 0 "function" {"arguments" "{\"name\":"}}]}}]}
                {"choices" [{"delta" {"tool_calls" [{"index" 0 "function" {"arguments" "\"clamp\"}"}}]}}]}
                {"choices" [{"delta" {"tool_calls" [{"index" 1 "function" {"name" "cloud_overview" "arguments" "{}"}}]}}]}
                {"choices" [{"delta" {} "finish_reason" "tool_calls"}]}]]
    (is (= {:text "Let me look."
            :tool-calls [{:id "call_1" :name "uses_of" :input-json "{\"name\":\"clamp\"}"}
                         {:id "call-1" :name "cloud_overview" :input-json "{}"}]
            :stop "tool_calls"
            :error nil
            :usage nil}
           (providers/completed (reduce providers/openai-chunk providers/empty-reply chunks)))
        "a call arriving without an id is given one from its place"))
  (is (= "model not found" (:error (providers/openai-chunk providers/empty-reply {"error" {"message" "model not found"}})))))

(deftest errors-say-what-the-provider-said
  (is (= "The provider answered 401, refusing the key: invalid x-api-key"
         (providers/error-message 401 {"type" "error" "error" {"type" "authentication_error" "message" "invalid x-api-key"}})))
  (is (= "The provider answered 404, finding nothing there: model 'llama9' not found"
         (providers/error-message 404 {"error" "model 'llama9' not found"})))
  (is (= "The provider answered 500." (providers/error-message 500 nil))))

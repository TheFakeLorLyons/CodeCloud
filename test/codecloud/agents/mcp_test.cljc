(ns codecloud.agents.mcp-test
  "Checks how the hub classifies, routes, and answers MCP messages."
  (:require [clojure.test :refer [deftest is testing]]
            [codecloud.agents.mcp :as mcp]))

(def ^:private server
  {:info {"name" "codecloud" "version" "0.4.2-1"}
   :instructions "Outline files before reading them."
   :tools [{"name" "query_wavelet_context" "inputSchema" {"type" "object"}}]})

(defn- request [id method params]
  (mcp/request id method params))

(deftest messages-are-classified
  (is (= :request (mcp/message-kind {"jsonrpc" "2.0" "id" 1 "method" "ping"})))
  (is (= :request (mcp/message-kind {"jsonrpc" "2.0" "id" "a" "method" "ping"})) "an id may be a string")
  (is (= :notification (mcp/message-kind {"jsonrpc" "2.0" "method" "notifications/initialized"})))
  (is (= :response (mcp/message-kind {"jsonrpc" "2.0" "id" 1 "result" {}})))
  (is (= :response (mcp/message-kind {"jsonrpc" "2.0" "id" 1 "error" {"code" -1 "message" "no"}})))
  (is (= :invalid (mcp/message-kind {"id" 1 "method" "ping"})) "the version must be 2.0")
  (is (= :invalid (mcp/message-kind {"jsonrpc" "2.0" "id" nil "method" "ping"})) "a request's id is never null")
  (is (= :invalid (mcp/message-kind {"jsonrpc" "2.0" "id" {} "method" "ping"})))
  (is (= :invalid (mcp/message-kind [1 2])))
  (is (= :invalid (mcp/message-kind "ping"))))

(deftest initialize-agrees-on-a-revision
  (let [{:keys [reply opened]} (mcp/route server (request 1 "initialize" {"protocolVersion" "2025-06-18"
                                                                          "capabilities" {}
                                                                          "clientInfo" {"name" "claude-code" "version" "2.1.0"}}))]
    (is (= {"jsonrpc" "2.0"
            "id" 1
            "result" {"protocolVersion" "2025-06-18"
                      "capabilities" {"tools" {"listChanged" true}}
                      "serverInfo" {"name" "codecloud" "version" "0.4.2-1"}
                      "instructions" "Outline files before reading them."}}
           reply))
    (is (= {:client {"name" "claude-code" "version" "2.1.0"} :version "2025-06-18"} opened)))
  (testing "a revision the hub does not speak is answered with the newest it does"
    (is (= (first mcp/supported-versions)
           (get-in (mcp/route server (request 2 "initialize" {"protocolVersion" "2099-01-01"}))
                   [:reply "result" "protocolVersion"])))
    (is (= "2024-11-05" (mcp/negotiated-version "2024-11-05")))
    (is (= (first mcp/supported-versions) (mcp/negotiated-version nil)))))

(deftest requests-are-answered-or-passed-on
  (is (= {:reply {"jsonrpc" "2.0" "id" 3 "result" {}}} (mcp/route server (request 3 "ping" nil))))
  (is (= [{"name" "query_wavelet_context" "inputSchema" {"type" "object"}}]
         (get-in (mcp/route server (request 4 "tools/list" {})) [:reply "result" "tools"])))
  (is (= {:call {:id 5 :name "query_wavelet_context" :arguments {"file" "src/a.clj" "center" 3}}}
         (mcp/route server (request 5 "tools/call" {"name" "query_wavelet_context"
                                                    "arguments" {"file" "src/a.clj" "center" 3}}))))
  (is (= {:call {:id 6 :name "query_wavelet_context" :arguments {}}}
         (mcp/route server (request 6 "tools/call" {"name" "query_wavelet_context"})))
      "a call without arguments has none")
  (testing "mistakes come back as JSON-RPC errors"
    (is (= -32602 (get-in (mcp/route server (request 7 "tools/call" {"name" "rm_rf"})) [:reply "error" "code"])))
    (is (= -32602 (get-in (mcp/route server (request 8 "tools/call" {"name" "query_wavelet_context" "arguments" [1]}))
                          [:reply "error" "code"])))
    (is (= -32602 (get-in (mcp/route server (request 9 "tools/list" [1])) [:reply "error" "code"])))
    (is (= {"jsonrpc" "2.0" "id" 10 "error" {"code" -32601 "message" "The hub does not handle resources/list"}}
           (:reply (mcp/route server (request 10 "resources/list" {})))))
    (is (= {"jsonrpc" "2.0" "id" 21 "error" {"code" -32600 "message" "Not a JSON-RPC 2.0 message"}}
           (:reply (mcp/route server {"id" 21 "method" "ping"})))
        "an invalid message keeps its id when it has a usable one")
    (is (nil? (get-in (mcp/route server {"jsonrpc" "2.0" "id" {} "method" "ping"}) [:reply "id"])))))

(deftest notifications-and-responses-need-no-answer
  (is (= {:notified "notifications/initialized"}
         (mcp/route server (mcp/notification "notifications/initialized" nil))))
  (is (= {} (mcp/route server {"jsonrpc" "2.0" "id" 1 "result" {}}))))

(deftest tool-results
  (is (= {"content" [{"type" "text" "text" "found"}]} (mcp/text-result "found")))
  (is (= {"content" [{"type" "text" "text" "no such file"}] "isError" true} (mcp/failed-result "no such file"))))

(ns codecloud.io.mcp-stdio-test
  "Checks the stdio MCP client against a stand-in server run as a child
  process, answering the way an MCP server does."
  (:require ["fs" :as fs]
            ["os" :as os]
            ["path" :as path]
            [cljs.test :refer [async deftest is]]
            [clojure.string :as string]
            [codecloud.io.mcp-stdio :as mcp-stdio]))

(def ^:private stand-in-source
  "A server that answers initialize, lists one tool, pings the client
  once initialized, and acts out a failing, a silent, and a crashing
  tool."
  (string/join
   "\n"
   ["const readline = require('readline');"
    "const lines = readline.createInterface({input: process.stdin});"
    "let answeredPing = false;"
    "const send = (message) => process.stdout.write(JSON.stringify(message) + '\\n');"
    "lines.on('line', (line) => {"
    "  const message = JSON.parse(line);"
    "  if (message.method === 'initialize') {"
    "    send({jsonrpc: '2.0', id: message.id, result: {protocolVersion: message.params.protocolVersion,"
    "          capabilities: {tools: {}}, serverInfo: {name: 'stand-in', version: '1'}}});"
    "  } else if (message.method === 'notifications/initialized') {"
    "    send({jsonrpc: '2.0', id: 'from-server', method: 'ping'});"
    "  } else if (message.id === 'from-server') {"
    "    answeredPing = Boolean(message.result);"
    "  } else if (message.method === 'tools/list') {"
    "    send({jsonrpc: '2.0', id: message.id, result: {tools: [{name: 'echo', inputSchema: {type: 'object'}}]}});"
    "  } else if (message.method === 'tools/call') {"
    "    const name = message.params.name;"
    "    if (name === 'echo') {"
    "      const text = JSON.stringify(message.params.arguments) + (answeredPing ? ' after a ping' : '');"
    "      const reply = JSON.stringify({jsonrpc: '2.0', id: message.id, result: {content: [{type: 'text', text}]}});"
    "      process.stdout.write(reply.slice(0, 9));"
    "      setTimeout(() => process.stdout.write(reply.slice(9) + '\\n'), 20);"
    "    } else if (name === 'fail') {"
    "      send({jsonrpc: '2.0', id: message.id, error: {code: -32602, message: 'fail is not a tool'}});"
    "    } else if (name === 'crash') {"
    "      process.stderr.write('crashing now\\n');"
    "      process.exit(3);"
    "    }"
    "  }"
    "});"]))

(defn- stand-in-path []
  (let [directory (.mkdtempSync fs (path/join (.tmpdir os) "codecloud-mcp-"))
        script (path/join directory "stand-in.js")]
    (.writeFileSync fs script stand-in-source)
    script))

(deftest a-session-with-a-child-server
  (async done
    (let [client-holder (atom nil)
          exits (atom [])
          logged (atom [])]
      (-> (mcp-stdio/start! {:command (.-execPath js/process)
                             :arguments [(stand-in-path)]
                             :environment {"CODECLOUD_TEST" "1"}
                             :client-info {"name" "codecloud" "version" "test"}
                             :on-exit (fn [_client reason] (swap! exits conj reason))
                             :on-log #(swap! logged conj %)})
          (.then (fn [client]
                   (reset! client-holder client)
                   (is (= {"name" "stand-in" "version" "1"} (:server-info client)))
                   (mcp-stdio/list-tools! client)))
          (.then (fn [tools]
                   (is (= ["echo"] (map #(get % "name") tools)))
                   (mcp-stdio/call-tool! @client-holder "echo" {"words" "hello" "lines" [1 2]})))
          (.then (fn [result]
                   (is (= [{"type" "text" "text" "{\"words\":\"hello\",\"lines\":[1,2]} after a ping"}] (get result "content"))
                       "the answer, split across writes, arrives whole, and the client answered the server's ping")
                   (mcp-stdio/call-tool! @client-holder "fail" {})))
          (.then (fn [_result] (is false "a JSON-RPC error should reject"))
                 (fn [error]
                   (is (= "MCP_-32602" (.-code error)))
                   (is (= "fail is not a tool" (.-message error)))))
          (.then (fn [_]
                   (mcp-stdio/request! @client-holder "tools/call" {"name" "silent"} 50)))
          (.then (fn [_result] (is false "a silent server should time out"))
                 (fn [error] (is (= "MCP_SILENT" (.-code error)))))
          (.then (fn [_]
                   (mcp-stdio/call-tool! @client-holder "crash" {})))
          (.then (fn [_result] (is false "a crash should reject"))
                 (fn [error]
                   (is (= "MCP_EXITED" (.-code error)))
                   (is (string/includes? (.-message error) "with code 3: crashing now"))
                   (is (= ["crashing now"] @logged) "standard error reaches the log")
                   (is (= 1 (count @exits)))
                   (is (not (mcp-stdio/running? @client-holder)))
                   (mcp-stdio/list-tools! @client-holder)))
          (.then (fn [_tools] (is false "a stopped server cannot answer"))
                 (fn [error] (is (= "MCP_EXITED" (.-code error)))))
          (.catch (fn [error] (is (nil? error) (str "the session failed: " (.-message error)))))
          (.finally (fn []
                      (some-> @client-holder mcp-stdio/stop!)
                      (done)))))))

(deftest a-server-that-cannot-start
  (async done
    (-> (mcp-stdio/start! {:command "codecloud-no-such-program"
                           :arguments []
                           :client-info {"name" "codecloud" "version" "test"}})
        (.then (fn [_client] (is false "a missing program should not start"))
               (fn [error] (is (= "MCP_EXITED" (.-code error)))))
        (.finally done))))

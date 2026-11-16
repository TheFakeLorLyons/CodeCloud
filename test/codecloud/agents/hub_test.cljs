(ns codecloud.agents.hub-test
  "Runs the hub with the wavescope-mcp bundle the build makes, dist/wavescope.mjs,
  against the polyglot fixture, calling it as an agent would."
  (:require ["http" :as http]
            ["path" :as path]
            [cljs.test :refer [async deftest is testing]]
            [clojure.string :as string]
            [codecloud.agents.hub :as hub]
            [codecloud.agents.scripted-agent :as agent]
            [codecloud.agents.tools :as tools]
            [codecloud.fixtures :as fixtures]
            [codecloud.io.files :as files]
            [codecloud.io.mcp-client :as mcp-client]
            [codecloud.io.mcp-stdio :as mcp-stdio]))

(def ^:private token "hub-test-token")

(def ^:private fixture-root (path/resolve "test-resources/polyglot"))

(def ^:private wavescope
  {:command (.-execPath js/process)
   :arguments [(path/resolve "dist/wavescope.mjs")]
   :environment {}})

(defn- start-hub! [present]
  (hub/start! {:address "127.0.0.1"
               :port 0
               :token token
               :version "test"
               :roots (constantly [fixture-root])
               :wavescope wavescope
               :on-agents #(reset! present %)}))

(defn- result-text [result]
  (get-in result ["content" 0 "text"]))

(deftest an-agent-reads-through-the-hub
  (async done
    (let [hub-holder (atom nil)
          agent-holder (atom nil)
          present (atom [])]
      (-> (start-hub! present)
          (.then (fn [started]
                   (reset! hub-holder started)
                   (is (= (str "http://127.0.0.1:" (:port started) "/mcp") (hub/url started)))
                   (agent/connect! (:port started) token {"name" "claude-code" "version" "2.1.0"})))
          (.then (fn [connected]
                   (reset! agent-holder connected)
                   (is (= ["Claude Code"] (map :name @present)) "the hub reports the agent as present")
                   (agent/ask! connected "tools/list" {})))
          (.then (fn [answer]
                   (is (= ["cloud_overview" "page_details" "uses_of" "path_between" "cloud_view" "viewer_focus"
                           "announce_work" "point_out" "coworkers"
                           "query_wavelet_context" "get_summary_at_scale" "get_important_positions"
                           "diff_wavelet_context" "get_complexity_heatmap" "get_cursor_context"
                           "get_cursor_important_positions"]
                          (map #(get % "name") (get-in answer ["result" "tools"]))))
                   (agent/call-tool! @agent-holder "query_wavelet_context" {"file" "src/app/core.cljs" "center" 4})))
          (.then (fn [result]
                   (is (not (get result "isError")) (result-text result))
                   (is (contains? (js->clj (js/JSON.parse (result-text result))) "bands")
                       "a relative path reaches wavescope as an absolute one")
                   (agent/call-tool! @agent-holder "get_important_positions" {"limit" 3})))
          (.then (fn [result]
                   (is (not (get result "isError")) "with neither file nor directory, the workspace folder is searched")
                   (is (= 3 (count (get (js->clj (js/JSON.parse (result-text result))) "positions"))))
                   (agent/call-tool! @agent-holder "get_summary_at_scale" {"file" "../../package.json" "start" 0 "end" 5})))
          (.then (fn [result]
                   (is (true? (get result "isError")))
                   (is (string/includes? (result-text result) "outside the workspace folders"))
                   (agent/call-tool! @agent-holder "get_complexity_heatmap" {"file" "/etc/hostname"})))
          (.then (fn [result]
                   (is (true? (get result "isError")) "an absolute path outside the workspace is refused too")
                   (agent/call-tool! @agent-holder "get_complexity_heatmap" {"file" "src/missing.py"})))
          (.then (fn [result]
                   (is (true? (get result "isError")) "a missing file is a failed result, not a broken session")))
          (.catch (fn [error] (is (nil? error) (str "the hub failed: " (.-message error)))))
          (.finally (fn []
                      (if-let [running @hub-holder]
                        (.finally (hub/stop! running) done)
                        (done))))))))

(defn- property-names [tool]
  (set (keys (get-in tool ["inputSchema" "properties"]))))

(deftest the-listed-tools-match-wavescope
  (async done
    (let [client-holder (atom nil)]
      (-> (mcp-stdio/start! (assoc wavescope :client-info {"name" "codecloud-test" "version" "test"}))
          (.then (fn [client]
                   (reset! client-holder client)
                   (mcp-stdio/list-tools! client)))
          (.then (fn [wavescope-tools]
                   (let [by-name (into {} (map (juxt #(get % "name") identity)) wavescope-tools)]
                     (doseq [listed tools/wavescope-tools
                             :let [tool-name (get listed "name")
                                   original (get by-name tool-name)]]
                       (testing tool-name
                         (is (some? original) "wavescope still offers the tool")
                         (is (= (property-names original) (property-names listed)) "the arguments keep wavescope's names")
                         (is (= (cond-> (set (get-in original ["inputSchema" "required"]))
                                  (contains? tools/cursor-tools tool-name) (disj "file"))
                                (set (get-in listed ["inputSchema" "required"])))
                             "the same arguments are required, but for the caret's file, which the hub fills in"))))))
          (.catch (fn [error] (is (nil? error) (str "wavescope failed: " (.-message error)))))
          (.finally (fn []
                      (some-> @client-holder mcp-stdio/stop!)
                      (done)))))))

(defn- caret-at
  "A caret on `line` of the fixture's file at `relative-path`, counting
  from zero, placed in an editor a moment ago."
  [relative-path line]
  {:file (path/join fixture-root relative-path) :line line :column 4 :anchor-line line :anchor-column 4
   :source :editor :at (js/Date.now)})

(deftest the-cloud-answers-through-the-hub
  (async done
    (let [snapshot (fixtures/snapshot-of "test-resources/polyglot")
          caret (caret-at "src/eeg/filters.py" 8)
          carets {:latest caret :by-file {(:file caret) caret}}
          hub-holder (atom nil)
          agent-holder (atom nil)
          call! (fn [tool-name arguments] (agent/call-tool! @agent-holder tool-name arguments))]
      (-> (hub/start! {:address "127.0.0.1"
                       :port 0
                       :token token
                       :version "test"
                       :roots (constantly [fixture-root])
                       :wavescope wavescope
                       :cloud (fn []
                                (js/Promise.resolve
                                 {:snapshot snapshot :cloud-name "polyglot" :root fixture-root :workspace-roots [fixture-root]
                                  :named-views {} :focus nil :open? true :visible? true :carets carets
                                  :read-text #(files/read-text (files/absolute-path fixture-root %))}))
                       :carets (constantly carets)})
          (.then (fn [started]
                   (reset! hub-holder started)
                   (agent/connect! (:port started) token {"name" "opencode" "version" "1.19.5"})))
          (.then (fn [connected]
                   (reset! agent-holder connected)
                   (call! "cloud_overview" {"limit" 3})))
          (.then (fn [result]
                   (is (string/starts-with? (result-text result) "polyglot: 33 pages in 33 files") (result-text result))
                   (call! "uses_of" {"name" "bandpass"})))
          (.then (fn [result]
                   (is (string/includes? (result-text result) "  src/eeg/stream.py:20 in Stream.read: filtered = bandpass(raw, 1, 50, board.RATE)")
                       "the hub reads the files that may use the definition")
                   (call! "viewer_focus" {})))
          (.then (fn [result]
                   (is (string/includes? (result-text result)
                                         "Their caret is in a VS Code editor, in src/eeg/filters.py, at line 9, column 5, inside bandpass")
                       (result-text result))
                   (call! "page_details" {"page" "nowhere.clj"})))
          (.then (fn [result]
                   (is (true? (get result "isError")) "a page that does not exist is a failed result")
                   (call! "get_cursor_context" {})))
          (.then (fn [result]
                   (is (not (get result "isError")) (result-text result))
                   (is (= 8 (get (js->clj (js/JSON.parse (result-text result))) "center"))
                       "wavescope reads around the caret the hub told it of")
                   (call! "get_cursor_important_positions" {"limit" 2})))
          (.then (fn [result]
                   (is (= 2 (count (js->clj (js/JSON.parse (result-text result))))))
                   (call! "get_cursor_context" {"file" "src/app/core.cljs"})))
          (.then (fn [result]
                   (is (true? (get result "isError")))
                   (is (= "The person's caret has not been in src/app/core.cljs since CodeCloud started; it is in src/eeg/filters.py."
                          (result-text result)))))
          (.catch (fn [error] (is (nil? error) (str "the hub failed: " (.-message error)))))
          (.finally (fn []
                      (if-let [running @hub-holder]
                        (.finally (hub/stop! running) done)
                        (done))))))))

(defn- agent-named
  "The agent called `agent-name` in a presence summary."
  [summary agent-name]
  (some #(when (= agent-name (:name %)) %) (:agents summary)))

(deftest agents-show-where-they-work
  (async done
    (let [snapshot (fixtures/snapshot-of "test-resources/polyglot")
          filters (path/join fixture-root "src/eeg/filters.py")
          core (path/join fixture-root "src/app/core.cljs")
          caret (caret-at "src/eeg/filters.py" 8)
          carets {:latest caret :by-file {(:file caret) caret}}
          hub-holder (atom nil)
          claude-holder (atom nil)
          opencode-holder (atom nil)
          summaries (atom [])
          latest #(peek @summaries)]
      (-> (hub/start! {:address "127.0.0.1"
                       :port 0
                       :token token
                       :version "test"
                       :roots (constantly [fixture-root])
                       :wavescope wavescope
                       :cloud (fn []
                                (js/Promise.resolve
                                 {:snapshot snapshot :cloud-name "polyglot" :root fixture-root :workspace-roots [fixture-root]
                                  :named-views {} :focus nil :open? true :visible? true :carets carets
                                  :read-text #(files/read-text (files/absolute-path fixture-root %))}))
                       :carets (constantly carets)
                       :on-presence #(swap! summaries conj %)})
          (.then (fn [started]
                   (reset! hub-holder started)
                   (is (false? (hub/note-change! started {:path filters :start 1 :end 1 :added 1 :removed 1}))
                       "with no agent connected, no agent made a change")
                   (agent/connect! (:port started) token {"name" "claude-code" "version" "2.1.0"})))
          (.then (fn [connected]
                   (reset! claude-holder connected)
                   (agent/connect! (:port @hub-holder) token {"name" "opencode" "version" "1.19.5"})))
          (.then (fn [connected]
                   (reset! opencode-holder connected)
                   (agent/call-tool! @claude-holder "query_wavelet_context" {"file" "src/app/core.cljs" "center" 2})))
          (.then (fn [result]
                   (let [[first-shown last-shown] (get-in (js->clj (js/JSON.parse (result-text result))) ["bands" "fine" "range"])
                         claude (agent-named (latest) "Claude Code")]
                     (is (= [core (inc first-shown) (inc last-shown)] ((juxt :path :start :end) (peek (:reads claude))))
                         "a read through wavescope leaves a band over the lines it showed in full")
                     (is (= [{:path core :line 3} "reading core.cljs around line 3" true]
                            ((juxt :place :doing :present?) claude))))
                   (agent/call-tool! @opencode-holder "announce_work" {"file" "src/eeg/filters.py" "line" 9 "intent" "tuning the bandpass"})))
          (.then (fn [result]
                   (is (= "The person sees this agent working on tuning the bandpass, at src/eeg/filters.py, line 9." (result-text result)))
                   (is (= [{:path filters :line 9} "tuning the bandpass"] ((juxt :place :intent) (agent-named (latest) "opencode"))))
                   (agent/call-tool! @opencode-holder "announce_work" {"intent" "mapping the tests" "name" "explorer"})))
          (.then (fn [_]
                   (is (= [(str (:session @opencode-holder) "/explorer") (:session @opencode-holder) "mapping the tests" true]
                          ((juxt :id :parent :doing :present?) (agent-named (latest) "explorer")))
                       "a subagent appears on its own, under its parent's session")
                   (agent/call-tool! @claude-holder "point_out" {"file" "src/eeg/filters.py" "line" 9 "note" "the cutoff is fixed"})))
          (.then (fn [_]
                   (is (= [["pin-1" filters 9 "the cutoff is fixed"]] (map (juxt :id :path :line :note) (:pins (latest)))))
                   (is (true? (hub/note-change! @hub-holder {:path filters :start 9 :end 10 :added 2 :removed 1})))
                   (is (= [[filters 9 10 true]] (map (juxt :path :start :end :credited?) (:changes (agent-named (latest) "opencode"))))
                       "the change goes to the agent that touched the file last, as a guess")
                   (agent/call-tool! @claude-holder "coworkers" {})))
          (.then (fn [result]
                   (is (string/starts-with? (result-text result) "Agents working here, the latest active first:\n") (result-text result))
                   (is (string/includes? (result-text result)
                                         "  opencode: at src/eeg/filters.py line 9; intends tuning the bandpass; changing filters.py; changed src/eeg/filters.py, lines 9-10, +2 −1, a moment ago, by CodeCloud's guess.")
                       (result-text result))
                   (is (string/includes? (result-text result) "  explorer, a subagent of opencode: mapping the tests.") (result-text result))
                   (is (string/includes? (result-text result) "  src/eeg/filters.py, line 9, from Claude Code: the cutoff is fixed"))
                   (agent/call-tool! @claude-holder "get_cursor_context" {})))
          (.then (fn [result]
                   (let [[first-shown last-shown] (get-in (js->clj (js/JSON.parse (result-text result))) ["bands" "fine" "range"])
                         claude (agent-named (latest) "Claude Code")]
                     (is (= [filters (inc first-shown) (inc last-shown) 9 "reading around the person's caret"]
                            [(:path (peek (:reads claude))) (:start (peek (:reads claude))) (:end (peek (:reads claude)))
                             (:line (:place claude)) (:doing claude)])
                         "a read around the caret puts the badge on the caret's line"))
                   (agent/call-tool! @claude-holder "page_details" {"page" "nowhere.clj"})))
          (.then (fn [_]
                   (is (= "reading around the person's caret" (:doing (agent-named (latest) "Claude Code"))) "a failed call reads nothing")
                   (hub/dismiss-pin! @hub-holder "pin-1")
                   (is (empty? (:pins (latest))) "the person dismisses the pin")
                   (is (= (latest) (hub/presence-summary @hub-holder)))))
          (.catch (fn [error] (is (nil? error) (str "the hub failed: " (.-message error)))))
          (.finally (fn []
                      (if-let [running @hub-holder]
                        (.finally (hub/stop! running) done)
                        (done))))))))

(deftest the-assistant-reaches-the-hub-as-a-client
  (async done
    (let [snapshot (fixtures/snapshot-of "test-resources/polyglot")
          hub-holder (atom nil)
          client-holder (atom nil)
          present (atom [])
          summaries (atom [])]
      (-> (hub/start! {:address "127.0.0.1"
                       :port 0
                       :token token
                       :version "test"
                       :roots (constantly [fixture-root])
                       :wavescope wavescope
                       :cloud (fn []
                                (js/Promise.resolve
                                 {:snapshot snapshot :cloud-name "polyglot" :root fixture-root :workspace-roots [fixture-root]
                                  :named-views {} :focus nil :open? true :visible? true :carets nil
                                  :read-text #(files/read-text (files/absolute-path fixture-root %))}))
                       :on-agents #(reset! present %)
                       :on-presence #(swap! summaries conj %)})
          (.then (fn [started]
                   (reset! hub-holder started)
                   (mcp-client/connect! {:url (hub/url started) :token token
                                         :client-info {"name" "codecloud-assistant" "title" "Assistant (test-model)" "version" "test"}})))
          (.then (fn [client]
                   (reset! client-holder client)
                   (is (some? (:session client)) "the hub opens a session for the assistant")
                   (mcp-client/list-tools! client)))
          (.then (fn [listed]
                   (is (= 16 (count listed)))
                   (is (= ["Assistant (test-model)"] (map :name @present)) "the assistant counts among the agents present")
                   (mcp-client/call-tool! @client-holder "page_details" {"page" "src/eeg/filters.py"})))
          (.then (fn [result]
                   (is (not (get result "isError")) (str result))
                   (is (string/starts-with? (get-in result ["content" 0 "text"]) "src/eeg/filters.py") (str result))
                   (is (= ["AT" "studying src/eeg/filters.py"]
                          ((juxt :initials :doing) (first (:agents (peek @summaries))))))
                   (mcp-client/call-tool! @client-holder "point_out" {"file" "nowhere.py" "line" 1 "note" "x"})))
          (.then (fn [result]
                   (is (true? (get result "isError")) "a failed call comes back as a failed result")
                   (mcp-client/close! @client-holder)))
          (.then (fn [_]
                   (is (empty? @present) "closing the client ends its session")))
          (.catch (fn [error] (is (nil? error) (str "the assistant's client failed: " (.-message error)))))
          (.finally (fn []
                      (if-let [running @hub-holder]
                        (.finally (hub/stop! running) done)
                        (done))))))))

(defn- listen!
  "Opens `agent`'s event stream on the hub, adding what arrives to the
  `heard` atom. Returns the request, to abort when done."
  [{:keys [port token session]} heard]
  (doto (.get http #js {:host "127.0.0.1" :port port :path hub/endpoint
                        :headers (clj->js (assoc (agent/headers-for token session) "Accept" "text/event-stream"))}
              (fn [^js response] (.on response "data" #(swap! heard str %))))
    (.on "error" (fn [_error] nil))))

(defn- heard-within
  "Resolves to true once `heard` holds `text`, or to false after a while."
  [heard text]
  (js/Promise.
   (fn [resolve _reject]
     (let [started (js/Date.now)
           patience 3000]
       (letfn [(check [] (cond
                           (string/includes? @heard text) (resolve true)
                           (> (- (js/Date.now) started) patience) (resolve false)
                           :else (js/setTimeout check 10)))]
         (check))))))

(deftest reporting-follows-the-level-the-person-sets
  (async done
    (let [level (atom :full)
          snapshot (fixtures/snapshot-of "test-resources/polyglot")
          hub-holder (atom nil)
          agent-holder (atom nil)
          stream-holder (atom nil)
          heard (atom "")
          summaries (atom [])
          names (fn [answer] (mapv #(get % "name") (get-in answer ["result" "tools"])))]
      (-> (hub/start! {:address "127.0.0.1"
                       :port 0
                       :token token
                       :version "test"
                       :roots (constantly [fixture-root])
                       :wavescope wavescope
                       :reporting #(deref level)
                       :cloud (fn []
                                (js/Promise.resolve
                                 {:snapshot snapshot :cloud-name "polyglot" :root fixture-root :workspace-roots [fixture-root]
                                  :named-views {} :focus nil :open? true :visible? true :carets {}
                                  :read-text #(files/read-text (files/absolute-path fixture-root %))}))
                       :on-presence #(swap! summaries conj %)})
          (.then (fn [started]
                   (reset! hub-holder started)
                   (agent/connect! (:port started) token {"name" "claude-code" "version" "2.1.0"})))
          (.then (fn [connected]
                   (reset! agent-holder connected)
                   (reset! stream-holder (listen! connected heard))
                   (agent/ask! connected "tools/list" {})))
          (.then (fn [answer]
                   (is (= 16 (count (names answer))))
                   (reset! level :agents)
                   (hub/reporting-changed! @hub-holder)
                   (heard-within heard "notifications/tools/list_changed")))
          (.then (fn [heard?]
                   (is heard? "connected agents hear that their tools changed")
                   (agent/call-tool! @agent-holder "announce_work" {"intent" "mapping the tests" "name" "explorer"})))
          (.then (fn [_]
                   (is (= [["Claude Code" "mapping the tests"]] (map (juxt :name :intent) (:agents (peek @summaries))))
                       "below full, a subagent's report joins its agent's badge")
                   (reset! level :off)
                   (agent/ask! @agent-holder "tools/list" {})))
          (.then (fn [answer]
                   (is (= 13 (count (names answer))))
                   (is (not-any? #{"announce_work" "point_out" "coworkers"} (names answer)))
                   (agent/ask! @agent-holder "tools/call" {"name" "announce_work" "arguments" {"intent" "anything"}})))
          (.then (fn [answer]
                   (is (some? (get answer "error")) "a withdrawn tool cannot be called")))
          (.catch (fn [error] (is (nil? error) (str "the hub failed: " (.-message error)))))
          (.finally (fn []
                      (some-> ^js @stream-holder .destroy)
                      (if-let [running @hub-holder]
                        (.finally (hub/stop! running) done)
                        (done))))))))

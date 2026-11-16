(ns codecloud.presentation.page-commands-test
  (:require [clojure.test :refer [deftest is testing]]
            [codecloud.presentation.page-commands :as page-commands]
            [codecloud.presentation.ui-state :as ui-state]))

(def ^:private clay
  {:title "Clay: show this file"
   :command "calva.runCustomREPLCommand"
   :args {"snippet" "(scicloj.clay.v2.api/make! {:source-path \"$file\"})"}
   :window "http://localhost:1971"})

(def ^:private snapshot
  {:id :working-tree
   :namespaces {"notes.intro" {:id "src/notes/intro.clj" :name "notes.intro" :importance 1.0
                               :files [{:path "src/notes/intro.clj" :line-count 40}]}}
   :edges []})

(defn- sent [effects message-type]
  (keep (fn [[effect-kind message]] (when (and (= :send effect-kind) (= message-type (:type message))) message)) effects))

(deftest commands-come-from-the-setting
  (is (= clay (page-commands/from-setting {"title" " Clay: show this file "
                                           "command" "calva.runCustomREPLCommand"
                                           "args" {"snippet" "(scicloj.clay.v2.api/make! {:source-path \"$file\"})"}
                                           "window" 1971}))
      "a port alone names this computer's")
  (is (= {:title "Notebook server" :shell "jupyter lab --port 8888" :window "http://localhost:8888/lab"}
         (page-commands/from-setting {"title" "Notebook server" "shell" "jupyter lab --port 8888" "window" "8888/lab"})))
  (is (= {:title "Both" :command "calva.loadFile"} (page-commands/from-setting {"title" "Both" "command" "calva.loadFile" "shell" "ls"}))
      "a command wins over a command line")
  (is (nil? (page-commands/from-setting {"title" "Nothing to run"})))
  (is (nil? (page-commands/from-setting {"command" "calva.loadFile"})) "a command needs a title")
  (is (nil? (page-commands/from-setting "calva.loadFile"))))

(deftest command-lines-take-the-file-and-line
  (is (= "clojure -M -m notes \"/work/src/notes/intro.clj\" src/notes/intro.clj 12"
         (page-commands/command-line "clojure -M -m notes \"${file}\" ${relativeFile} ${line}"
                                     {:file "/work/src/notes/intro.clj" :relative-file "src/notes/intro.clj" :line 12}))))

(deftest a-page-runs-its-commands
  (let [loaded (:state (ui-state/handle-all ui-state/initial-state
                                            [[:host {:type :host/init :settings {:page-commands [clay]}}]
                                             [:host {:type :host/snapshot :snapshot snapshot}]]))]
    (testing "a command runs at the start of the page's file and shows its window"
      (let [{:keys [state effects]} (ui-state/handle loaded [:run-page-command 0 "notes.intro"])]
        (is (= [{:title "Clay: show this file" :command "calva.runCustomREPLCommand"
                 :args {"snippet" "(scicloj.clay.v2.api/make! {:source-path \"$file\"})"}
                 :path "src/notes/intro.clj" :line 1 :column 1}]
               (map #(dissoc % :type :protocol/version) (sent effects :view/run-page-command))))
        (is (= ["http://localhost:1971"] (map :source (:windows state))))
        (testing "and run again, loads the window it showed afresh"
          (let [{again :state again-effects :effects} (ui-state/handle state [:run-page-command 0 "notes.intro"])]
            (is (= 1 (count (:windows again))))
            (is (= 1 (count (sent again-effects :view/window-open))))))))
    (testing "while the page is being edited, a command runs at the caret"
      (let [editing (-> loaded
                        (assoc :editing {:namespace-id "notes.intro" :path "src/notes/intro.clj"})
                        (assoc-in [:buffers "src/notes/intro.clj" :caret] [6 4]))]
        (is (= [{:path "src/notes/intro.clj" :line 7 :column 5}]
               (map #(select-keys % [:path :line :column])
                    (sent (:effects (ui-state/handle editing [:pick-page-command "notes.intro"])) :view/pick-page-command))))))
    (testing "the host asks for a command for the page in hand"
      (let [asked (fn [state] (ui-state/handle state [:host {:type :host/pick-page-command}]))
            place-of (fn [{:keys [effects]}] (map :path (sent effects :view/pick-page-command)))]
        (is (= ["src/notes/intro.clj"] (place-of (asked (assoc loaded :focused-id "notes.intro"))))
            "the page brought closer")
        (is (= ["src/notes/intro.clj"] (place-of (asked (assoc loaded :hovered-id "notes.intro"))))
            "or the page under the pointer")
        (let [{:keys [state effects]} (asked loaded)]
          (is (empty? (sent effects :view/pick-page-command)))
          (is (= ["Click a page first, then run a command for it."] (:notices state))
              "and says what to do without one"))))
    (testing "the keys pressed in the cloud ask the same, with no host between"
      (let [pressed (fn [state] (ui-state/handle state [:pick-command-for-page-in-hand]))]
        (is (= ["src/notes/intro.clj"]
               (map :path (sent (:effects (pressed (assoc loaded :focused-id "notes.intro"))) :view/pick-page-command))))
        (is (= ["Click a page first, then run a command for it."] (:notices (:state (pressed loaded)))))))
    (testing "a page or command that is gone runs nothing"
      (is (empty? (sent (:effects (ui-state/handle loaded [:run-page-command 4 "notes.intro"])) :view/run-page-command)))
      (is (empty? (:effects (ui-state/handle loaded [:pick-page-command "notes.gone"])))))))

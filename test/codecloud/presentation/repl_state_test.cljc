(ns codecloud.presentation.repl-state-test
  "Checks the REPL pane's state as tabs of REPLs and shells open and take
  what is typed into them, the pane follows the page last clicked, code
  from the page being edited evaluates in Calva's scopes and loads, and
  the pane remembers its size."
  (:require [clojure.test :refer [deftest is testing]]
            [codecloud.presentation.repl-state :as repl-state]
            [codecloud.presentation.ui-state :as ui-state]))

(def ^:private snapshot
  {:id :working-tree
   :namespaces {"app.core" {:id "app.core" :name "app.core" :kind :namespace :importance 1.0
                            :files [{:path "src/app/core.clj" :dialect "clj"}] :requires ["app.db" "app.util"]}
                "app.db" {:id "app.db" :name "app.db" :kind :namespace :importance 0.6
                          :files [{:path "src/app/db.clj" :dialect "clj"}] :requires ["app.util"]
                          :definitions [{:name "add" :kind :function :line 2 :end-line 3 :path "src/app/db.clj"}]}
                "app.util" {:id "app.util" :name "app.util" :kind :namespace :importance 0.8
                            :files [{:path "src/app/util.cljc" :dialect "cljc"}] :requires []}
                "app.ui" {:id "app.ui" :name "app.ui" :kind :namespace :importance 0.3
                          :files [{:path "src/app/ui.cljs" :dialect "cljs"}] :requires ["app.util" "app.core"]}
                "app.loop-a" {:id "app.loop-a" :name "app.loop-a" :kind :namespace :importance 0.2
                              :files [{:path "src/app/loop_a.clj" :dialect "clj"}] :requires ["app.loop-b"]}
                "app.loop-b" {:id "app.loop-b" :name "app.loop-b" :kind :namespace :importance 0.2
                              :files [{:path "src/app/loop_b.clj" :dialect "clj"}] :requires ["app.loop-a" "app.util"]}
                "web/page.js" {:id "web/page.js" :name "web/page.js" :kind :file :importance 0.1
                               :files [{:path "web/page.js"}] :requires []}}
   :edges []})

(defn- sent-messages [effects]
  (keep (fn [[effect-kind message]] (when (= :send effect-kind) (dissoc message :protocol/version))) effects))

(defn- loaded []
  (:state (ui-state/handle ui-state/initial-state [:host {:type :host/snapshot :snapshot snapshot}])))

(defn- log-of [state tab]
  (mapv (juxt :kind :text) (get-in state [:repl :sessions tab :log])))

(defn- connected [state tab]
  (:state (ui-state/handle state [:host {:type :host/repl-status :tab tab :status :connected :port 7888
                                         :message "nbb on port 7888"}])))

(deftest opening-the-pane
  (let [{opened :state :keys [effects]} (ui-state/handle ui-state/initial-state [:toggle-repl])]
    (testing "opening the pane the first time opens a REPL tab that connects"
      (is (get-in opened [:repl :open?]))
      (is (= ["tab-1"] (get-in opened [:repl :tabs])))
      (is (= "tab-1" (get-in opened [:repl :active]) (get-in opened [:repl :evaluating])))
      (is (= :connecting (get-in opened [:repl :sessions "tab-1" :status])))
      (is (= [{:type :view/repl-connect :tab "tab-1"}] (sent-messages effects))))
    (testing "closing it says nothing, and reopening a disconnected tab connects it"
      (let [{closed :state closing-effects :effects} (ui-state/handle opened [:toggle-repl])
            disconnected (:state (ui-state/handle closed [:host {:type :host/repl-status :tab "tab-1" :status :disconnected}]))]
        (is (= [false []] [(get-in closed [:repl :open?]) closing-effects]))
        (is (= [{:type :view/repl-connect :tab "tab-1"}] (sent-messages (:effects (ui-state/handle disconnected [:toggle-repl])))))))
    (testing "after a failure, connecting again waits for the viewer"
      (let [failed (:state (ui-state/handle-all opened [[:toggle-repl]
                                                        [:host {:type :host/repl-status :tab "tab-1" :status :failed
                                                                :message "No nREPL port file."}]]))]
        (is (get-in failed [:repl :open?]) "a failure opens the pane, which says why")
        (is (= [[:info "No nREPL port file."]] (log-of failed "tab-1"))
            "the log notes each change")))))

(deftest tabs-of-repls-and-shells
  (let [{with-repl :state} (ui-state/handle ui-state/initial-state [:toggle-repl])
        {with-shell :state shell-effects :effects} (ui-state/handle with-repl [:repl-new-tab :shell])]
    (testing "a shell tab opens a terminal and shows"
      (is (= ["tab-1" "tab-2"] (get-in with-shell [:repl :tabs])))
      (is (= "tab-2" (get-in with-shell [:repl :active])))
      (is (= "tab-1" (get-in with-shell [:repl :evaluating])) "code from pages still goes to the REPL tab")
      (is (= [{:type :view/shell-open :tab "tab-2"}] (sent-messages shell-effects))))
    (testing "a shell tab can join a terminal VS Code has open"
      (is (= [{:type :view/shell-open :tab "tab-3" :terminal "terminal-7"}]
             (sent-messages (:effects (ui-state/handle with-shell [:repl-new-tab :shell "terminal-7"]))))))
    (testing "the menu of new tabs asks for VS Code's terminals and lists them"
      (let [{menu :state menu-effects :effects} (ui-state/handle with-shell [:repl-menu])]
        (is (get-in menu [:repl :menu-open?]))
        (is (= [{:type :view/list-terminals}] (sent-messages menu-effects)))
        (is (= [{:id "terminal-7" :name "zsh"}]
               (get-in (:state (ui-state/handle menu [:host {:type :host/terminals :terminals [{:id "terminal-7" :name "zsh"}]}]))
                       [:repl :terminals])))))
    (testing "choosing a REPL tab makes it the one that evaluates"
      (let [two-repls (:state (ui-state/handle with-shell [:repl-new-tab :repl]))]
        (is (= "tab-3" (get-in two-repls [:repl :evaluating])))
        (is (= "tab-1" (get-in (:state (ui-state/handle two-repls [:repl-select-tab "tab-1"])) [:repl :evaluating])))
        (is (= "tab-3" (get-in (:state (ui-state/handle two-repls [:repl-select-tab "tab-2"])) [:repl :evaluating]))
            "choosing a shell tab leaves the REPL that evaluates as it was")))
    (testing "closing a tab shows its neighbor and lets go of its connection or terminal"
      (let [{closed :state closing-effects :effects} (ui-state/handle with-shell [:repl-close-tab "tab-2"])
            {last-closed :state} (ui-state/handle closed [:repl-close-tab "tab-1"])]
        (is (= ["tab-1"] (get-in closed [:repl :tabs])))
        (is (= "tab-1" (get-in closed [:repl :active])))
        (is (= [{:type :view/repl-close :tab "tab-2"}] (sent-messages closing-effects)))
        (is (= [nil nil []] ((juxt #(get-in % [:repl :active]) #(get-in % [:repl :evaluating]) #(get-in % [:repl :tabs])) last-closed)))
        (is (empty? (:effects (ui-state/handle last-closed [:repl-close-tab "tab-9"]))))))))

(deftest a-tab-joined-to-calvas-repl
  (let [{menu :state} (ui-state/handle-all ui-state/initial-state
                                           [[:toggle-repl]
                                            [:host {:type :host/terminals :terminals [] :calva? true}]])
        {joined :state join-effects :effects} (ui-state/handle menu [:repl-new-tab :calva])]
    (testing "with Calva there, the menu offers its REPL, and the tab joins it"
      (is (get-in menu [:repl :calva?]))
      (is (= [{:type :view/repl-connect :tab "tab-2" :via :calva}] (sent-messages join-effects)))
      (is (= [:connecting "Joining Calva's REPL…" :calva] ((juxt :status :message :via) (get-in joined [:repl :sessions "tab-2"])))))
    (testing "a port typed into it is code, and connecting again joins Calva again"
      (let [waiting (:state (ui-state/handle joined [:host {:type :host/repl-status :tab "tab-2" :status :failed :via :calva
                                                            :message "Calva has no REPL connected yet. Jack in or connect with Calva, and the tab joins it."}]))]
        (is (= :view/repl-eval (:type (first (sent-messages (:effects (ui-state/handle waiting [:repl-submit "7888"]))))))
            "Calva tabs never connect to a port")
        (is (= [{:type :view/repl-connect :tab "tab-2" :via :calva}] (sent-messages (:effects (ui-state/handle waiting [:repl-connect])))))))
    (testing "code Calva evaluated elsewhere shows as input with its output"
      (let [mirrored (:state (ui-state/handle-all joined
                                                  [[:host {:type :host/repl-output :tab "tab-2" :id "calva-1" :input "(+ 1 1)" :ns "app.db"}]
                                                   [:host {:type :host/repl-output :tab "tab-2" :id "calva-1" :value "2" :ns "app.db"}]]))]
        (is (= [[:input "app.db=> (+ 1 1)"] [:value "2"]] (vec (take-last 2 (log-of mirrored "tab-2")))))))
    (testing "a reloaded view learns which tabs join Calva"
      (is (= :calva (get-in (:state (ui-state/handle ui-state/initial-state
                                                     [:host {:type :host/repl-status :tab "tab-5" :status :connected :via :calva
                                                             :message "Calva's REPL, session clj"}]))
                            [:repl :sessions "tab-5" :via]))))))

(deftest typing-into-a-repl-tab
  (let [opened (:state (ui-state/handle ui-state/initial-state [:toggle-repl]))]
    (testing "a port typed while disconnected connects there"
      (is (= [{:type :view/repl-connect :tab "tab-1" :port 7888}]
             (sent-messages (:effects (ui-state/handle opened [:repl-submit " 7888 "])))))
      (is (= [{:type :view/repl-connect :tab "tab-1" :host "localhost" :port 7888}]
             (sent-messages (:effects (ui-state/handle opened [:repl-submit "localhost:7888"]))))))
    (testing "code evaluates in the tab's namespace, which follows the pane's own replies"
      (let [ready (connected opened "tab-1")
            {sent :state sent-effects :effects} (ui-state/handle ready [:repl-submit "(in-ns 'app.db)"])
            answered (:state (ui-state/handle-all sent [[:host {:type :host/repl-output :tab "tab-1" :id "eval-0"
                                                                :value "#namespace[app.db]" :ns "app.db"}]
                                                        [:host {:type :host/repl-output :tab "tab-1" :id "eval-0" :status ["done"]}]]))
            {again :state again-effects :effects} (ui-state/handle answered [:repl-submit "7888"])]
        (is (= [{:type :view/repl-eval :tab "tab-1" :id "eval-0" :code "(in-ns 'app.db)"}] (sent-messages sent-effects)))
        (is (= [:input "user=> (in-ns 'app.db)"] (last (log-of sent "tab-1"))))
        (is (= "app.db" (get-in answered [:repl :sessions "tab-1" :ns])))
        (is (empty? (get-in answered [:repl :sessions "tab-1" :pending])))
        (is (= [{:type :view/repl-eval :tab "tab-1" :id "eval-1" :code "7888" :ns "app.db"}] (sent-messages again-effects))
            "a number is code once connected")
        (is (= ["(in-ns 'app.db)" "7888"] (get-in again [:repl :sessions "tab-1" :history])))))
    (testing "the host's notes, output, and errors join the log, one entry for each stream"
      (let [state (:state (ui-state/handle-all opened
                                               [[:host {:type :host/repl-output :tab "tab-1" :id "eval-0" :note "Loaded its ns form first."}]
                                                [:host {:type :host/repl-output :tab "tab-1" :id "eval-0" :out "a"}]
                                                [:host {:type :host/repl-output :tab "tab-1" :id "eval-0" :out "b\n"}]
                                                [:host {:type :host/repl-output :tab "tab-1" :id "eval-0" :err "oops\n"}]]))]
        (is (= [[:info "Loaded its ns form first."] [:out "ab\n"] [:err "oops\n"]] (log-of state "tab-1")))
        (is (empty? (get-in (:state (ui-state/handle state [:repl-clear])) [:repl :sessions "tab-1" :log])))))
    (testing "interrupting asks the host only while something runs"
      (is (empty? (:effects (ui-state/handle opened [:repl-interrupt]))))
      (is (= [{:type :view/repl-interrupt :tab "tab-1"}]
             (sent-messages (:effects (ui-state/handle (assoc-in opened [:repl :sessions "tab-1" :pending "eval-0"] {:origin :pane})
                                                       [:repl-interrupt]))))))
    (testing "Ctrl+Alt+C Ctrl+Alt+D interrupts every REPL tab still evaluating, whichever tab shows"
      (let [two-running (:state (ui-state/handle-all opened [[:repl-new-tab :repl]
                                                             [:repl-new-tab :shell]]))
            evaluating (-> two-running
                           (assoc-in [:repl :sessions "tab-1" :pending "eval-0"] {:origin :pane})
                           (assoc-in [:repl :sessions "tab-2" :pending "eval-1"] {:origin :page}))]
        (is (= [{:type :view/repl-interrupt :tab "tab-1"} {:type :view/repl-interrupt :tab "tab-2"}]
               (sent-messages (:effects (ui-state/handle evaluating [:interrupt-evaluations])))))
        (is (empty? (:effects (ui-state/handle two-running [:interrupt-evaluations]))))))))

(deftest typing-into-a-shell-tab
  (let [opened (:state (ui-state/handle-all ui-state/initial-state
                                            [[:repl-new-tab :shell]
                                             [:host {:type :host/shell-status :tab "tab-1" :status :ready :label "bash"
                                                     :cwd "/work/orchard" :message "Opened a terminal running bash."}]]))
        {running :state run-effects :effects} (ui-state/handle opened [:repl-submit "ls -a"])]
    (testing "a command runs in the tab's terminal, shown after the folder it runs in"
      (is (= [{:type :view/shell-run :tab "tab-1" :id "eval-0" :command "ls -a"}] (sent-messages run-effects)))
      (is (= :running (get-in running [:repl :sessions "tab-1" :status])))
      (is (= [:input "orchard $ ls -a"] (last (log-of running "tab-1")))))
    (testing "output joins the log, and a failing exit and a new folder end the command"
      (let [ended (:state (ui-state/handle-all running
                                               [[:host {:type :host/shell-output :tab "tab-1" :id "eval-0" :text "src\n"}]
                                                [:host {:type :host/shell-output :tab "tab-1" :id "eval-0" :text "test\n"}]
                                                [:host {:type :host/shell-output :tab "tab-1" :id "eval-0" :done? true
                                                        :exit-code 2 :cwd "/work"}]]))]
        (is (= [[:out "src\ntest\n"] [:info "Exited with code 2."]] (vec (take-last 2 (log-of ended "tab-1")))))
        (is (= [:ready "/work" {}] ((juxt :status :cwd :pending) (get-in ended [:repl :sessions "tab-1"]))))
        (is (= "work $ " (repl-state/shell-prompt (get-in ended [:repl :sessions "tab-1"]))))))
    (testing "a line typed while a command runs goes to it as input, its echo coming back as output"
      (let [{answered :state answer-effects :effects} (ui-state/handle running [:repl-submit "Lorelai"])
            {empty-line :state empty-effects :effects} (ui-state/handle running [:repl-submit ""])]
        (is (= [{:type :view/shell-input :tab "tab-1" :text "Lorelai"}] (sent-messages answer-effects)))
        (is (= (log-of running "tab-1") (log-of answered "tab-1")) "the terminal's echo shows it")
        (is (= ["ls -a" "Lorelai"] (get-in answered [:repl :sessions "tab-1" :history])))
        (is (= :running (get-in answered [:repl :sessions "tab-1" :status])))
        (is (= [{:type :view/shell-input :tab "tab-1" :text ""}] (sent-messages empty-effects))
            "an empty line still goes, as Enter answering a prompt")
        (is (= ["ls -a"] (get-in empty-line [:repl :sessions "tab-1" :history])))))
    (testing "a command typed in VS Code's own terminal panel shows as input, and the tab runs until it ends"
      (let [state (:state (ui-state/handle opened [:host {:type :host/shell-output :tab "tab-1" :id "panel-1"
                                                          :command "git status" :text "clean\n"}]))]
        (is (= [[:input "orchard $ git status"] [:out "clean\n"]] (vec (take-last 2 (log-of state "tab-1")))))
        (is (= :running (get-in state [:repl :sessions "tab-1" :status])))
        (is (= :ready (get-in (:state (ui-state/handle state [:host {:type :host/shell-output :tab "tab-1" :id "panel-1"
                                                                     :done? true :exit-code 0}]))
                              [:repl :sessions "tab-1" :status])))
        (is (= :running (get-in (:state (ui-state/handle-all running
                                                             [[:host {:type :host/shell-output :tab "tab-1" :id "panel-2"
                                                                      :command "make" :text ""}]
                                                              [:host {:type :host/shell-output :tab "tab-1" :id "eval-0"
                                                                      :done? true :exit-code 0}]]))
                                [:repl :sessions "tab-1" :status]))
            "the tab runs while any of its commands does")))
    (testing "Ctrl+C goes to the terminal, and a closed terminal takes no more commands"
      (is (= [{:type :view/shell-interrupt :tab "tab-1"}] (sent-messages (:effects (ui-state/handle running [:repl-interrupt])))))
      (let [closed (:state (ui-state/handle running [:host {:type :host/shell-status :tab "tab-1" :status :closed
                                                            :message "The terminal closed."}]))
            {refused :state refused-effects :effects} (ui-state/handle closed [:repl-submit "ls"])]
        (is (empty? (get-in closed [:repl :sessions "tab-1" :pending])))
        (is (empty? refused-effects))
        (is (= :err (first (last (log-of refused "tab-1")))))))))

(deftest the-pane-follows-the-page-last-clicked
  (let [with-repl (connected (:state (ui-state/handle (loaded) [:toggle-repl])) "tab-1")
        lifted (:state (ui-state/handle with-repl [:click-namespace "app.db"]))]
    (testing "lifting a Clojure page puts the REPL tab that evaluates in its namespace"
      (is (= {:namespace-id "app.db" :ns "app.db" :path "src/app/db.clj"} (get-in lifted [:repl :following])))
      (is (= ["app.db" "src/app/db.clj"] ((juxt :ns :ns-path) (get-in lifted [:repl :sessions "tab-1"]))))
      (is (= [{:type :view/repl-eval :tab "tab-1" :id "eval-0" :code "(add 1 2)" :ns "app.db" :path "src/app/db.clj"}]
             (sent-messages (:effects (ui-state/handle lifted [:repl-submit "(add 1 2)"]))))
          "code typed into the pane runs there, read from that file so its ns form can load first"))
    (testing "a namespace the pane's code moves into brings the file holding it, or none outside the cloud"
      (let [moved (fn [ns]
                    (get-in (:state (ui-state/handle-all lifted [[:repl-submit "(in-ns 'elsewhere)"]
                                                                 [:host {:type :host/repl-output :tab "tab-1" :id "eval-0"
                                                                         :value "#namespace[elsewhere]" :ns ns}]]))
                            [:repl :sessions "tab-1"]))]
        (is (= ["app.core" "src/app/core.clj"] ((juxt :ns :ns-path) (moved "app.core"))))
        (is (= ["scratch" nil] ((juxt :ns :ns-path) (moved "scratch"))))))
    (testing "a page in another language leaves the namespace as it was"
      (is (= "app.db" (get-in (:state (ui-state/handle lifted [:click-namespace "web/page.js"])) [:repl :sessions "tab-1" :ns]))))
    (testing "a REPL tab opened later starts in the namespace followed"
      (is (= "app.db" (get-in (:state (ui-state/handle lifted [:repl-new-tab :repl])) [:repl :sessions "tab-2" :ns]))))))

(deftest files-load-after-what-they-require
  (testing "requires load first, each once, and only files that run beside the namespace's own"
    (is (= ["src/app/util.cljc" "src/app/db.clj" "src/app/core.clj"] (repl-state/load-order snapshot "app.core")))
    (is (= ["src/app/util.cljc" "src/app/ui.cljs"] (repl-state/load-order snapshot "app.ui"))
        "a .cljs namespace skips .clj files and what only they require")
    (is (= ["src/app/util.cljc" "src/app/loop_b.clj" "src/app/loop_a.clj"] (repl-state/load-order snapshot "app.loop-a"))
        "a cycle loads in the order it was reached")
    (is (= [] (repl-state/load-order snapshot "web/page.js"))))
  (testing "the button loads the namespace the pane follows"
    (let [following (:state (ui-state/handle-all (loaded) [[:toggle-repl] [:click-namespace "app.core"]]))
          {:keys [state effects]} (ui-state/handle following [:repl-load-following])]
      (is (= [{:type :view/repl-load-files :tab "tab-1" :id "eval-0"
               :paths ["src/app/util.cljc" "src/app/db.clj" "src/app/core.clj"]}]
             (sent-messages effects)))
      (is (= [:input "app.core=> Loading app.core and its requires/dependencies"] (last (log-of state "tab-1"))))
      (is (seq (:notices (:state (ui-state/handle (loaded) [:repl-load-following]))))
          "without a page clicked, it says what to do"))))

(defn- editing-file
  "The state editing `text` as the file of `namespace-id` at `path`, the
  caret at `position`."
  [namespace-id path text position]
  (let [with-sources (:state (ui-state/handle-all (loaded)
                                                  [[:host {:type :host/sources :snapshot-id :working-tree
                                                           :entries [{:namespace-id namespace-id :source-key [namespace-id [nil]]
                                                                      :files [{:path path :text text}]}]}]]))]
    (:state (ui-state/handle with-sources [:begin-editing {:namespace-id namespace-id :path path :position position
                                                           :columns 1 :rows 4}]))))

(deftest evaluating-from-a-page
  (let [db-path "src/app/db.clj"
        text "(ns app.db)\n(defn add [a b]\n  (+ a b))\n(comment (add 1 2))\n"
        editing (editing-file "app.db" db-path text [2 9])
        result-of (fn [state] (select-keys (get-in state [:inline-results [db-path 2]]) [:status :text]))]
    (testing "without a REPL tab, the form at the caret opens one and goes to it, in the file's namespace"
      (let [{:keys [state effects]} (ui-state/handle editing [:evaluate :form])]
        (is (= [{:type :view/repl-eval :tab "tab-1" :id "eval-0" :code "(+ a b)" :ns "app.db" :path db-path :line 3 :column 3}]
               (sent-messages effects)))
        (is (= ["tab-1"] (get-in state [:repl :tabs])))
        (is (get-in state [:repl :open?]) "the pane opens with the tab, showing the evaluation")
        (is (= {:id "eval-0" :namespace-id "app.db" :path db-path :line 2 :column 10 :status :pending}
               (select-keys (get-in state [:inline-results [db-path 2]]) [:id :namespace-id :path :line :column :status])))
        (is (= [:input "app.db=> (+ a b)"] (last (log-of state "tab-1"))))
        (testing "its value shows at the end of its line, until the file is edited"
          (let [answered (:state (ui-state/handle-all state [[:host {:type :host/repl-output :tab "tab-1" :id "eval-0" :value "3" :ns "app.db"}]
                                                             [:host {:type :host/repl-output :tab "tab-1" :id "eval-0" :status ["done"]}]]))]
            (is (= {:status :value :text "3"} (result-of answered)))
            (is (empty? (:inline-results (:state (ui-state/handle answered [:edit :type " "])))))))
        (testing "an error shows its message, and a request that never reached a server says so"
          (is (= {:status :error :text "Divide by zero"}
                 (result-of (:state (ui-state/handle-all
                                     state
                                     [[:host {:type :host/repl-output :tab "tab-1" :id "eval-0"
                                              :err "Execution error (ArithmeticException) at app.db/add (db.clj:3).\nDivide by zero\n"}]
                                      [:host {:type :host/repl-output :tab "tab-1" :id "eval-0" :ex "class java.lang.ArithmeticException"
                                              :status ["eval-error"]}]
                                      [:host {:type :host/repl-output :tab "tab-1" :id "eval-0" :status ["done"]}]])))))
          (is (= {:status :error :text "Not connected"}
                 (result-of (:state (ui-state/handle state [:host {:type :host/repl-output :tab "tab-1" :id "eval-0"
                                                                   :status ["done" "not-connected"]}]))))))))
    (testing "the top-level form, echoed whole in the pane, and inside a rich comment the form there"
      (let [{:keys [state effects]} (ui-state/handle editing [:evaluate :top-level])]
        (is (= "(defn add [a b]\n  (+ a b))" (:code (first (sent-messages effects)))))
        (is (= [:input "app.db=> (defn add [a b]\n  (+ a b))"] (last (log-of state "tab-1")))))
      (is (= "(add 1 2)" (:code (first (sent-messages (:effects (ui-state/handle (editing-file "app.db" db-path text [3 15])
                                                                                  [:evaluate :top-level]))))))))
    (testing "the whole file loads, its result on the first line"
      (let [{:keys [state effects]} (ui-state/handle editing [:evaluate :file])]
        (is (= [{:type :view/repl-load-file :tab "tab-1" :id "eval-0" :code (subs text 0 (dec (count text))) :path db-path}]
               (sent-messages effects)))
        (is (= {:status :pending :loading? true} (select-keys (get-in state [:inline-results [db-path 0]]) [:status :loading?])))))
    (testing "the file loads after the files of what it requires"
      (let [{:keys [state effects]} (ui-state/handle (editing-file "app.core" "src/app/core.clj" "(ns app.core)\n(def x 1)" [1 3])
                                                     [:evaluate :file-with-requires])]
        (is (= [{:type :view/repl-load-files :tab "tab-1" :id "eval-0"
                 :paths ["src/app/util.cljc" "src/app/db.clj" "src/app/core.clj"]}]
               (sent-messages effects)))
        (is (= [:input "app.core=> Loading app.core and its requires/dependencies, 2 namespaces"] (last (log-of state "tab-1"))))))
    (testing "nothing goes without a closed Clojure form at the caret"
      (doseq [state [(editing-file "app.db" db-path "(ns app.db)\n\n(def x 1)" [1 0])
                     (editing-file "app.db" db-path "(ns app.db)\n(defn add [a b]\n" [1 15])
                     (assoc-in editing [:buffers db-path :language] :python)]]
        (let [{evaluated :state :keys [effects]} (ui-state/handle state [:evaluate :form])]
          (is (empty? effects))
          (is (seq (:notices evaluated))))))))

(deftest calva-s-scopes
  (let [lines ["(ns app.db)" "(defn plan [rooms]" "  (-> rooms (get :kitchen) (vec)))" "(comment (plan {:kitchen [1 2]}))"]
        evaluated (fn [scope caret & [anchor]]
                    (repl-state/evaluation {:lines lines :caret caret :anchor (or anchor caret) :language :clojure
                                            :path "src/app/db.clj"}
                                           scope))]
    (testing "the enclosing form is the collection around the caret, beside a form or not"
      (is (= {:code "(-> rooms (get :kitchen) (vec))" :start [2 2] :end [2 33] :namespace-name "app.db"}
             (evaluated :enclosing [2 26])))
      (is (= "(defn plan [rooms]\n  (-> rooms (get :kitchen) (vec)))" (:code (evaluated :enclosing [2 34])))
          "with nothing around the caret, the top-level form beside it"))
    (testing "up to the caret, the brackets left open close"
      (is (= {:code "(-> rooms (get :kitchen))" :start [2 2] :end [2 27] :namespace-name "app.db"}
             (evaluated :to-caret [2 27])))
      (is (= "(defn plan [rooms]\n  (-> rooms (get :kitchen)))" (:code (evaluated :top-level-to-caret [2 27]))))
      (is (= "(plan {:kitchen [1]})" (:code (evaluated :top-level-to-caret [3 27])))
          "inside a rich comment, from the start of the form within it")
      (is (= "(ns app.db)\n(defn plan [rooms]\n  (-> rooms))" (:code (evaluated :file-to-caret [2 12])))))
    (testing "a selection up to its end, its brackets closed"
      (is (= "(-> rooms (get :kitchen))" (:code (evaluated :to-caret [2 27] [2 2]))))
      (is (= "(get :kitchen) (vec)" (:code (evaluated :form [2 32] [2 12])))
          "evaluating the form sends a selection as it is"))
    (testing "nothing goes where nothing comes before the caret or nothing encloses it"
      (is (:notice (evaluated :file-to-caret [0 0])))
      (is (:notice (evaluated :to-caret [1 0]))))))

(deftest a-file-naming-no-namespace-runs-in-the-default-one
  (is (= "user" (repl-state/default-namespace "src/scratch.clj")))
  (is (= "user" (repl-state/default-namespace "src/scratch.cljc")))
  (is (= "cljs.user" (repl-state/default-namespace "src/scratch.cljs")))
  (let [path "src/app/db.clj"
        text "(defn plus-two [x]\n  (+ x 2))\n(plus-two 3)"
        editing (editing-file "app.db" path text [2 12])]
    (testing "a form goes to the default namespace"
      (is (= "user" (:ns (first (sent-messages (:effects (ui-state/handle editing [:evaluate :form]))))))))
    (testing "loading the file moves into it first, on the first line"
      (is (= (str "(ns user) " text) (:code (first (sent-messages (:effects (ui-state/handle editing [:evaluate :file])))))))))
  (is (= "(ns a)\n(def x 1)" (repl-state/file-code "src/a.clj" "(ns a)\n(def x 1)")))
  (is (= "(ns cljs.user) (def x 1)" (repl-state/file-code "src/a.cljs" "(def x 1)"))))

(deftest errors-say-what-went-wrong-in-one-line
  (is (= "Divide by zero" (repl-state/error-summary "Execution error (ArithmeticException) at app.db/add (db.clj:3).\nDivide by zero\n" nil)))
  (testing "an error printed as null is named by its class"
    (is (= "StackOverflowError" (repl-state/error-summary "Execution error (StackOverflowError) at user/f (REPL:1).\nnull\n" nil)))
    (is (= "NullPointerException" (repl-state/error-summary "null\n" "class java.lang.NullPointerException")))
    (is (= "NullPointerException" (repl-state/error-summary nil "class java.lang.NullPointerException"))))
  (is (nil? (repl-state/error-summary nil nil))))

(deftest the-pane-remembers-its-place
  (let [geometry {:left 40 :top 60 :width 520 :height 300}
        {:keys [state effects]} (ui-state/handle ui-state/initial-state [:save-repl-geometry geometry])]
    (is (= geometry (get-in state [:repl :geometry])))
    (is (= [{:type :view/save-preference :key :repl-geometry :value geometry}] (sent-messages effects)))
    (is (empty? (:effects (ui-state/handle ui-state/initial-state [:save-repl-geometry {:left "far"}])))))
  (is (= {:left 1 :top 2 :width 300 :height 200}
         (get-in (:state (ui-state/handle ui-state/initial-state
                                          [:host {:type :host/init :repl-geometry {:left 1 :top 2 :width 300 :height 200}}]))
                 [:repl :geometry]))
      "the place comes back with the host's first message"))

(deftest a-reloaded-view-gets-its-tabs-back
  (let [state (:state (ui-state/handle-all ui-state/initial-state
                                           [[:host {:type :host/repl-status :tab "tab-4" :status :connected :port 7888
                                                    :message "Clojure 1.12.0 on port 7888"}]
                                            [:host {:type :host/shell-status :tab "tab-6" :status :ready :label "zsh"}]]))]
    (is (= ["tab-4" "tab-6"] (get-in state [:repl :tabs])))
    (is (= [:repl :shell] (mapv #(get-in state [:repl :sessions % :kind]) ["tab-4" "tab-6"])))
    (is (= "tab-4" (get-in state [:repl :active])) "the first tab the host names shows")
    (is (= ["tab-7"] (map :tab (sent-messages (:effects (ui-state/handle state [:repl-new-tab :repl])))))
        "new tabs number after the ones the host named")))

(deftest evaluating-from-a-vs-code-editor
  (let [from-editor {:type :host/evaluate :id "editor-1" :scope :form :code "(add 1 2)" :path "src/app/db.clj"
                     :namespace-name "app.db" :line 4 :column 10}
        {:keys [state effects]} (ui-state/handle (loaded) [:host from-editor])]
    (testing "code from a file open in an editor goes to the REPL tab that evaluates under the host's id, echoed there"
      (is (= [{:type :view/repl-eval :tab "tab-1" :id "editor-1" :code "(add 1 2)" :ns "app.db" :path "src/app/db.clj"
               :line 4 :column 10}]
             (sent-messages effects)))
      (is (= [:input "app.db=> (add 1 2)"] (last (log-of state "tab-1"))))
      (is (get-in state [:repl :open?]))
      (is (empty? (:inline-results state)) "its result shows in the editor, not on a page")
      (is (= "app.db" (get-in state [:repl :following :ns]) (get-in state [:repl :sessions "tab-1" :ns]))
          "the pane follows the file's page, as a click on the page would"))
    (testing "its value shows in the pane"
      (is (= [:value "3"] (last (log-of (:state (ui-state/handle state [:host {:type :host/repl-output :tab "tab-1" :id "editor-1"
                                                                               :value "3" :ns "app.db"}]))
                                        "tab-1")))))
    (testing "loading the file with its requires/dependencies finds its page by its path"
      (is (= [{:type :view/repl-load-files :tab "tab-1" :id "editor-2"
               :paths ["src/app/util.cljc" "src/app/db.clj" "src/app/core.clj"]}]
             (sent-messages (:effects (ui-state/handle state [:host {:type :host/evaluate :id "editor-2" :scope :file-with-requires
                                                                     :code "(ns app.core)" :path "src/app/core.clj"
                                                                     :namespace-name "app.core" :line 1 :column 1}]))))))))

(deftest a-symbol-the-cloud-defines-gets-a-hint
  (let [following (connected (:state (ui-state/handle-all (loaded) [[:toggle-repl] [:click-namespace "app.db"]])) "tab-1")
        submitted (:state (ui-state/handle following [:repl-submit "(add 1 2)"]))
        replied (fn [state err]
                  (log-of (:state (ui-state/handle state [:host {:type :host/repl-output :tab "tab-1" :id "eval-0" :err err}]))
                          "tab-1"))]
    (is (= [:info (str "add is defined in src/app/db.clj, which this REPL has not loaded. "
                       "Ctrl+Alt+C Enter on its page loads it, as does the load button above.")]
           (last (replied submitted "Syntax error compiling at (REPL:1:1).\nUnable to resolve symbol: add in this context\n"))))
    (testing "in another namespace, the namespaces defining it"
      (let [elsewhere (:state (ui-state/handle-all (loaded) [[:toggle-repl] [:click-namespace "app.core"] [:repl-submit "(add 1 2)"]]))]
        (is (= [:info "add is defined in app.db."] (last (replied elsewhere "Could not resolve symbol: add\n"))))))
    (testing "a symbol the cloud does not define gets none"
      (is (= :err (first (last (replied submitted "Unable to resolve symbol: subtract in this context\n"))))))))

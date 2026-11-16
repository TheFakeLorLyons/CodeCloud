(ns codecloud.presentation.ui-state-test
  (:require [clojure.test :refer [deftest is testing]]
            [codecloud.presentation.ui-state :as ui-state]))

(defn- snapshot-with-id [snapshot-id]
  {:id snapshot-id
   :namespaces {"app.core" {:name "app.core" :importance 1.0 :files [{:path "src/app/core.clj"}]}
                "app.db" {:name "app.db" :importance 0.4 :files [{:path "src/app/db.clj"}]}}
   :edges []})

(def ^:private commits
  [{:sha "c3" :subject "newest"} {:sha "c2" :subject "middle"} {:sha "c1" :subject "oldest"}])

(defn- run-events [events]
  (ui-state/handle-all ui-state/initial-state events))

(defn- sent-types [effects]
  (keep (fn [[effect-kind message]] (when (= :send effect-kind) (:type message))) effects))

(deftest snapshots-show-when-requested
  (let [{:keys [state effects]} (run-events [[:ready]
                                             [:host {:type :host/snapshot :snapshot (snapshot-with-id :working-tree)}]])]
    (is (= [:view/ready] (sent-types effects)))
    (is (= :working-tree (get-in state [:snapshot :id])))
    (is (some #{[:snapshot-shown :working-tree]} effects)))
  (testing "a working-tree update while viewing history does not pull the view away"
    (let [{:keys [state]} (run-events [[:host {:type :host/history :commits commits :available? true}]
                                       [:host {:type :host/snapshot :snapshot (snapshot-with-id :working-tree)}]
                                       [:select-snapshot "c2"]
                                       [:host {:type :host/snapshot :snapshot (snapshot-with-id "c2")}]
                                       [:host {:type :host/snapshot :snapshot (assoc (snapshot-with-id :working-tree) :label "newer")}]])]
      (is (= "c2" (get-in state [:snapshot :id])))
      (is (= "newer" (get-in state [:working-snapshot :label]))))))

(deftest history-stepping-and-playback
  (let [loaded (:state (run-events [[:host {:type :host/history :commits commits :available? true}]
                                    [:host {:type :host/snapshot :snapshot (snapshot-with-id :working-tree)}]]))]
    (testing "stepping back from the working tree goes to the newest commit"
      (let [{:keys [state effects]} (ui-state/handle loaded [:step-history -1])]
        (is (= "c3" (:requested-snapshot-id state)))
        (is (= [:view/select-snapshot] (sent-types effects)))))
    (testing "stepping forward from the newest commit returns to the working tree at once"
      (let [at-newest (:state (ui-state/handle-all loaded [[:select-snapshot "c3"]
                                                           [:host {:type :host/snapshot :snapshot (snapshot-with-id "c3")}]]))
            {:keys [state effects]} (ui-state/handle at-newest [:step-history 1])]
        (is (= :working-tree (get-in state [:snapshot :id])))
        (is (empty? (sent-types effects)))))
    (testing "playback starts at the oldest commit and ends on the working tree"
      (let [{started :state} (ui-state/handle loaded [:toggle-playback])
            _ (is (= "c1" (:requested-snapshot-id started)))
            _ (is (true? (:playing? started)))
            waiting (ui-state/handle started [:playback-tick])
            _ (is (= "c1" (get-in waiting [:state :requested-snapshot-id])) "ticks wait for the requested snapshot")
            final-state (:state (ui-state/handle-all started
                                                     [[:host {:type :host/snapshot :snapshot (snapshot-with-id "c1")}]
                                                      [:playback-tick]
                                                      [:host {:type :host/snapshot :snapshot (snapshot-with-id "c2")}]
                                                      [:playback-tick]
                                                      [:host {:type :host/snapshot :snapshot (snapshot-with-id "c3")}]
                                                      [:playback-tick]]))]
        (is (= :working-tree (get-in final-state [:snapshot :id])))
        (is (false? (:playing? final-state)))))))

(deftest lifting-and-sources
  (let [loaded (:state (run-events [[:host {:type :host/snapshot :snapshot (snapshot-with-id :working-tree)}]]))
        {clicked :state click-effects :effects} (ui-state/handle loaded [:click-namespace "app.db"])
        db-key (ui-state/source-key (:snapshot loaded) "app.db")]
    (is (= "app.db" (:focused-id clicked)))
    (is (some #{[:lift "app.db"]} click-effects))
    (is (= [:view/request-sources] (sent-types click-effects)))
    (testing "a second request for the same source is not sent"
      (is (empty? (sent-types (:effects (ui-state/handle clicked [:need-sources ["app.db"]])))))
      (let [files [{:path "src/app/db.clj" :text "(ns app.db)"}]
            {with-source :state arrival-effects :effects}
            (ui-state/handle clicked [:host {:type :host/sources
                                             :entries [{:source-key db-key :namespace-id "app.db" :files files}]}])]
        (is (= files (ui-state/source-of with-source "app.db")))
        (is (some #{[:source-arrived db-key]} arrival-effects))
        (is (empty? (sent-types (:effects (ui-state/handle with-source [:need-sources ["app.db"]])))))))
    (testing "requests come in batches of what is still missing"
      (let [{:keys [effects]} (ui-state/handle loaded [:need-sources ["app.core" "app.db" "app.missing"]])
            [[_ message]] effects]
        (is (= ["app.core" "app.db"] (map :namespace-id (:namespaces message))))))
    (testing "clicking again sets the page down; escape does too"
      (is (nil? (:focused-id (:state (ui-state/handle clicked [:click-namespace "app.db"])))))
      (is (some #{[:set-down "app.db"]} (:effects (ui-state/handle clicked [:escape])))))
    (testing "a click that lands after its page left the cloud lifts nothing"
      (let [hidden (:state (ui-state/handle loaded [:hide-page "app.db"]))
            {:keys [state effects]} (ui-state/handle hidden [:click-namespace "app.db"])]
        (is (nil? (:focused-id state)))
        (is (empty? effects))))))

(deftest filters-and-views
  (let [loaded (:state (run-events [[:host {:type :host/snapshot :snapshot (snapshot-with-id :working-tree)}]
                                    [:host {:type :host/views :views {"core" {:include ["/src/app/core.clj"] :ignore []}}}]
                                    [:click-namespace "app.db"]]))
        {hidden :state hide-effects :effects} (ui-state/handle loaded [:hide-page "app.db"])]
    (is (= #{"app.core"} (ui-state/visible-ids hidden)))
    (is (nil? (:focused-id hidden)) "a lifted page that the filter hides is set down")
    (is (= [:view/save-preference] (sent-types hide-effects)))
    (is (= #{"app.core"} (ui-state/visible-ids (:state (ui-state/handle loaded [:toggle-view "core"])))))
    (is (= #{"app.core" "app.db"} (ui-state/visible-ids (:state (ui-state/handle hidden [:clear-filter])))))
    (testing "the current filter saves as a named view"
      (let [{:keys [effects]} (ui-state/handle hidden [:save-view "  without db "])
            [[_ message]] effects]
        (is (= {:type :view/save-view :name "without db" :selection {:include [] :ignore ["/src/app/db.clj"]}}
               message))))
    (is (empty? (:effects (ui-state/handle hidden [:save-view "   "]))))
    (testing "arriving from the editor at a hidden namespace shows it again"
      (let [{:keys [state effects]} (ui-state/handle hidden [:host {:type :host/arrive :path "src/app/db.clj" :line 2}])]
        (is (contains? (ui-state/visible-ids state) "app.db"))
        (is (some #(= :arrive (first %)) effects)))))
  (let [{:keys [state effects]} (run-events [[:toggle-display-mode]])]
    (is (= :names (:display-mode state)))
    (is (= [:view/save-preference] (sent-types effects))))
  (is (= :names (:display-mode (:state (run-events [[:host {:type :host/init :display-mode :names}]]))))))

(deftest arriving-from-the-editor
  (testing "an arrival before the snapshot waits for it"
    (let [{:keys [state effects]} (run-events [[:host {:type :host/arrive :path "src/app/db.clj" :line 12 :text-scale 0.6}]
                                               [:host {:type :host/snapshot :snapshot (snapshot-with-id :working-tree)}]])]
      (is (nil? (:arrival state)))
      (is (some #{[:arrive {:path "src/app/db.clj" :line 12 :text-scale 0.6 :namespace-id "app.db"}]} effects))))
  (testing "an arrival while viewing history returns to the working tree"
    (let [{:keys [state effects]} (run-events [[:host {:type :host/history :commits commits :available? true}]
                                               [:host {:type :host/snapshot :snapshot (snapshot-with-id :working-tree)}]
                                               [:select-snapshot "c1"]
                                               [:host {:type :host/snapshot :snapshot (snapshot-with-id "c1")}]
                                               [:host {:type :host/arrive :path "src/app/core.clj" :line 3}]])]
      (is (= :working-tree (get-in state [:snapshot :id])))
      (is (some #(= :arrive (first %)) effects)))))

(deftest searching
  (let [loaded (:state (run-events [[:host {:type :host/snapshot :snapshot (snapshot-with-id :working-tree)}]]))
        {:keys [state effects]} (ui-state/handle-all loaded [[:open-search] [:search-query "db"] [:search-submit]])]
    (is (= "app.db" (:focused-id state)))
    (is (some #{[:lift "app.db"]} effects))
    (is (false? (get-in state [:search :open?]))))
  (testing "the arrow keys choose among the results, most important first, and wrap around"
    (let [loaded (:state (run-events [[:host {:type :host/snapshot :snapshot (snapshot-with-id :working-tree)}]]))
          searching (:state (ui-state/handle-all loaded [[:open-search] [:search-query "app"]]))]
      (is (= ["app.core" "app.db"] (ui-state/search-results searching)))
      (is (= "app.db" (:focused-id (:state (ui-state/handle-all searching [[:search-move 1] [:search-submit]])))))
      (is (= "app.db" (:focused-id (:state (ui-state/handle-all searching [[:search-move -1] [:search-submit]])))))
      (is (= "app.core" (:focused-id (:state (ui-state/handle-all searching [[:search-move 1] [:search-move 1]
                                                                             [:search-submit]])))))
      (is (= 0 (get-in (:state (ui-state/handle-all searching [[:search-move 1] [:search-query "ap"]]))
                       [:search :selected]))
          "a new query starts at the top again")
      (is (= "app.db" (:focused-id (:state (ui-state/handle searching [:search-pick "app.db"]))))))))

(deftest editing-in-the-cloud
  (let [db-path "src/app/db.clj"
        loaded (:state (run-events [[:host {:type :host/snapshot :snapshot (snapshot-with-id :working-tree)}]
                                    [:host {:type :host/sources
                                            :entries [{:namespace-id "app.db" :source-key ["app.db" [nil]]
                                                       :files [{:path db-path :text "(ns app.db)\n(def x 1)\n"}]}]}]]))
        editing (:state (ui-state/handle loaded [:begin-editing {:namespace-id "app.db" :path db-path :position [1 8]
                                                                 :columns 1 :rows 2}]))]
    (testing "a click puts the caret into the file's text"
      (is (= {:namespace-id "app.db" :path db-path :columns 1 :rows 2} (:editing editing)))
      (is (= [1 8] (get-in editing [:buffers db-path :caret]))))
    (let [{:keys [state effects]} (ui-state/handle editing [:edit :type "0"])]
      (testing "typing sends each change to the host and marks the file unsaved"
        (is (= ["(ns app.db)" "(def x 10)"] (get-in state [:buffers db-path :lines])))
        (is (= [[:send {:type :view/edit :path db-path :range [1 8 1 8] :text "0" :replaced "" :revision 1 :epoch 0}]]
               effects))
        (is (contains? (:dirty-paths state) db-path)))
      (testing "moving the caret changes nothing in the file"
        (is (empty? (:effects (ui-state/handle state [:edit :move :left false])))))
      (testing "saving asks the host to save the file"
        (is (= [:view/save] (sent-types (:effects (ui-state/handle state [:save-file]))))))
      (testing "a buffer stays until the working tree catches up with its last edit"
        (let [acknowledged (:state (ui-state/handle-all state [[:host {:type :host/edited :path db-path :revision 1
                                                                      :content-key "k1" :dirty? true}]
                                                               [:end-editing]]))
              caught-up (assoc-in (snapshot-with-id :working-tree) [:namespaces "app.db" :files 0 :content-key] "k1")]
          (is (contains? (:buffers acknowledged) db-path))
          (is (empty? (:buffers (:state (ui-state/handle acknowledged [:host {:type :host/snapshot :snapshot caught-up}])))))))
      (testing "a change from elsewhere replaces the buffer's text, and later edits build on it"
        (let [rebased (:state (ui-state/handle state [:host {:type :host/document :path db-path :text "(ns app.db)\n"
                                                             :content-key "k2" :epoch 1 :dirty? true}]))]
          (is (= ["(ns app.db)"] (get-in rebased [:buffers db-path :lines])))
          (is (= 1 (:epoch (second (first (:effects (ui-state/handle rebased [:edit :type "x"])))))))))
      (testing "saving elsewhere clears the unsaved mark"
        (is (empty? (:dirty-paths (:state (ui-state/handle state [:host {:type :host/dirty :path db-path :dirty? false}]))))))
      (testing "clicking another page stops editing"
        (is (nil? (:editing (:state (ui-state/handle state [:click-namespace "app.core"]))))))
      (testing "hiding the page stops editing"
        (is (nil? (:editing (:state (ui-state/handle state [:hide-page "app.db"])))))))
    (testing "escape and a click on the sky stop editing"
      (is (nil? (:editing (:state (ui-state/handle editing [:escape])))))
      (is (nil? (:editing (:state (ui-state/handle editing [:click-empty]))))))
    (testing "an untouched buffer leaves with its session"
      (is (empty? (:buffers (:state (ui-state/handle editing [:end-editing])))))))
  (testing "history is read-only"
    (let [historical (:state (run-events [[:select-snapshot "c1"] [:host {:type :host/snapshot :snapshot (snapshot-with-id "c1")}]]))
          {:keys [state]} (ui-state/handle historical [:begin-editing {:namespace-id "app.db" :path "src/app/db.clj"
                                                                       :position [0 0]}])]
      (is (nil? (:editing state)))
      (is (seq (:notices state)))))
  (testing "a file in a language without an adapter is read-only"
    (let [snapshot {:id :working-tree
                    :namespaces {"native/A.java" {:name "native/A.java" :kind :module :language :java
                                                  :files [{:path "native/A.java" :language :java}]}}
                    :edges []}
          loaded (:state (run-events [[:host {:type :host/snapshot :snapshot snapshot}]]))
          {:keys [state]} (ui-state/handle loaded [:begin-editing {:namespace-id "native/A.java" :path "native/A.java"
                                                                   :position [0 0]}])]
      (is (nil? (:editing state)))
      (is (seq (:notices state)))
      (is (not (ui-state/editable? loaded (get-in snapshot [:namespaces "native/A.java"]) "native/A.java"))))))

(deftest lifting-folders
  (let [loaded (:state (run-events [[:host {:type :host/snapshot :snapshot (snapshot-with-id :working-tree)}]]))
        {lifted :state lift-effects :effects} (ui-state/handle loaded [:click-folder "src/app"])]
    (is (= "src/app" (:lifted-folder lifted)))
    (is (some #{[:lift-folder "src/app"]} lift-effects))
    (is (some #{[:set-down-folder "src/app"]} (:effects (ui-state/handle lifted [:click-folder "src/app"]))))
    (testing "a page lifts out of a lifted folder and sets down first"
      (let [with-page (:state (ui-state/handle lifted [:click-namespace "app.db"]))]
        (is (= ["app.db" "src/app"] ((juxt :focused-id :lifted-folder) with-page)))
        (let [once (:state (ui-state/handle with-page [:escape]))]
          (is (= [nil "src/app"] ((juxt :focused-id :lifted-folder) once)))
          (is (= [nil nil] ((juxt :focused-id :lifted-folder) (:state (ui-state/handle once [:escape]))))))))
    (testing "hiding a lifted folder sets it down"
      (let [{:keys [state effects]} (ui-state/handle lifted [:hide-folder "src/app"])]
        (is (nil? (:lifted-folder state)))
        (is (some #{[:set-down-folder "src/app"]} effects))))
    (testing "flying somewhere sets everything down"
      (is (some #{[:set-down-folder "src/app"]} (:effects (ui-state/handle lifted [:double-click-namespace "app.core"])))))))

(deftest arranging-coloring-and-spacing
  (let [loaded (:state (run-events [[:host {:type :host/snapshot :snapshot (snapshot-with-id :working-tree)}]]))]
    (testing "a shown snapshot colors its pages with the default layers"
      (is (= #{:folder :dependents} (:active-layers loaded)))
      (is (= #{"app.core" "app.db"} (set (keys (:page-colors loaded)))))
      (is (ui-state/colored-by-default? loaded)))
    (testing "changing the arrangement remembers it, puts moved pages back, and refits"
      (let [moved (:state (ui-state/handle loaded [:move-page "app.db" [1 2 3]]))
            {:keys [state effects]} (ui-state/handle moved [:set-arrangement :lines])]
        (is (= {"app.db" [1 2 3]} (:moved-pages moved)))
        (is (= :lines (:arrangement state)))
        (is (= {} (:moved-pages state)))
        (is (= [:view/save-preference] (sent-types effects)))
        (is (some #{[:refit]} effects))
        (is (empty? (:effects (ui-state/handle state [:set-arrangement :lines]))))))
    (testing "an unknown arrangement falls back to folders"
      (is (= :folders (:arrangement (:state (ui-state/handle (assoc loaded :arrangement :lines)
                                                             [:set-arrangement :sideways]))))))
    (testing "color layers turn on and off, and the choice is remembered"
      (let [{:keys [state effects]} (ui-state/handle loaded [:toggle-color-layer :folder])]
        (is (= #{:dependents} (:active-layers state)))
        (is (= [[:send {:type :view/save-preference :key :color-layers :value [:dependents]}]] effects))
        (is (not (ui-state/colored-by-default? state)))
        (is (= #{:dependents :length} (:active-layers (:state (ui-state/handle state [:toggle-color-layer :length])))))))
    (testing "folder clouds take their folders' colors unless the viewer turns that off"
      (is (true? (:tint-clouds? loaded)))
      (let [{:keys [state effects]} (ui-state/handle loaded [:toggle-cloud-tint])]
        (is (false? (:tint-clouds? state)))
        (is (= [[:send {:type :view/save-preference :key :tint-clouds? :value false}]] effects)))
      (is (false? (:tint-clouds? (:state (ui-state/handle loaded [:host {:type :host/init :tint-clouds? false}])))))
      (is (true? (:tint-clouds? (:state (ui-state/handle loaded [:host {:type :host/init}]))))))
    (testing "with every layer off, every page is gray"
      (let [state (:state (ui-state/handle loaded [:set-color-layers []]))]
        (is (= #{} (:active-layers state)))
        (is (every? nil? (vals (:page-colors state))))))
    (testing "layers the cloud does not offer are left out, and nil returns to the defaults"
      (let [chosen (:state (ui-state/handle loaded [:set-color-layers [:length :rainbow]]))]
        (is (= #{:length} (:active-layers chosen)))
        (is (= #{:folder :dependents} (:active-layers (:state (ui-state/handle chosen [:set-color-layers nil])))))))
    (testing "the cloud file's layers join the built-in ones"
      (let [state (:state (ui-state/handle loaded [:host {:type :host/views :views {}
                                                         :color-layers [{:key :cloud/tests :label "Tests"
                                                                         :patterns ["db.clj"] :colors ["pink"]}]}]))
            only-tests (:state (ui-state/handle state [:set-color-layers [:cloud/tests]]))]
        (is (= :cloud/tests (:key (peek (:layer-registry state)))))
        (is (some? (get-in only-tests [:page-colors "app.db"])))
        (is (nil? (get-in only-tests [:page-colors "app.core"])))))
    (testing "the totals open and close, and the choice is remembered"
      (let [{:keys [state effects]} (ui-state/handle loaded [:toggle-totals])]
        (is (true? (:totals-open? state)))
        (is (= [:view/save-preference] (sent-types effects)))
        (is (false? (:totals-open? (:state (ui-state/handle state [:toggle-totals])))))))
    (testing "spacing stays within its limits and steps by a ratio"
      (let [{:keys [least most]} ui-state/spread-limits]
        (is (= most (:spread (:state (ui-state/handle loaded [:set-spread 99])))))
        (is (= least (:spread (:state (ui-state/handle loaded [:preview-spread 0])))))
        (is (< 1.2 (:spread (:state (ui-state/handle loaded [:step-spread 1]))) 1.3))))
    (testing "pages go back one at a time or all together"
      (let [moved (:state (ui-state/handle-all loaded [[:move-page "app.db" [1 0 0]] [:move-page "app.core" [0 1 0]]]))]
        (is (= ["app.core"] (keys (:moved-pages (:state (ui-state/handle moved [:put-back "app.db"]))))))
        (is (= {} (:moved-pages (:state (ui-state/handle moved [:put-everything-back])))))))
    (testing "remembered preferences arrive with init"
      (let [initialized (:state (ui-state/handle loaded [:host {:type :host/init :arrangement :complection
                                                                :color-layers [:importance] :spread 1.6
                                                                :totals-open? true}]))]
        (is (= [:complection #{:importance} 1.6 true]
               ((juxt :arrangement :active-layers :spread :totals-open?) initialized)))))))

(def ^:private linked-snapshot
  {:id :working-tree
   :weights {:functions 1.0 :references 1.0 :lines 1.0}
   :namespaces {"app.core" {:id "app.core" :name "app.core" :importance 1.0 :files [{:path "src/app/core.clj"}]
                            :requires ["app.db"] :required-by []
                            :metrics {:functions 4 :references 0 :lines 90}}
                "app.db" {:id "app.db" :name "app.db" :importance 0.5 :files [{:path "src/app/db.clj"}]
                          :requires [] :required-by ["app.core"]
                          :metrics {:functions 1 :references 2 :lines 20}}
                "tools.build" {:id "tools.build" :name "tools.build" :importance 0.2 :files [{:path "tools/build.clj"}]
                               :requires [] :required-by [] :metrics {:functions 1 :references 0 :lines 5}}}
   :edges [{:from "app.core" :to "app.db" :kind :load :reference-count 1}]
   :cycles []})

(deftest header-pages-follow-the-header-mode
  (let [snapshot {:id :working-tree
                  :namespaces {"src/board.cpp" {:name "src/board.cpp" :importance 1.0
                                                :files [{:path "src/board.cpp" :file-role :source}]}
                               "src/board.hpp" {:name "src/board.hpp" :importance 0.5
                                                :files [{:path "src/board.hpp" :file-role :header
                                                         :companion "src/board.cpp"}]}}
                  :edges []}
        loaded (:state (run-events [[:host {:type :host/snapshot :snapshot snapshot}]]))
        saved-preferences (fn [effects]
                            (keep (fn [[_ message]]
                                    (when (= :view/save-preference (:type message)) (select-keys message [:key :value])))
                                  effects))]
    (is (= :beside (:header-mode loaded)))
    (is (= #{"src/board.cpp" "src/board.hpp"} (ui-state/visible-ids loaded)))
    (testing "hidden headers leave the cloud, and the choice is remembered"
      (let [{:keys [state effects]} (ui-state/handle loaded [:set-header-mode :hidden])]
        (is (= #{"src/board.cpp"} (ui-state/visible-ids state)))
        (is (some #{[:refit]} effects))
        (is (= [{:key :header-mode :value :hidden}] (saved-preferences effects)))
        (is (= #{"src/board.cpp" "src/board.hpp"}
               (ui-state/visible-ids (:state (ui-state/handle state [:set-header-mode :alone])))))))
    (testing "a remembered mode arrives with the host's first message"
      (is (= #{"src/board.cpp"}
             (ui-state/visible-ids (:state (run-events [[:host {:type :host/init :header-mode :hidden}]
                                                        [:host {:type :host/snapshot :snapshot snapshot}]]))))))
    (testing "choosing the mode in force changes nothing"
      (is (empty? (:effects (ui-state/handle loaded [:set-header-mode :beside])))))))

(deftest names-jump-to-their-definitions
  (let [snapshot {:id :working-tree
                  :namespaces {"main.cpp" {:name "main.cpp" :importance 1.0 :files [{:path "main.cpp"}] :definitions []}
                               "board.cpp" {:name "board.cpp" :importance 0.5 :files [{:path "board.cpp"}]
                                            :definitions [{:name "Board::read" :line 7} {:name "Board::read" :line 12}
                                                          {:name "Board::open" :line 20}]}}
                  :edges [{:from "main.cpp" :to "board.cpp" :kind :implicit :uses {"Board::read" 2 "Board::open" 1}}]}
        loaded (:state (run-events [[:host {:type :host/snapshot :snapshot snapshot}]]))
        jump (fn [written] (ui-state/handle loaded [:jump-to-name {:namespace-id "main.cpp" :name written :x 5 :y 6}]))]
    (testing "a name with one definition flies to its line"
      (is (some #{[:fly-to-line {:namespace-id "board.cpp" :path "board.cpp" :name "Board::open" :line 20}]}
                (:effects (jump "open")))))
    (testing "a name with several opens a menu of them at the pointer"
      (let [{:keys [state]} (jump "read")]
        (is (= {:kind :definitions :name "read" :x 5 :y 6} (select-keys (:menu state) [:kind :name :x :y])))
        (is (= [7 12] (mapv :line (get-in state [:menu :choices]))))))
    (testing "a name defined on no shown page says so"
      (is (= ["No page shown defines missing."] (:notices (:state (jump "missing"))))))))

(deftest hidden-pages-leave-weights-and-colors
  (let [loaded (:state (run-events [[:host {:type :host/snapshot :snapshot linked-snapshot}]]))
        {:keys [state effects]} (ui-state/handle loaded [:toggle-page "app.core"])]
    (is (= #{"app.db" "tools.build"} (ui-state/visible-ids state)))
    (is (= [:view/save-preference] (sent-types effects)))
    (is (not-any? #{[:refit]} effects) "a single page leaves the camera where it is")
    (testing "the shown pages are measured among themselves"
      (is (= 0 (get-in state [:visible-snapshot :namespaces "app.db" :metrics :references])))
      (is (= 1 (get-in state [:visible-snapshot :namespaces "app.db" :hidden-required-by])))
      (is (= 1.0 (double (get-in state [:visible-snapshot :namespaces "app.db" :importance])))))
    (testing "colors follow the shown pages"
      (is (= #{"app.db" "tools.build"} (set (keys (:page-colors state)))))
      (let [without-tools (:state (ui-state/handle loaded [:toggle-folder "tools"]))]
        (is (not= (get-in loaded [:layer-contexts :folder :colors ["src" "app"]])
                  (get-in without-tools [:layer-contexts :folder :colors ["src" "app"]]))
            "folder hues spread again over the folders still shown")))
    (testing "the page comes back"
      (is (= #{"app.core" "app.db" "tools.build"}
             (ui-state/visible-ids (:state (ui-state/handle state [:toggle-page "app.core"]))))))
    (testing "a folder and its pages turn off together"
      (is (= #{"tools.build"} (ui-state/visible-ids (:state (ui-state/handle loaded [:toggle-folder "src/app"]))))))))

(deftest colors-can-be-chosen
  (let [loaded (:state (run-events [[:host {:type :host/snapshot :snapshot linked-snapshot}]]))
        chooser {:target [:folder "src/app"] :label "src/app" :x 10 :y 20}
        teal {:day [0.64 0.12 185] :night [0.80 0.12 188]}
        opened (:state (ui-state/handle loaded [:open-color-chooser chooser]))
        {:keys [state effects]} (ui-state/handle opened [:choose-color [:folder "src/app"] teal])]
    (is (= chooser (:color-chooser opened)))
    (is (= teal (get-in state [:color-overrides :folder "src/app"])))
    (is (= [[:send {:type :view/save-preference :key :color-overrides :value {:folder {"src/app" teal}}}]] effects))
    (is (not= (get-in loaded [:page-colors "app.db"]) (get-in state [:page-colors "app.db"])))
    (testing "previews change colors without remembering them"
      (is (empty? (:effects (ui-state/handle opened [:preview-color [:folder "src/app"] teal])))))
    (testing "the default comes back"
      (let [reset (:state (ui-state/handle state [:reset-color [:folder "src/app"]]))]
        (is (= {} (:color-overrides reset)))
        (is (= (get-in loaded [:page-colors "app.db"]) (get-in reset [:page-colors "app.db"])))))
    (testing "escape and a click on the sky close the chooser first"
      (is (nil? (:color-chooser (:state (ui-state/handle opened [:escape])))))
      (is (nil? (:color-chooser (:state (ui-state/handle opened [:click-empty]))))))
    (testing "chosen colors arrive with init"
      (is (= teal (get-in (:state (ui-state/handle loaded [:host {:type :host/init :color-overrides {:folder {"src/app" teal}}}]))
                          [:color-overrides :folder "src/app"]))))))

(defn- sent-messages [effects]
  (keep (fn [[effect-kind message]] (when (= :send effect-kind) (dissoc message :protocol/version))) effects))

(deftest the-repl-pane
  (testing "opening the pane looks for the project's server, and closing it says nothing"
    (let [{opened :state :keys [effects]} (ui-state/handle ui-state/initial-state [:toggle-repl])]
      (is (get-in opened [:repl :open?]))
      (is (= :connecting (get-in opened [:repl :status])))
      (is (= [{:type :view/repl-connect}] (sent-messages effects)))
      (is (= [false []] ((juxt #(get-in % [:state :repl :open?]) :effects) (ui-state/handle opened [:toggle-repl]))))
      (is (empty? (:effects (ui-state/handle (assoc-in ui-state/initial-state [:repl :status] :failed) [:toggle-repl])))
          "after a failure, connecting again waits for the viewer")))
  (testing "a failed connection opens the pane, which says why"
    (let [state (:state (ui-state/handle ui-state/initial-state
                                         [:host {:type :host/repl-status :status :failed :message "No nREPL port file."}]))]
      (is (get-in state [:repl :open?]))
      (is (= [[:info "No nREPL port file."]] (mapv (juxt :kind :text) (get-in state [:repl :log]))))))
  (testing "a port typed while disconnected connects there"
    (is (= [{:type :view/repl-connect :port 7888}]
           (sent-messages (:effects (ui-state/handle ui-state/initial-state [:repl-submit " 7888 "])))))
    (is (= [{:type :view/repl-connect :host "localhost" :port 7888}]
           (sent-messages (:effects (ui-state/handle ui-state/initial-state [:repl-submit "localhost:7888"]))))))
  (testing "code typed while connected evaluates in the pane's namespace, which follows its replies"
    (let [connected (assoc-in ui-state/initial-state [:repl :status] :connected)
          {first-state :state first-effects :effects} (ui-state/handle connected [:repl-submit "(in-ns 'app.db)"])
          answered (:state (ui-state/handle-all first-state
                                                [[:host {:type :host/repl-output :id "eval-0" :value "#namespace[app.db]" :ns "app.db"}]
                                                 [:host {:type :host/repl-output :id "eval-0" :status ["done"]}]]))
          {second-state :state second-effects :effects} (ui-state/handle answered [:repl-submit "7888"])]
      (is (= [{:type :view/repl-eval :id "eval-0" :code "(in-ns 'app.db)"}] (sent-messages first-effects)))
      (is (= "user=> (in-ns 'app.db)" (:text (first (get-in first-state [:repl :log])))))
      (is (= "app.db" (get-in answered [:repl :ns])))
      (is (empty? (get-in answered [:repl :pending])))
      (is (= [{:type :view/repl-eval :id "eval-1" :code "7888" :ns "app.db"}] (sent-messages second-effects))
          "a number is code once connected")
      (is (= ["(in-ns 'app.db)" "7888"] (get-in second-state [:repl :history])))))
  (testing "output of one request joins one entry, and the log keeps its latest entries"
    (let [state (:state (ui-state/handle-all ui-state/initial-state
                                             [[:host {:type :host/repl-output :id "eval-0" :out "a"}]
                                              [:host {:type :host/repl-output :id "eval-0" :out "b\n"}]
                                              [:host {:type :host/repl-output :id "eval-0" :err "oops\n"}]
                                              [:host {:type :host/repl-output :id "eval-0" :out "c"}]]))
          flooded (reduce (fn [state index]
                            (:state (ui-state/handle state [:host {:type :host/repl-output :id (str "eval-" index) :value "1"}])))
                          ui-state/initial-state
                          (range 450))]
      (is (= [[:out "ab\n"] [:err "oops\n"] [:out "c"]] (mapv (juxt :kind :text) (get-in state [:repl :log]))))
      (is (= 400 (count (get-in flooded [:repl :log]))))
      (is (= 449 (:number (peek (get-in flooded [:repl :log])))))
      (is (empty? (get-in (:state (ui-state/handle state [:repl-clear])) [:repl :log])))))
  (testing "interrupting asks the host only while something runs"
    (is (empty? (:effects (ui-state/handle ui-state/initial-state [:repl-interrupt]))))
    (is (= [{:type :view/repl-interrupt}]
           (sent-messages (:effects (ui-state/handle (assoc-in ui-state/initial-state [:repl :pending "eval-0"] {:origin :pane})
                                                     [:repl-interrupt])))))))

(defn- editing-file
  "The state editing `text` as the file of app.db, the caret at `position`."
  [text position]
  (let [loaded (:state (run-events [[:host {:type :host/snapshot :snapshot (snapshot-with-id :working-tree)}]
                                    [:host {:type :host/sources :snapshot-id :working-tree
                                            :entries [{:namespace-id "app.db" :source-key ["app.db" [nil]]
                                                       :files [{:path "src/app/db.clj" :text text}]}]}]]))]
    (:state (ui-state/handle loaded [:begin-editing {:namespace-id "app.db" :path "src/app/db.clj" :position position
                                                     :columns 1 :rows 4}]))))

(deftest evaluating-from-a-page
  (let [db-path "src/app/db.clj"
        text "(ns app.db)\n(defn add [a b]\n  (+ a b))\n(comment (add 1 2))\n"
        editing (editing-file text [2 9])
        result-of (fn [state] (select-keys (get-in state [:inline-results [db-path 2]]) [:status :text]))]
    (testing "the form at the caret goes to the REPL in the file's namespace, from where it starts"
      (let [{:keys [state effects]} (ui-state/handle editing [:evaluate :form])]
        (is (= [{:type :view/repl-eval :id "eval-0" :code "(+ a b)" :ns "app.db" :path db-path :line 3 :column 3}]
               (sent-messages effects)))
        (is (= {:id "eval-0" :namespace-id "app.db" :path db-path :line 2 :column 10 :status :pending}
               (select-keys (get-in state [:inline-results [db-path 2]]) [:id :namespace-id :path :line :column :status])))
        (is (= "app.db=> (+ a b)" (:text (peek (get-in state [:repl :log])))))
        (testing "its value shows at the end of its line, until the file is edited"
          (let [answered (:state (ui-state/handle-all state [[:host {:type :host/repl-output :id "eval-0" :value "3" :ns "app.db"}]
                                                             [:host {:type :host/repl-output :id "eval-0" :status ["done"]}]]))]
            (is (= {:status :value :text "3"} (result-of answered)))
            (is (nil? (get-in answered [:repl :ns])) "the pane keeps its own namespace")
            (is (empty? (:inline-results (:state (ui-state/handle answered [:edit :type " "])))))
            (is (= {:status :value :text "3"}
                   (result-of (:state (ui-state/handle answered [:host {:type :host/document :path db-path :text (str text "\n")
                                                                        :content-key "k2" :epoch 1 :dirty? true}]))))
                "a change elsewhere leaves a result whose line still reads the same")))
        (testing "an error shows its message"
          (is (= {:status :error :text "Divide by zero"}
                 (result-of (:state (ui-state/handle-all
                                     state
                                     [[:host {:type :host/repl-output :id "eval-0"
                                              :err "Execution error (ArithmeticException) at app.db/add (db.clj:3).\nDivide by zero\n"}]
                                      [:host {:type :host/repl-output :id "eval-0" :ex "class java.lang.ArithmeticException"
                                              :status ["eval-error"]}]
                                      [:host {:type :host/repl-output :id "eval-0" :status ["done"]}]]))))))
        (testing "a request that never reached a server says so"
          (is (= {:status :error :text "Not connected"}
                 (result-of (:state (ui-state/handle state [:host {:type :host/repl-output :id "eval-0"
                                                                   :status ["done" "not-connected"]}]))))))))
    (testing "the top-level form, and inside a rich comment the form there"
      (is (= "(defn add [a b]\n  (+ a b))" (:code (first (sent-messages (:effects (ui-state/handle editing [:evaluate :top-level])))))))
      (is (= "(add 1 2)" (:code (first (sent-messages (:effects (ui-state/handle (editing-file text [3 15]) [:evaluate :top-level]))))))))
    (testing "a selection goes as it is"
      (let [selected (:state (ui-state/handle editing [:edit :select [1 6] [1 9]]))]
        (is (= "add" (:code (first (sent-messages (:effects (ui-state/handle selected [:evaluate :form])))))))))
    (testing "the whole file loads, its result on the first line"
      (let [{:keys [state effects]} (ui-state/handle editing [:evaluate :file])]
        (is (= [{:type :view/repl-load-file :id "eval-0" :code (subs text 0 (dec (count text))) :path db-path}]
               (sent-messages effects)))
        (is (= {:status :pending :loading? true}
               (select-keys (get-in state [:inline-results [db-path 0]]) [:status :loading?])))))
    (testing "nothing goes without a closed Clojure form at the caret"
      (doseq [state [(editing-file "(ns app.db)\n\n(def x 1)" [1 0])
                     (editing-file "(ns app.db)\n(defn add [a b]\n" [1 15])
                     (assoc-in editing [:buffers db-path :language] :python)]]
        (let [{evaluated :state :keys [effects]} (ui-state/handle state [:evaluate :form])]
          (is (empty? effects))
          (is (seq (:notices evaluated))))))))

(deftest clicks-on-the-page-being-edited
  (let [editing (editing-file "(ns app.db)\n(def x 1)" [1 3])
        lifted (:state (ui-state/handle editing [:click-namespace "app.db"]))]
    (testing "a click off the code of a page being edited lifts it, the caret staying"
      (is (= "app.db" (:focused-id lifted)))
      (is (= "src/app/db.clj" (get-in lifted [:editing :path]))))
    (testing "another such click leaves it lifted and the caret in place"
      (let [{:keys [state effects]} (ui-state/handle lifted [:click-namespace "app.db"])]
        (is (= "app.db" (:focused-id state)))
        (is (= [1 3] (get-in state [:buffers "src/app/db.clj" :caret])))
        (is (empty? effects))))
    (testing "a click on another page still stops editing"
      (is (nil? (:editing (:state (ui-state/handle lifted [:click-namespace "app.core"]))))))))

(deftest the-inspector-minimizes
  (let [{:keys [state effects]} (ui-state/handle ui-state/initial-state [:toggle-inspector])]
    (is (true? (:inspector-minimized? state)))
    (is (= [{:type :view/save-preference :key :inspector-minimized? :value true}] (sent-messages effects)))
    (is (false? (:inspector-minimized? (:state (ui-state/handle state [:toggle-inspector]))))))
  (testing "the choice comes back with the host's first message"
    (is (true? (:inspector-minimized? (:state (ui-state/handle ui-state/initial-state
                                                                 [:host {:type :host/init :inspector-minimized? true}])))))
    (is (false? (:inspector-minimized? (:state (ui-state/handle ui-state/initial-state [:host {:type :host/init}])))))))

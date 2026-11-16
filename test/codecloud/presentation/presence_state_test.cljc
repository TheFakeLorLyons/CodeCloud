(ns codecloud.presentation.presence-state-test
  "Checks how the cloud shows where agents work, finding pages for paths,
  noticing changes to open files, flying after the agent followed,
  listing the Coworkers pane's activity, fading, and counting the changed
  lines on each page."
  (:require [clojure.test :refer [deftest is]]
            [codecloud.presentation.presence-state :as presence-state]))

(def ^:private minute
  "A minute in milliseconds."
  60000)

(def ^:private snapshot
  {:id :working-tree
   :namespaces {"app.core" {:files [{:path "src/app/core.clj" :line-count 30}]}
                "app.db" {:files [{:path "src/app/db.clj" :line-count 12}]}}})

(def ^:private claude
  {:id "session-1" :name "Claude Code" :initials "CC" :hue 235 :present? true
   :place {:path "src/app/core.clj" :line 5} :doing "reading core.clj around line 5"
   :reads [{:path "src/app/core.clj" :start 1 :end 20 :at (* 2 minute)}]
   :changes []})

(def ^:private opencode
  {:id "session-2" :name "opencode" :initials "Op" :hue 150 :present? true
   :place {:path "src/app/db.clj" :line 4} :doing "changing db.clj"
   :reads []
   :changes [{:path "src/app/db.clj" :start 4 :end 5 :added 2 :removed 1 :at (* 3 minute) :credited? true}]})

(deftest paths-lead-to-pages
  (is (= {"src/app/core.clj" "app.core" "src/app/db.clj" "app.db"} (presence-state/page-index snapshot)))
  (is (= {:namespace-id "app.db" :path "src/app/db.clj" :line 4}
         (presence-state/place-location (presence-state/page-index snapshot) {:path "src/app/db.clj" :line 4})))
  (is (nil? (presence-state/place-location (presence-state/page-index snapshot) {:path "notes.txt" :line 1}))
      "a file no page holds is nowhere to fly"))

(deftest the-followed-agent-stays-while-present
  (let [following (presence-state/received (assoc presence-state/initial :following "session-2") {:agents [claude opencode] :pins []})]
    (is (= "session-2" (:following following)))
    (is (nil? (:following (presence-state/received following {:agents [claude] :pins []})))
        "an agent gone from the presence is followed no longer")))

(deftest changes-to-open-files-raise-notices
  (let [[first-notices first-latest] (presence-state/change-notices [opencode] #{"src/app/db.clj"} nil)]
    (is (= [] first-notices) "a view opening late raises nothing for changes made before")
    (is (= (* 3 minute) first-latest))
    (let [later (update opencode :changes conj {:path "src/app/db.clj" :start 9 :end 9 :added 1 :removed 1 :at (* 4 minute)})
          [notices latest] (presence-state/change-notices [later claude] #{"src/app/db.clj"} first-latest)]
      (is (= ["opencode changed src/app/db.clj, which is open here."] notices))
      (is (= (* 4 minute) latest)))
    (is (= [] (first (presence-state/change-notices [opencode] #{"src/app/core.clj"} 0)))
        "a change to a file not open here raises nothing")))

(deftest the-camera-keeps-up-with-the-agent-followed
  (let [page-of-path (presence-state/page-index snapshot)
        before (presence-state/received (assoc presence-state/initial :following "session-1") {:agents [claude opencode] :pins []})
        moved (presence-state/received before {:agents [(assoc claude :place {:path "src/app/db.clj" :line 9}) opencode] :pins []})]
    (is (= {:namespace-id "app.db" :path "src/app/db.clj" :line 9}
           (presence-state/followed-place before moved page-of-path #{"app.core" "app.db"})))
    (is (= {:namespace-id "app.db" :path "src/app/db.clj" :line 1 :whole? true}
           (presence-state/followed-place
            before
            (presence-state/received before {:agents [(assoc claude :place {:path "src/app/db.clj" :line 1 :whole? true}) opencode]
                                             :pins []})
            page-of-path #{"app.core" "app.db"}))
        "an agent reading a file whole is followed to the whole page")
    (is (nil? (presence-state/followed-place before before page-of-path #{"app.core" "app.db"})) "an agent that stays put needs no flight")
    (is (nil? (presence-state/followed-place before moved page-of-path #{"app.core"})) "nor one gone to a hidden page")
    (is (nil? (presence-state/followed-place (dissoc before :following) (dissoc moved :following) page-of-path #{"app.db"}))
        "nor any while no agent is followed")))

(deftest the-pane-lists-what-agents-did
  (let [pinned {:agents [claude opencode]
                :pins [{:id "pin-1" :agent-id "session-1" :path "src/app/core.clj" :line 9 :note "look here" :at (* 5 minute)}]}]
    (is (= [[:pin "Claude Code"] [:change "opencode"] [:read "Claude Code"]]
           (map (juxt :kind (comp :name :agent)) (presence-state/activity pinned 10)))
        "the newest first")
    (is (= 2 (count (presence-state/activity pinned 2))))))

(deftest marks-fade-and-changes-add-up
  (is (= 1 (presence-state/fade 1000 1000 presence-state/read-lifetime)))
  (is (= 0 (presence-state/fade 1000 (+ 1000 presence-state/read-lifetime) presence-state/read-lifetime)))
  (is (< 0.7 (presence-state/fade 0 (quot presence-state/read-lifetime 2) presence-state/read-lifetime) 0.8)
      "half its life gone, a mark keeps three quarters of its strength")
  (let [both {:agents [opencode (assoc claude :changes [{:path "src/app/db.clj" :start 1 :end 1 :added 1 :removed 0 :at (* 4 minute)}
                                                        {:path "src/app/core.clj" :at (* 4 minute)}])]}]
    (is (= {"app.db" {:added 3 :removed 1 :whole? false :agents ["opencode" "Claude Code"]}
            "app.core" {:added 0 :removed 0 :whole? true :agents ["Claude Code"]}}
           (presence-state/page-changes both (presence-state/page-index snapshot) (* 5 minute))))
    (is (empty? (presence-state/page-changes both (presence-state/page-index snapshot) (+ (* 4 minute) presence-state/change-lifetime)))
        "changes past their lifetime count no more")))

(ns codecloud.io.analyzer-test
  "Checks that reading a working tree again reuses what has not changed
  and sees what has, and that what the cloud leaves out is listed and,
  when chosen, read gray."
  (:require ["child_process" :as child-process]
            ["fs" :as fs]
            ["os" :as os]
            ["path" :as path]
            [cljs.test :refer [async deftest is]]
            [codecloud.io.analyzer :as analyzer]
            [codecloud.model.config :as config]))

(defn- write! [folder relative text]
  (let [file-path (path/join folder relative)]
    (.mkdirSync fs (path/dirname file-path) #js {:recursive true})
    (.writeFileSync fs file-path text)))

(defn- requires-of [snapshot namespace-id]
  (get-in snapshot [:namespaces namespace-id :requires]))

(deftest reading-again-reuses-what-has-not-changed
  (async done
    (let [folder (.mkdtempSync fs (path/join (.tmpdir os) "codecloud-analyzer-"))
          configuration (config/combine)
          reader (analyzer/create-analyzer {:root-directory folder})
          read! (fn [unsaved] (analyzer/analyze-working-tree reader configuration {:open-text unsaved}))
          no-unsaved (constantly nil)
          earlier (atom nil)
          minute-ago (/ (- (js/Date.now) 60000) 1000)]
      (write! folder "src/app/core.clj" "(ns app.core (:require [app.util :as util]))\n(defn start [] (util/helper))\n")
      (write! folder "src/app/util.clj" "(ns app.util)\n(defn helper [] 1)\n")
      (.utimesSync fs (path/join folder "src/app/core.clj") minute-ago minute-ago)
      (-> (read! no-unsaved)
          (.then (fn [{:keys [snapshot]}]
                   (reset! earlier snapshot)
                   (is (= ["app.util"] (requires-of snapshot "app.core")))
                   (is (= {"src/app/core.clj" true "src/app/util.clj" false}
                          (update-vals @(:stamps reader) :settled?))
                       "a file changed just now is read again next time, in case a coarse clock hides a change")
                   (read! no-unsaved)))
          (.then (fn [{:keys [snapshot]}]
                   (is (identical? @earlier snapshot) "with nothing changed, the same snapshot")
                   (write! folder "src/app/util.clj" "(ns app.util (:require [app.core]))\n(defn helper [] 2)\n")
                   (read! no-unsaved)))
          (.then (fn [{:keys [snapshot]}]
                   (is (not (identical? @earlier snapshot)))
                   (is (= ["app.core"] (requires-of snapshot "app.util")) "a change made at once is read")
                   (reset! earlier snapshot)
                   (read! {"src/app/core.clj" "(ns app.core)\n(defn start [] 0)\n"})))
          (.then (fn [{:keys [snapshot]}]
                   (is (= [] (requires-of snapshot "app.core")) "unsaved text counts over the file")
                   (read! no-unsaved)))
          (.then (fn [{:keys [snapshot]}]
                   (is (= ["app.util"] (requires-of snapshot "app.core")) "and the file counts again once it is gone")
                   (.unlinkSync fs (path/join folder "src/app/util.clj"))
                   (read! no-unsaved)))
          (.then (fn [{:keys [snapshot]}]
                   (is (= #{"app.core"} (set (keys (:namespaces snapshot)))))
                   (is (= ["src/app/core.clj"] (keys @(:stamps reader))) "a deleted file's stamp goes with it")
                   (is (= 1 (count @(:working-facts reader))) "the cache keeps what the last reading used")))
          (.catch (fn [error] (is (nil? error) (str "the reading failed: " (.-message error)))))
          (.finally (fn []
                      (.rmSync fs folder #js {:recursive true :force true})
                      (done)))))))

(deftest ignored-folders-and-files-are-listed-and-read-when-chosen
  (async done
    (let [folder (.mkdtempSync fs (path/join (.tmpdir os) "codecloud-ignored-"))
          reader (analyzer/create-analyzer {:root-directory folder})
          configuration (assoc (config/combine) :ignore ["out/"])
          read! (fn [shown] (analyzer/analyze-working-tree reader (assoc configuration :shown-ignored shown) {}))
          page-names (fn [snapshot] (set (map :name (vals (:namespaces snapshot)))))]
      (.execFileSync child-process "git" #js ["init" "-q"] #js {:cwd folder})
      (write! folder ".gitignore" "target/\n*.log\n")
      (write! folder "src/app/core.clj" "(ns app.core)\n(defn start [] 1)\n")
      (write! folder "target/gen/made.clj" "(ns app.made)\n(defn built [] 2)\n")
      (write! folder "out/main.clj" "(ns app.out)\n")
      (write! folder "notes.md" "# Notes\n")
      (write! folder "debug.log" "started\n")
      (write! folder "docs/cloud.png" "not really a picture")
      (-> (analyzer/ignored-entries reader configuration "")
          (.then (fn [{:keys [entries more]}]
                   (is (= [{:path "out/" :folder? true :reason :cloud}
                           {:path "target/" :folder? true :reason :git}
                           {:path ".gitignore" :folder? false :reason :type}
                           {:path "debug.log" :folder? false :reason :git}
                           {:path "notes.md" :folder? false :reason :type}]
                          entries))
                   (is (zero? more))
                   (analyzer/ignored-entries reader configuration "target/")))
          (.then (fn [{:keys [entries]}]
                   (is (= [{:path "target/gen/" :folder? true}] entries) "an ignored folder lists what it holds")
                   (analyzer/ignored-entries reader configuration "../")))
          (.then (fn [{:keys [entries]}]
                   (is (empty? entries) "nothing outside the cloud's folder is listed")
                   (read! [])))
          (.then (fn [{:keys [snapshot]}]
                   (is (= #{"app.core"} (page-names snapshot)))
                   (read! ["target/" "notes.md"])))
          (.then (fn [{:keys [snapshot]}]
                   (let [by-name (into {} (map (juxt :name identity)) (vals (:namespaces snapshot)))]
                     (is (= #{"app.core" "app.made" "notes.md"} (page-names snapshot)) "a chosen folder brings in what it holds")
                     (is (:ignored? (get by-name "app.made")) "marked so it shows gray")
                     (is (:ignored? (get by-name "notes.md")) "a file of a kind CodeCloud does not read comes in as text")
                     (is (not (:ignored? (get by-name "app.core"))) "the cloud's own pages stay as they were"))))
          (.catch (fn [error] (is (nil? error) (str "the reading failed: " (.-message error)))))
          (.finally (fn []
                      (.rmSync fs folder #js {:recursive true :force true})
                      (done)))))))

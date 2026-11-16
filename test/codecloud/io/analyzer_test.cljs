(ns codecloud.io.analyzer-test
  "Checks that reading a working tree again reuses what has not changed
  and sees what has."
  (:require ["fs" :as fs]
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

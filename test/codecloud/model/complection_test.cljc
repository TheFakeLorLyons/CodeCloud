(ns codecloud.model.complection-test
  (:require [clojure.string :as string]
            [clojure.test :refer [deftest is testing]]
            [codecloud.model.complection :as complection]
            [codecloud.model.parse :as parse]))

(defn- source-of [& lines]
  (string/join "\n" lines))

(def tangled-source
  (source-of
   "(ns my.app.jobs"
   "  (:require [clojure.core.async :as async :refer [<!]]))"
   ""
   "(defn plain [x] (inc x))"
   ""
   "(defn waits [channel]"
   "  (async/go"
   "    (let [value (<! channel)]"
   "      (async/>! channel value))))"
   ""
   "(defn classify [n]"
   "  (cond"
   "    (= n 1) :one (= n 2) :two (= n 3) :three (= n 4) :four"
   "    (= n 5) :five (= n 6) :six (= n 7) :seven (= n 8) :eight))"
   ""
   "(def counter (atom 0))"
   "(defn bump! []"
   "  ;; TODO stop counting twice"
   "  (swap! counter inc) (swap! counter inc) (reset! counter 0) (swap! counter inc)"
   "  (Thread/sleep 10))"
   ""
   "(defmulti handle :kind)"
   "(defmethod handle :click [event]"
   "  (async/go (<! (async/timeout 10)) event))"
   ""
   "(def settings {:a 1 :b 2})"
   ""
   "(defn shapes [] [{:outline [[0 0] [1 0] [1 [1 [1 [1]]]]] :fill {:colors [[1 2] [3 [4]]]}}])"
   ""))

(def flags
  {"todo" {:text ["TODO"] :symbols []}
   "sleeping" {:text [] :symbols ["Thread/sleep"]}
   "parking" {:text [] :symbols ["clojure.core.async/<!"]}})

(defn- hotspot-named [facts hotspot-name]
  (some #(when (= hotspot-name (:name %)) %) (:measured-forms facts)))

(deftest measures-tangled-definitions
  (let [facts (parse/analyze-file "src/my/app/jobs.clj" tangled-source {:flags flags})]
    (testing "plain code and data score nothing"
      (is (nil? (hotspot-named facts "plain")))
      (is (nil? (hotspot-named facts "settings")))
      (is (nil? (hotspot-named facts "counter"))))
    (testing "more than one asynchronous call counts, through aliases and refers"
      (is (= 3 (get-in (hotspot-named facts "waits") [:excesses :asynchronous]))))
    (testing "flagged symbols match as the file's aliases and refers qualify them"
      (is (= {"parking" 1} (get-in (hotspot-named facts "waits") [:excesses :flags])))
      (is (= {"parking" 1} (get-in (hotspot-named facts "handle :click") [:excesses :flags]))))
    (testing "every clause of a cond is a branch"
      (is (= 8 (get-in (hotspot-named facts "classify") [:excesses :branches]))))
    (testing "state changes and flags from text and symbols"
      (let [{:keys [excesses score]} (hotspot-named facts "bump!")]
        (is (= 4 (:state excesses)))
        (is (= {"todo" 1 "sleeping" 1} (:flags excesses)))
        (is (< 1 score))))
    (testing "nesting counts code forms, so deep data literals score nothing"
      (is (nil? (hotspot-named facts "shapes"))))
    (testing "methods are measured under their multimethod and dispatch value"
      (is (some? (hotspot-named facts "handle :click"))))
    (testing "the whole file's flags count, wherever they appear"
      (is (= {"todo" 1 "sleeping" 1 "parking" 2} (:flag-counts facts))))))

(deftest measures-templates-as-written
  (let [macro-source (source-of
                      "(ns my.app.macros)"
                      "(defmacro timed [label & body]"
                      "  `(let [started# (System/nanoTime)"
                      "         result# (do ~@body)]"
                      "     (println ~label (- (System/nanoTime) started#))"
                      "     result#))")
        facts (parse/analyze-file "src/my/app/macros.clj" macro-source {})]
    (is (nil? (hotspot-named facts "timed"))
        "a short template scores nothing, although its expansion nests deeply")))

(deftest scores-past-thresholds
  (let [plain {:lines 10 :branches 2 :nesting 4 :asynchronous 1 :state 1 :flags {}}
        thresholds complection/default-thresholds]
    (is (zero? (complection/score plain thresholds)))
    (is (== 1 (complection/score (assoc plain :lines 50) thresholds)))
    (is (== 2 (complection/score (assoc plain :asynchronous 3) thresholds)))
    (is (== 1 (complection/score (assoc plain :flags {"todo" 2}) thresholds)))
    (is (= {:lines 50} (complection/excesses (assoc plain :lines 50) thresholds)))))

(deftest configured-limits
  (let [thresholds (complection/thresholds-with {:lines 60 :branches 0 :unknown 3})]
    (testing "configured amounts replace the defaults and keep the step sizes"
      (is (= {:allowed 60 :per-point 25} (:lines thresholds)))
      (is (= {:allowed 0 :per-point 6} (:branches thresholds)))
      (is (= (:nesting complection/default-thresholds) (:nesting thresholds))))
    (testing "names that are not measurements are ignored"
      (is (not (contains? thresholds :unknown)))))
  (testing "a cloud file's limits reach the analysis"
    (let [lenient (parse/analyze-file "src/my/app/jobs.clj" tangled-source
                                      {:flags flags :complection {:branches 10}})]
      (is (nil? (hotspot-named lenient "classify"))))))

(deftest resolves-calls
  (let [context {:aliases {"a" "clojure.core.async"} :refers {"go" "clojure.core.async/go"}}]
    (is (= "clojure.core.async/<!" (complection/call-name context '(a/<! channel))))
    (is (= "clojure.core.async/go" (complection/call-name context '(go 1))))
    (is (= "swap!" (complection/call-name context '(clojure.core/swap! state inc))))
    (is (= "js/setTimeout" (complection/call-name context '(js/setTimeout f 10))))
    (is (nil? (complection/call-name context 'not-a-call)))))

(ns codecloud.model.config-test
  (:require [clojure.string :as string]
            [clojure.test :refer [deftest is testing]]
            [codecloud.model.config :as config]
            [codecloud.protocol :as protocol]))

(deftest validates-cloud-file-values
  (let [{validated :config problems :problems}
        (config/validate {:ignore ["dev/"] :weights {:lines 0.5 :mood 3 :functions -1}
                          :extensions [".CLJ" "cljs"]})]
    (is (= ["dev/"] (:ignore validated)))
    (is (= {:lines 0.5} (:weights validated)))
    (is (= ["clj" "cljs"] (:extensions validated)))
    (is (= 1 (count problems))))
  (testing "values of the wrong shape are dropped with a problem each"
    (let [{validated :config problems :problems} (config/validate {:ignore "dev/" :extensions "clj"})]
      (is (= {} validated))
      (is (= 2 (count problems)))))
  (is (seq (:problems (config/validate [1 2 3])))))

(deftest reads-cloud-files-and-deps-edn
  (is (= {:config {} :problems []} (config/cloud-file-config "  \n")))
  (is (= ["out/" "target/" ".shadow-cljs/" ".cpcache/"]
         (:ignore (:config (config/cloud-file-config config/cloud-file-template)))))
  (is (seq (:problems (config/cloud-file-config "{:ignore [\"a\""))))
  (is (= {:ignore ["resources/"]}
         (:config (config/deps-edn-config "{:paths [\"src\"] :codecloud/config {:ignore [\"resources/\"]}}"))))
  (is (= {} (:config (config/deps-edn-config "{:paths [\"src\"] :deps {}}"))))
  (testing "tagged literals in deps.edn do not break reading"
    (is (= {} (:config (config/deps-edn-config "{:aliases {:x {:jvm-opts #env FOO}}}"))))))

(deftest combines-with-precedence
  (let [combined (config/combine {:ignore ["a/"] :weights {:lines 2.0} :extensions ["clj"]}
                                 {:ignore ["b/" "a/"] :weights {:functions 0.5} :extensions ["cljs"]})]
    (is (= ["a/" "b/"] (:ignore combined)))
    (is (= {:functions 0.5 :references 1.0 :lines 2.0} (:weights combined)))
    (is (= ["cljs"] (:extensions combined))))
  (is (= (:extensions config/default-config) (:extensions (config/combine {:ignore ["a/"]})))))

(deftest validates-includes-and-views
  (let [{validated :config problems :problems}
        (config/validate {:include ["src/"]
                          :views {:ui ["src/ui/"]
                                  "server side" {:include ["src/server/"] :ignore ["**/legacy/"]}
                                  :broken {:include "src/"}}})]
    (is (= ["src/"] (:include validated)))
    (is (= {"ui" {:include ["src/ui/"] :ignore []}
            "server side" {:include ["src/server/"] :ignore ["**/legacy/"]}}
           (:views validated)))
    (is (= 1 (count problems))))
  (is (seq (:problems (config/validate {:views ["src/"]}))))
  (testing "views from deps.edn and the cloud file merge by name"
    (is (= #{"a" "b"} (set (keys (:views (config/combine {:views {"a" {:include ["x/"] :ignore []}}}
                                                          {:views {"b" {:include ["y/"] :ignore []}}}))))))))

(deftest validates-flags
  (let [{validated :config problems :problems}
        (config/validate {:flags {:todo ["TODO" "FIXME" ""]
                                  'blocking ['Thread/sleep 'deref "deref"]
                                  :broken "TODO"}})]
    (is (= {"todo" {:text ["TODO" "FIXME"] :symbols []}
            "blocking" {:text ["deref"] :symbols ["Thread/sleep" "deref"]}}
           (:flags validated)))
    (is (= 1 (count problems))))
  (is (= {"todo" {:text ["TODO" "FIXME" "HACK"] :symbols []}}
         (:flags (:config (config/cloud-file-config config/cloud-file-template)))))
  (testing "flags from deps.edn and the cloud file merge by name"
    (is (= #{"a" "b"} (set (keys (:flags (config/combine {:flags {"a" {:text ["x"] :symbols []}}}
                                                          {:flags {"b" {:text [] :symbols ["y"]}}}))))))))

(deftest validates-complection-limits
  (let [{validated :config problems :problems}
        (config/validate {:complection {:lines 40 :branches 0 :nesting -1 :mood 3}})]
    (is (= {:lines 40 :branches 0} (:complection validated)))
    (is (= [":complection accepts non-negative numbers for :lines, :branches, :nesting, :asynchronous, and :state."]
           problems)))
  (is (seq (:problems (config/validate {:complection [40]}))))
  (testing "limits from deps.edn and the cloud file merge by measurement"
    (is (= {:lines 40 :state 4}
           (:complection (config/combine {:complection {:lines 30 :state 4}}
                                         {:complection {:lines 40}}))))))

(deftest validates-color-layers
  (let [{validated :config problems :problems}
        (config/validate {:color-layers {:long {:metric :lines :colors ["green" "Red"]}
                                         "tests" {:label "Test code" :patterns ["test/"] :color "#ff66aa"}
                                         :moody {:metric :mood :colors ["green" "red"]}
                                         :pale {:patterns ["src/"] :color "chartreuse"}}})]
    (is (= {:cloud/long {:key :cloud/long :label "long" :metric :lines :colors ["green" "Red"]}
            :cloud/tests {:key :cloud/tests :label "Test code" :patterns ["test/"] :colors ["#ff66aa"]}}
           (:color-layers validated)))
    (is (= 2 (count problems)))
    (is (= [:cloud/long :cloud/tests] (map :key (config/color-layer-specs validated)))))
  (is (seq (:problems (config/validate {:color-layers [:long]}))))
  (testing "layers from deps.edn and the cloud file merge by name"
    (is (= #{:cloud/a :cloud/b}
           (set (keys (:color-layers (config/combine {:color-layers {:cloud/a {:key :cloud/a}}}
                                                     {:color-layers {:cloud/b {:key :cloud/b}}}))))))))

(defn- with-insertion
  "Text after inserting `insertion`, as config/view-insertion returns it,
  into `text`, as an editor applies it."
  [text {:keys [line column] :as insertion}]
  (let [lines (string/split text #"\n" -1)
        offset (+ (reduce + (map #(inc (count %)) (take (dec line) lines))) (dec column))]
    (str (subs text 0 offset) (:text insertion) (subs text offset))))

; My linter flags this function and I am not sure why; but it passes the test.
(deftest saves-views-into-cloud-file-text
  (let [view {:include ["/src/ui/"] :ignore ["/src/ui/legacy/"]}
        into-template (with-insertion config/cloud-file-template
                        (config/view-insertion config/cloud-file-template "ui" view))
        second-view (with-insertion into-template
                      (config/view-insertion into-template "server side" {:include ["/src/server/"]}))
        without-views "{:ignore [\"out/\"]}\n"
        added-map (with-insertion without-views (config/view-insertion without-views "ui" view))]
    (is (= {"ui" {:include ["/src/ui/"] :ignore ["/src/ui/legacy/"]}}
           (:views (:config (config/cloud-file-config into-template)))))
    (is (= #{"ui" "server side"} (set (keys (:views (:config (config/cloud-file-config second-view)))))))
    (is (string/includes? second-view "\n         \"server side\""))
    (is (= {"ui" {:include ["/src/ui/"] :ignore ["/src/ui/legacy/"]}}
           (:views (:config (config/cloud-file-config added-map)))))
    (is (= ["out/"] (:ignore (:config (config/cloud-file-config added-map)))))
    (is (:error (config/view-insertion "[1 2 3]" "ui" view)))
    (is (:error (config/view-insertion "{:views [1]}" "ui" view)))))

(deftest recognizes-source-paths
  (is (config/source-path? config/default-config "src/a/b.cljc"))
  (is (config/source-path? config/default-config "bb/task.bb"))
  (is (not (config/source-path? config/default-config "deps.edn")))
  (is (not (config/source-path? config/default-config "README.md"))))

(deftest protocol-round-trip
  (let [original (protocol/message :host/snapshot {:snapshot {:namespaces {"a" {:kind :namespace :requires #{"b"}}}}})
        decoded (protocol/decode (protocol/encode original))]
    (is (= original (dissoc decoded :protocol/version)))
    (is (number? (:protocol/version decoded))))
  (is (nil? (protocol/decode "not edn {")))
  (is (nil? (protocol/decode "[1 2 3]"))))

(deftest reads-every-known-language-unless-narrowed
  (testing "a cloud without :extensions or :languages reads every known language"
    (is (config/source-path? config/default-config "web/app.ts"))
    (is (config/source-path? config/default-config "native/filter.c"))
    (is (config/source-path? config/default-config "bin/deploy"))
    (is (not (config/source-path? config/default-config "notes.txt"))))
  (testing ":languages narrows the cloud and names languages by id, label, or tag"
    (let [{narrowed :config problems :problems} (config/validate {:languages [:clojure "Python" 'c++ :cobol]})]
      (is (= [:clojure :python :cpp] (:languages narrowed)))
      (is (= 1 (count problems)))
      (is (string/includes? (first problems) ":cobol"))
      (is (config/source-path? narrowed "src/app.py"))
      (is (not (config/source-path? narrowed "web/app.js")))
      (is (config/language-wanted? narrowed :cpp))
      (is (not (config/language-wanted? narrowed :shell)))))
  (testing ":extensions lists exactly the types to read, and types no language claims read as plain text"
    (let [listed {:extensions ["clj" "txt"]}]
      (is (config/source-path? listed "notes.txt"))
      (is (not (config/source-path? listed "web/app.js")))
      (is (= ["clj" "txt"] (config/read-extensions listed)))))
  (is (some #{"py"} (config/read-extensions config/default-config)))
  (is (= ["py" "pyi" "pyw"] (config/read-extensions {:languages [:python]}))))

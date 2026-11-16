(ns codecloud.model.ignored-test
  (:require [clojure.string :as string]
            [clojure.test :refer [deftest is testing]]
            [codecloud.model.ignored :as ignored]))

(deftest only-paths-inside-the-cloud-are-chosen
  (is (= ["target/" ".gitignore" "docs/notes.md"]
         (ignored/chosen-paths ["target/" ".gitignore" "docs/notes.md"])))
  (is (= [] (ignored/chosen-paths ["../outside/" "/etc/passwd" "a/../b" "a//b" "./a" "C:\\Windows" "" 7]))
      "nothing leaves the cloud's folder, and the folder itself is no choice")
  (is (= [] (ignored/chosen-paths "target/")) "a value that is no list chooses nothing")
  (is (ignored/valid-path? "") "the cloud's own folder can still be listed"))

(deftest a-chosen-folder-brings-in-everything-inside-it
  (let [shown? (ignored/shown-predicate ["target/" ".gitignore"])]
    (is (shown? "target/generated.clj"))
    (is (shown? "target/classes/app/core.clj"))
    (is (shown? ".gitignore"))
    (is (not (shown? "targets/other.clj")) "a folder's name alone is no prefix")
    (is (not (shown? "src/app/core.clj"))))
  (testing "how a path is shown"
    (is (= :itself (ignored/shown-by ["target/"] "target/")))
    (is (= :folder (ignored/shown-by ["target/"] "target/classes/")))
    (is (nil? (ignored/shown-by ["target/"] "notes.md")))
    (is (= :folder (ignored/shown-by ["target/generated.clj" "target/"] "target/generated.clj"))
        "a chosen folder settles what it holds, even a file chosen itself")))

(deftest choosing-again-takes-a-path-back-out
  (is (= ["target/"] (ignored/toggled [] "target/")))
  (is (= ["target/" ".gitignore"] (ignored/toggled ["target/"] ".gitignore")))
  (is (= [".gitignore"] (ignored/toggled ["target/" ".gitignore"] "target/"))))

(deftest files-holding-no-text-are-passed-over
  (is (ignored/readable? "notes.md"))
  (is (ignored/readable? ".gitignore"))
  (is (ignored/readable? "LICENSE"))
  (is (not (ignored/readable? "docs/cloud.PNG")))
  (is (not (ignored/readable? "target/app.jar"))))

(deftest the-list-names-what-the-cloud-leaves-out
  (let [ignored-by-cloud #{"out" "node_modules"}
        folder-ignored? (fn [folder] (contains? ignored-by-cloud folder))
        inside-ignored-folder? (fn [path]
                                 (let [segments (string/split path #"/")]
                                   (some #(folder-ignored? (string/join "/" (take % segments))) (range 1 (count segments)))))
        listed (ignored/entries {:git-ignored ["target/" ".cpcache/" "debug.log" "target/classes/" "shot.png"]
                                 :files ["src/app/core.clj" "out/main.js" "out/cljs/core.js" "node_modules/pkg/index.js"
                                         ".gitignore" "README.md" "docs/notes.md" "docs/cloud.png" "deps.edn"]
                                 :cloud-ignored? inside-ignored-folder?
                                 :cloud-folder-ignored? folder-ignored?
                                 :read? #{"src/app/core.clj" "deps.edn"}})]
    (is (= [{:path ".cpcache/" :folder? true :reason :git}
            {:path "node_modules/" :folder? true :reason :cloud}
            {:path "out/" :folder? true :reason :cloud}
            {:path "target/" :folder? true :reason :git}
            {:path ".gitignore" :folder? false :reason :type}
            {:path "README.md" :folder? false :reason :type}
            {:path "debug.log" :folder? false :reason :git}
            {:path "docs/notes.md" :folder? false :reason :type}]
           listed)
        "folders first, a folder's contents with it, and pictures passed over")))

(deftest the-repository-folder-never-shows
  (is (= [{:path "build/" :folder? true :reason :git}]
         (ignored/entries {:git-ignored [".git/" "build/"] :files [".git/config"]
                           :cloud-ignored? (constantly false) :cloud-folder-ignored? (constantly false)
                           :read? (constantly false)}))))

(deftest a-folder-lists-what-it-holds
  (is (= [{:path "target/classes/" :folder? true}
          {:path "target/generated.clj" :folder? false}]
         (ignored/child-entries "target/" [{:name "generated.clj" :folder? false}
                                           {:name "classes" :folder? true}
                                           {:name "app.jar" :folder? false}
                                           {:name ".git" :folder? true}])))
  (is (= {:entries [1 2] :more 3} (ignored/limited [1 2 3 4 5] 2))))

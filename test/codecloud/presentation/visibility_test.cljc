(ns codecloud.presentation.visibility-test
  (:require [clojure.test :refer [deftest is testing]]
            [codecloud.model.selection :as selection]
            [codecloud.presentation.visibility :as visibility]))

(def ^:private snapshot
  {:namespaces {"app.ui.views" {:files [{:path "src/app/ui/views.cljs"}] :requires ["app.db"] :required-by []}
                "app.ui.legacy" {:files [{:path "src/app/ui/legacy/old.cljs"}] :requires [] :required-by []}
                "app.server" {:files [{:path "src/app/server.clj"}] :requires ["app.db"] :required-by []}
                "app.db" {:files [{:path "src/app/db.clj"} {:path "src/app/db.cljs"}]
                          :requires [] :required-by ["app.ui.views" "app.server"]}
                "app.db-test" {:files [{:path "test/app/db_test.clj"}] :requires ["app.db"] :required-by []}}})

(def ^:private named-views
  {"ui" {:include ["src/app/ui/"] :ignore ["**/legacy/"]}
   "server" {:include ["src/app/server.clj" "src/app/db.*"] :ignore []}})

(deftest selections-follow-gitignore-rules
  (let [matcher (selection/compile-selection {:include ["src/"] :ignore ["**/legacy/"]})]
    (is (selection/selected? matcher "src/app/core.clj"))
    (is (not (selection/selected? matcher "src/app/legacy/old.clj")))
    (is (not (selection/selected? matcher "test/app/core_test.clj"))))
  (is (selection/selected? (selection/compile-selection {}) "anything/at/all.clj"))
  (is (= "/src/app/" (selection/directory-pattern "src/app")))
  (is (= "/**" (selection/directory-pattern "")))
  (is (= "/src/app/db.clj" (selection/file-pattern "src/app/db.clj"))))

(deftest filters-pick-what-shows
  (is (= 5 (count (visibility/visible-ids snapshot named-views visibility/no-filter))))
  (testing "named views show their own files and leave the rest out"
    (is (= #{"app.ui.views"} (visibility/visible-ids snapshot named-views {:active-views #{"ui"}})))
    (is (= #{"app.ui.views" "app.server" "app.db"}
           (visibility/visible-ids snapshot named-views {:active-views #{"ui" "server"}}))))
  (testing "single folders and hidden paths"
    (is (= #{"app.db-test"} (visibility/visible-ids snapshot named-views (visibility/show-only ["/test/"]))))
    (is (= #{"app.ui.views" "app.ui.legacy" "app.server" "app.db"}
           (visibility/visible-ids snapshot named-views (visibility/hide visibility/no-filter ["/test/"])))))
  (testing "a namespace with several files shows while any of them does"
    (is (contains? (visibility/visible-ids snapshot named-views (visibility/hide visibility/no-filter ["/src/app/db.clj"]))
                   "app.db")))
  (is (= #{"app.ui.views" "app.server" "app.db"}
         (visibility/visible-ids snapshot named-views
                                 (visibility/show-only (visibility/neighborhood-patterns snapshot "app.db")))))
  (is (= #{"app.db-test"} (visibility/visible-ids snapshot {"gone" {:include ["src/"]}}
                                                  {:active-views #{"missing"} :only ["/test/"]}))))

(deftest filters-read-as-sentences
  (is (nil? (visibility/describe visibility/no-filter)))
  (is (= "Showing the ui view." (visibility/describe {:active-views #{"ui"}})))
  (is (= "Showing the server and ui views and src/x. Hiding test."
         (visibility/describe {:active-views #{"ui" "server"} :only ["/src/x/"] :hidden ["/test/"]})))
  (is (= "Showing a, b, c, and 2 more."
         (visibility/describe (visibility/show-only ["/a/" "/b/" "/c/" "/d/" "/e/"]))))
  (is (= #{"ui"} (:active-views (visibility/toggle-view visibility/no-filter "ui"))))
  (is (empty? (:active-views (visibility/toggle-view {:active-views #{"ui"}} "ui"))))
  (is (= {:include ["src/app/ui/" "/x/"] :ignore ["**/legacy/" "/y/"]}
         (visibility/as-selection named-views {:active-views #{"ui"} :only ["/x/"] :hidden ["/y/"]} snapshot)))
  (is (= "Hiding Python and test." (visibility/describe {:hidden ["/test/"] :hidden-languages #{:python}}))))

(def ^:private mixed-snapshot
  (assoc-in snapshot [:namespaces "tools/build.py"]
            {:language :python :files [{:path "tools/build.py"}] :requires [] :required-by []}))

(defn- shown-after [filter-state]
  (visibility/visible-ids mixed-snapshot named-views filter-state))

(deftest pages-folders-and-languages-turn-on-and-off
  (let [everything (shown-after visibility/no-filter)
        toggled (fn [filter-state event & arguments]
                  (apply event filter-state mixed-snapshot named-views (shown-after filter-state) arguments))]
    (testing "a page hides and shows again"
      (let [hidden (toggled visibility/no-filter visibility/toggle-pages ["app.server"])]
        (is (= (disj everything "app.server") (shown-after hidden)))
        (is (= everything (shown-after (toggled hidden visibility/toggle-pages ["app.server"]))))))
    (testing "a folder hides whole, and a page inside it shows alone"
      (let [hidden (toggled visibility/no-filter visibility/toggle-folder "src/app")
            one-back (toggled hidden visibility/toggle-pages ["app.ui.views"])]
        (is (= #{"app.db-test" "tools/build.py"} (shown-after hidden)))
        (is (= #{"app.db-test" "tools/build.py" "app.ui.views"} (shown-after one-back)))
        (is (not-any? #{"/src/app/"} (:hidden one-back)) "the folder pattern split around the page")
        (is (= everything (shown-after (toggled one-back visibility/toggle-folder "src/app"))))))
    (testing "a folder partly shown shows whole, and hiding it covers the patterns inside"
      (let [inner (toggled visibility/no-filter visibility/toggle-pages ["app.server"])]
        (is (= everything (shown-after (toggled inner visibility/toggle-folder "src/app"))))
        (is (= ["/src/app/"] (:hidden (visibility/hide-folder inner "src/app"))))))
    (testing "a language hides and shows, and one of its pages can show alone"
      (let [hidden (toggled visibility/no-filter visibility/toggle-language :python)]
        (is (= (disj everything "tools/build.py") (shown-after hidden)))
        (is (= everything (shown-after (toggled hidden visibility/toggle-pages ["tools/build.py"]))))
        (is (= #{:python} (:hidden-languages hidden)))))
    (testing "a page outside the active views joins them"
      (let [ui-only (visibility/toggle-view visibility/no-filter "ui")
            with-test (toggled ui-only visibility/toggle-pages ["app.db-test"])]
        (is (= #{"app.ui.views" "app.db-test"} (shown-after with-test)))))
    (testing "saving a view writes hidden languages as their files"
      (is (= {:include [] :ignore ["/tools/build.py"]}
             (visibility/as-selection named-views (visibility/hide-language visibility/no-filter :python)
                                      mixed-snapshot))))))

(deftest folders-form-a-tree
  (let [{:keys [path page-ids all-ids folders]} (visibility/folder-tree mixed-snapshot)
        [src test-folder tools] folders]
    (is (= "" path))
    (is (empty? page-ids))
    (is (= (set (keys (:namespaces mixed-snapshot))) all-ids))
    (is (= ["src" "test" "tools"] (mapv :path folders)))
    (is (= ["src/app"] (mapv :path (:folders src))))
    (is (= #{"app.db-test"} (:all-ids test-folder)))
    (is (= ["tools/build.py"] (:page-ids tools)))
    (is (= :some (visibility/shown-share #{"app.db"} (:all-ids src))))
    (is (= :none (visibility/shown-share #{} (:all-ids src))))
    (is (= :all (visibility/shown-share (:all-ids src) (:all-ids src))))))

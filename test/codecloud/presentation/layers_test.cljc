(ns codecloud.presentation.layers-test
  (:require [clojure.test :refer [deftest is testing]]
            [codecloud.model.config :as config]
            [codecloud.presentation.color :as color]
            [codecloud.presentation.layers :as layers]
            [codecloud.presentation.palette :as palette]))

(defn- page
  "A Clojure namespace page in the file at `path`, with `fields` added."
  [namespace-name path & {:as fields}]
  (merge {:id namespace-name :name namespace-name :kind :namespace :language :clojure
          :files [{:path path}] :requires [] :required-by [] :importance 0.5 :metrics {:lines 10}}
         fields))

(defn- snapshot-of [& pages]
  {:namespaces (into {} (map (juxt :id identity)) pages) :edges []})

(def ^:private app-snapshot
  (snapshot-of (page "app.model.user" "src/app/model/user.clj" :required-by ["app.ui.views"])
               (page "app.model.order" "src/app/model/order.clj")
               (page "app.model.user-test" "test/app/model/user_test.clj" :requires ["app.model.user"])
               (page "app.ui.views" "src/app/ui/views.clj" :requires ["app.model.user"] :required-by ["app.core"])
               (page "app.ui.events" "src/app/ui/events.clj")
               (page "app.core" "src/app/core.clj" :requires ["app.ui.views"] :metrics {:lines 400})))

(defn- close? [expected actual]
  (<= (Math/abs (double (- expected actual))) 0.000001))

(defn- same-color? [expected actual]
  (and (= (count expected) (count actual)) (every? true? (map close? expected actual))))

(defn- same-colors?
  "True when two {:day :night} colors agree on both skies."
  [expected actual]
  (every? (fn [theme-name] (same-color? (get expected theme-name) (get actual theme-name))) [:day :night]))

(defn- hue-distance [first-hue second-hue]
  (let [difference (mod (Math/abs (double (- first-hue second-hue))) 360)]
    (min difference (- 360 difference))))

(defn- oklab-distance [first-oklch second-oklch]
  (Math/sqrt (reduce + (map (fn [first-part second-part] (Math/pow (- first-part second-part) 2))
                            (color/oklch->oklab first-oklch)
                            (color/oklch->oklab second-oklch)))))

(defn- colors-with [enabled snapshot]
  (let [registry layers/built-in]
    (layers/page-colors registry (layers/prepare registry snapshot) (set enabled) snapshot)))

(deftest pages-belong-to-the-folders-of-their-files
  (is (= ["src" "app" "model"] (layers/folder-path (page "app.model.user" "src/app/model/user.clj"))))
  (is (= ["web" "js"] (layers/folder-path {:kind :module :name "web/js/app.js" :files [{:path "web/js/app.js"}]})))
  (is (= [] (layers/folder-path {:kind :module :name "setup.py" :files [{:path "setup.py"}]})))
  (is (= "src/app/model" (layers/folder-name ["src" "app" "model"])))
  (is (= "top level" (layers/folder-name []))))

(deftest blending-mixes-in-oklab
  (let [green [0.62 0.15 145]
        blue [0.55 0.15 258]
        mixed (layers/blend [[1 green] [1 blue]])]
    (is (nil? (layers/blend [])))
    (is (nil? (layers/blend [[1 nil] [0 green]])))
    (is (same-color? green (layers/blend [[2 green]])))
    (testing "an even mix lies between its parts"
      (is (< (oklab-distance mixed green) (oklab-distance green blue)))
      (is (< (oklab-distance mixed blue) (oklab-distance green blue)))
      (is (< (hue-distance (nth mixed 2) 201) 12)))
    (testing "weight pulls a mix toward the heavier color"
      (is (< (oklab-distance (layers/blend [[3 green] [1 blue]]) green) (oklab-distance mixed green))))
    (testing "opposite hues keep half the chroma they cancel"
      (is (<= 0.07 (second (layers/blend [[1 [0.6 0.15 90]] [1 [0.6 0.15 270]]])) 0.08)))
    (testing "colors that are not red never mix to the reds of cycles and parse errors"
      (let [[_ _ hue] (layers/blend [[1 [0.6 0.15 335]] [1 [0.6 0.15 80]]])]
        (is (not (< 15 hue 45))))
      (is (< 15 (nth (layers/blend [[1 [0.6 0.19 32]] [1 [0.6 0.15 60]]]) 2) 45) "a red part keeps the mix red"))))

(deftest folder-layer-tells-folders-apart
  (let [colors (colors-with [:folder] app-snapshot)
        hue-of (fn [id] (nth (:day (get colors id)) 2))]
    (testing "pages in one folder share its color"
      (is (= (get colors "app.model.user") (get colors "app.model.order"))))
    (testing "different folders get distinct colors, tests included"
      (doseq [[first-id second-id] [["app.model.user" "app.ui.views"] ["app.model.user" "app.core"]
                                    ["app.model.user" "app.model.user-test"] ["app.ui.views" "app.core"]]]
        (is (> (hue-distance (hue-of first-id) (hue-of second-id)) 40) (str first-id " and " second-id))))
    (testing "folder colors stay clear of the reds that mark cycles"
      (doseq [[_ {:keys [day night]}] colors]
        (is (<= 55 (nth day 2) 335))
        (is (<= 55 (nth night 2) 335)))))
  (let [nested (snapshot-of (page "lib.db.core" "src/lib/db/core.clj")
                            (page "lib.db.pool.a" "src/lib/db/pool/a.clj")
                            (page "lib.db.migrate.b" "src/lib/db/migrate/b.clj")
                            (page "lib.web.handler" "src/lib/web/handler.clj")
                            (page "lib.web.routes.c" "src/lib/web/routes/c.clj"))
        colors (colors-with [:folder] nested)
        hue-of (fn [id] (nth (:day (get colors id)) 2))
        lightness-of (fn [id] (first (:day (get colors id))))
        tree-order ["lib.db.core" "lib.db.migrate.b" "lib.db.pool.a" "lib.web.handler" "lib.web.routes.c"]]
    (testing "a folder's subfolders take the hues that follow its own, in the order of the file tree"
      (is (apply < (map hue-of tree-order))))
    (testing "neighbors in the tree differ in lightness as well as hue"
      (doseq [[earlier later] (partition 2 1 tree-order)]
        (is (>= (Math/abs (- (lightness-of earlier) (lightness-of later))) 0.07) (str earlier " and " later))))))

(deftest dependents-layer-follows-the-pages-that-use-it
  (let [folder-colors (colors-with [:folder] app-snapshot)
        colors (colors-with [:dependents] app-snapshot)]
    (is (same-colors? (get folder-colors "app.ui.views") (get colors "app.model.user"))
        "a page used only from app.ui takes app.ui's folder color")
    (is (same-colors? (get folder-colors "app.ui.views") (get colors "app.core"))
        "a page nothing uses takes the colors of what it uses")
    (is (nil? (get colors "app.model.order")) "a page with no links leaves its color to other layers")))

(deftest pages-blend-the-layers-turned-on
  (testing "with every layer off, no page has a color and every page is gray"
    (is (every? nil? (vals (colors-with [] app-snapshot))))
    (doseq [theme-name [:day :night]]
      (let [{:keys [panel accent]} (palette/page-oklch theme-name {:blend nil})]
        (is (< (second panel) 0.015))
        (is (< (second accent) 0.015)))))
  (testing "two layers blend into a color between their own"
    (let [folder (:day (get (colors-with [:folder] app-snapshot) "app.model.user"))
          dependents (:day (get (colors-with [:dependents] app-snapshot) "app.model.user"))
          both (:day (get (colors-with [:folder :dependents] app-snapshot) "app.model.user"))]
      (is (< (oklab-distance both folder) (oklab-distance folder dependents)))
      (is (< (oklab-distance both dependents) (oklab-distance folder dependents)))))
  (testing "a layer that leaves a page alone takes no part in its blend"
    (is (= (get (colors-with [:folder] app-snapshot) "app.model.order")
           (get (colors-with [:folder :dependents] app-snapshot) "app.model.order"))))
  (testing "cycles and parse errors show over any blend"
    (let [blend (get (colors-with [:folder] app-snapshot) "app.core")]
      (is (= (palette/color-of :night :cycle) (:accent (palette/page-oklch :night {:blend blend :cyclic? true}))))
      (is (= (palette/color-of :day :parse-error) (:accent (palette/page-oklch :day {:blend blend :parse-error? true}))))
      (is (= (palette/color-of :day :cycle-warning)
             (:accent (palette/page-oklch :day {:blend blend :cycle-warning? true})))))))

(deftest language-layer-and-defaults
  (let [mixed (snapshot-of (page "app.core" "src/app/core.clj")
                           (page "tools/build.py" "tools/build.py" :kind :module :language :python))
        colors (colors-with [:language] mixed)]
    (is (same-colors? {:day (palette/language-accent :day :python) :night (palette/language-accent :night :python)}
                      (get colors "tools/build.py")))
    (is (not= (get colors "app.core") (get colors "tools/build.py")))
    (is (= [:language :folder :dependents] (layers/default-keys mixed)))
    (is (= [:folder :dependents] (layers/default-keys app-snapshot)))
    (testing "a few pages of another language leave the language layer off"
      (is (= [:folder :dependents]
             (layers/default-keys (apply snapshot-of (page "web/site.css" "web/site.css" :kind :module :language :css)
                                         (for [index (range 12)]
                                           (page (str "app.page" index) (str "src/app/page" index ".clj"))))))))))

(deftest a-language-family-shares-one-entry
  (let [native (snapshot-of (page "cpp/board.cpp" "cpp/board.cpp" :kind :module :language :cpp)
                            (page "cpp/ring.hpp" "cpp/ring.hpp" :kind :module :language :cpp)
                            (page "src/filter.c" "src/filter.c" :kind :module :language :c))
        contexts (layers/prepare layers/built-in native)
        language-colors (colors-with [:language] native)]
    (is (= ["C++ and C"] (map :label ((:legend layers/language-layer) (:language contexts)))))
    (is (same-colors? (get language-colors "src/filter.c") (get language-colors "cpp/board.cpp")))))

(deftest languages-without-an-adapter-are-unclassified
  (let [mixed (snapshot-of (page "app.core" "src/app/core.clj" :metrics {:lines 10 :complection 2})
                           (page "app.util" "src/app/util.clj" :metrics {:lines 20 :complection 6})
                           (page "native/Board.java" "native/Board.java" :kind :module :language :java
                                 :metrics {:lines 900})
                           (page "native/board.go" "native/board.go" :kind :module :language :go :metrics {:lines 40}))
        contexts (layers/prepare layers/built-in mixed)
        node (fn [id] (get-in mixed [:namespaces id]))
        language-colors (colors-with [:language] mixed)]
    (testing "every language read only as text takes the one unclassified color"
      (is (same-colors? palette/unclassified (get language-colors "native/Board.java")))
      (is (same-colors? palette/unclassified (get language-colors "native/board.go")))
      (is (= ["Clojure" "Unclassified: Go and Java"]
             (map :label ((:legend layers/language-layer) (:language contexts)))))
      (is (= "Java, unclassified" ((:describe layers/language-layer) (:language contexts) (node "native/Board.java")))))
    (testing "measurements that need an adapter leave such pages unclassified and outside the range"
      (is (same-colors? palette/unclassified
                        (layers/layer-color layers/complection-layer (:complection contexts) (node "native/Board.java"))))
      (is (= "unclassified" ((:describe layers/complection-layer) (:complection contexts) (node "native/Board.java"))))
      (is (same-color? (palette/color-of :day :complection-high)
                       (:day (layers/layer-color layers/complection-layer (:complection contexts) (node "app.util")))))
      (is (= palette/unclassified (:unclassified ((:legend layers/complection-layer) (:complection contexts))))))
    (testing "length measures every page"
      (is (same-color? (palette/color-of :day :length-high)
                       (:day (layers/layer-color layers/length-layer (:length contexts) (node "native/Board.java")))))
      (is (nil? (:unclassified ((:legend layers/length-layer) (:length contexts))))))))

(deftest heat-layers-run-from-least-to-most
  (let [contexts (layers/prepare layers/built-in app-snapshot)
        color-of (fn [id] (layers/layer-color layers/length-layer (:length contexts) (get-in app-snapshot [:namespaces id])))
        {:keys [low high]} ((:legend layers/length-layer) (:length contexts))]
    (is (same-color? (:day low) (:day (color-of "app.model.order"))) "the shortest page takes the low color")
    (is (same-color? (:day high) (:day (color-of "app.core"))) "the longest page takes the high color")
    (is (= "400 lines" ((:describe layers/length-layer) (:length contexts) (get-in app-snapshot [:namespaces "app.core"])))))
  (testing "a snapshot where every page measures the same stays at the low end"
    (let [even (snapshot-of (page "a.one" "src/a/one.clj") (page "a.two" "src/a/two.clj"))
          context ((:prepare layers/complection-layer) even)]
      (is (same-color? (palette/color-of :day :complection-low)
                       (:day (layers/layer-color layers/complection-layer context (get-in even [:namespaces "a.one"]))))))))

(deftest cloud-file-layers-join-the-built-in-ones
  (let [registry (layers/registry [{:key :cloud/tests :label "Tests" :patterns ["test/"] :colors ["pink"]}
                                   {:key :cloud/long :label "Long" :metric :lines :colors ["green" "#cc3311"]}])
        contexts (layers/prepare registry app-snapshot)
        colors (layers/page-colors registry contexts #{:cloud/tests} app-snapshot)
        heat (layers/page-colors registry contexts #{:cloud/long} app-snapshot)]
    (is (= (concat (map :key layers/built-in) [:cloud/tests :cloud/long]) (map :key registry)))
    (is (same-colors? (get layers/named-colors "pink") (get colors "app.model.user-test")))
    (is (nil? (get colors "app.model.user")))
    (is (same-color? (get-in layers/named-colors ["green" :day]) (:day (get heat "app.model.order"))))
    (is (same-color? (:day (layers/color-named "#cc3311")) (:day (get heat "app.core")))))
  (testing "a hex color brightens by night and unknown names give nothing"
    (is (<= 0.66 (first (:night (layers/color-named "#202060")))))
    (is (nil? (layers/color-named "chartreuse"))))
  (is (= config/color-names (set (keys layers/named-colors)))
      "the colors a cloud file may name are the ones layers can draw"))

(deftest any-layer-map-extends-the-registry
  (let [even-lines {:key :even-lines
                    :label "Even lines"
                    :kind :category
                    :prepare (fn [_snapshot] {:even (get layers/named-colors "teal")})
                    :color (fn [{:keys [even]} node] (when (even? (get-in node [:metrics :lines] 0)) even))
                    :describe (fn [_context node] (str (get-in node [:metrics :lines]) " lines"))
                    :legend (fn [{:keys [even]}] [(assoc even :label "even")])}
        registry (conj layers/built-in even-lines)
        contexts (layers/prepare registry app-snapshot)
        colors (layers/page-colors registry contexts #{:even-lines} app-snapshot)]
    (is (same-colors? (get layers/named-colors "teal") (get colors "app.core")))
    (is (= [{:key :even-lines :label "Even lines" :color (get layers/named-colors "teal") :description "400 lines"}]
           (layers/breakdown registry contexts #{:even-lines} (get-in app-snapshot [:namespaces "app.core"]))))))

(deftest layers-explain-themselves
  (let [contexts (layers/prepare layers/built-in app-snapshot)
        user (get-in app-snapshot [:namespaces "app.model.user"])]
    (is (= [["Folder" "src/app/model"] ["Dependents" "used by src/app/ui 1"]]
           (map (juxt :label :description) (layers/breakdown layers/built-in contexts #{:folder :dependents} user))))
    (is (= "src/app/model" (:label (first ((:legend layers/folder-layer) (:folder contexts))))))
    (is (= "used by src/app/model" (:label (first ((:legend layers/dependents-layer) (:dependents contexts))))))
    (is (= (nth (:day (layers/layer-color layers/folder-layer (:folder contexts) user)) 2)
           (get (layers/folder-hues contexts app-snapshot) "app.model.user")))))

(deftest folder-clouds-tint-by-the-layers-shown
  (is (= :strong (layers/cloud-tint #{:folder} true)))
  (is (= :slight (layers/cloud-tint #{:folder :dependents} true)))
  (is (= :slight (layers/cloud-tint #{} true)))
  (is (nil? (layers/cloud-tint #{:folder} false))))

(deftest chosen-colors-replace-a-layers-own
  (let [teal (get layers/named-colors "teal")
        pink (get layers/named-colors "pink")
        overrides {:folder {"src/app/ui" teal}
                   :language {:clojure pink}
                   :length {:low teal}}
        contexts (layers/prepare layers/built-in app-snapshot overrides)
        colors (fn [enabled] (layers/page-colors layers/built-in contexts (set enabled) app-snapshot))
        views (get-in app-snapshot [:namespaces "app.ui.views"])]
    (testing "a folder's chosen color colors its pages and the pages they serve"
      (is (same-colors? teal (get (colors [:folder]) "app.ui.views")))
      (is (same-colors? teal (get (colors [:dependents]) "app.model.user")))
      (is (not (same-colors? teal (get (colors [:folder]) "app.core")))))
    (testing "a language and a ramp end take their chosen colors"
      (is (same-colors? pink (get (colors [:language]) "app.core")))
      (is (same-colors? teal (get (colors [:length]) "app.model.order"))))
    (testing "the legend and the inspector name what a click would change"
      (is (some #(= [:folder "src/app/ui"] (:target %)) ((:legend layers/folder-layer) (:folder contexts))))
      (is (= [:length :low] (:low-target ((:legend layers/length-layer) (:length contexts)))))
      (is (= {:folder [:folder "src/app/ui"] :language [:language :clojure]}
             (into {} (keep (fn [{layer-key :key :keys [target]}] (when target [layer-key target])))
                   (layers/breakdown layers/built-in contexts #{:folder :language :dependents} views)))))
    (testing "a target's color is the one chosen for it, or the layer's own"
      (is (same-colors? teal (layers/target-color layers/built-in contexts [:folder "src/app/ui"])))
      (is (= (get-in (layers/prepare layers/built-in app-snapshot) [:folder :colors ["src" "app" "model"]])
             (layers/target-color layers/built-in contexts [:folder "src/app/model"]))))))

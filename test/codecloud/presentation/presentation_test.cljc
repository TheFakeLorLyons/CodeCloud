(ns codecloud.presentation.presentation-test
  (:require [clojure.string :as string]
            [clojure.test :refer [deftest is testing]]
            [codecloud.model.source :as source]
            [codecloud.presentation.code-colors :as code-colors]
            [codecloud.presentation.color :as color]
            [codecloud.presentation.highlight :as highlight]
            [codecloud.presentation.layout :as layout]
            [codecloud.presentation.opacity :as opacity]
            [codecloud.presentation.page :as page]
            [codecloud.presentation.palette :as palette]
            [codecloud.presentation.scene :as scene]
            [codecloud.presentation.sky :as sky]
            [codecloud.presentation.zoom :as zoom]))

(defn- own-hex [theme-name token-kind]
  (:color (code-colors/style code-colors/own theme-name token-kind)))

(defn- close? [expected actual tolerance]
  (<= (Math/abs (double (- expected actual))) tolerance))

(defn- same-color?
  "True when two colors agree in every component, whether the numbers came
  out as integers or as floating point."
  [expected actual]
  (and (= (count expected) (count actual))
       (every? true? (map #(close? %1 %2 0.000001) expected actual))))

(deftest color-conversions
  (doseq [hex ["#ffffff" "#000000" "#3fae5a" "#6fa8dc" "#e0605a"]]
    (is (= hex (color/rgb->hex (color/oklch->rgb (color/rgb->oklch (color/hex->rgb hex)))))
        (str hex " survives a round trip through Oklch")))
  (testing "colors outside sRGB lose chroma, not hue or lightness"
    (let [rgb (color/oklch->rgb [0.7 0.4 150])]
      (is (every? #(<= 0 % 1) rgb))
      (is (close? 0.7 (first (color/rgb->oklch rgb)) 0.01))))
  (testing "hue mixing takes the short way around"
    (is (close? 0 (mod (nth (color/mix-oklch [0.5 0.1 350] [0.5 0.1 10] 0.5) 2) 360) 0.0001)))
  (is (close? 21 (color/contrast-ratio [1 1 1] [0 0 0]) 0.01)))

(deftest folder-clouds-take-their-tint
  (testing "by day a cloud takes its folder's hue, more strongly when strong"
    (let [chroma-of (fn [tint] (nth (:shade (palette/folder-cloud-oklch :day 120 tint)) 1))]
      (is (close? 120 (nth (:light (palette/folder-cloud-oklch :day 120 :strong)) 2) 0.000001))
      (is (< (chroma-of :slight) (chroma-of :strong)))))
  (testing "without a tint or a hue a cloud stays plain"
    (is (= {:light (palette/color-of :day :cloud) :shade (palette/color-of :day :cloud-shade)}
           (palette/folder-cloud-oklch :day 120 nil)
           (palette/folder-cloud-oklch :day nil :strong))))
  (testing "by night every cloud stays a colorful moonlit blue"
    (doseq [tint [:slight :strong]
            folder-hue (range 65 336 5)
            tone [:light :shade]
            :let [[_ chroma hue] (get (palette/folder-cloud-oklch :night folder-hue tint) tone)]]
      (is (and (> chroma 0.012) (<= 225 hue 295))
          (str "a folder of hue " folder-hue " with a " tint " tint")))))

(deftest palette-reads-on-every-page
  (testing "a blend keeps its hue and holds its lightness to what reads on each sky"
    (let [blend {:day [0.95 0.15 150] :night [0.30 0.15 150]}
          day (palette/page-oklch :day {:blend blend})
          night (palette/page-oklch :night {:blend blend})]
      (is (close? 150 (nth (:accent day) 2) 0.000001))
      (is (<= (first (:accent day)) 0.80))
      (is (>= (first (:accent night)) 0.58))))
  (testing "lighter blends make lighter glass, so light and dark layers still show"
    (let [glass-lightness (fn [theme-name lightness]
                            (first (:panel (palette/page-oklch theme-name {:blend {theme-name [lightness 0.15 200]}}))))]
      (is (< (glass-lightness :day 0.45) (glass-lightness :day 0.75)))
      (is (< (glass-lightness :night 0.6) (glass-lightness :night 0.9)))))
  (is (same-color? (palette/color-of :night :cycle)
                   (:accent (palette/page-oklch :night {:blend {:night [0.7 0.15 200]} :cyclic? true}))))
  (doseq [theme-name [:day :night]
          hue (range 0 360 15)
          lightness [0.4 0.55 0.7 0.85 0.95]
          chroma [0 0.08 0.19]]
    (let [panel (color/oklch->rgb (:panel (palette/page-oklch theme-name {:blend {theme-name [lightness chroma hue]}})))
          label (str " on a " (name theme-name) " page blended to " [lightness chroma hue])]
      (is (>= (color/contrast-ratio (color/hex->rgb (own-hex theme-name :text)) panel) 6)
          (str "code text" label))
      (doseq [token-kind [:comment :string :keyword :number :special :definition :reader]]
        (is (>= (color/contrast-ratio (color/hex->rgb (own-hex theme-name token-kind)) panel) 3)
            (str (name token-kind) " tokens" label)))))
  (doseq [theme-name [:day :night]
          panel-key [:page-cycle :page-parse-error :panel]]
    (let [panel (color/oklch->rgb (palette/color-of theme-name panel-key))
          label (str " on the " (name theme-name) " " (name panel-key))]
      (is (>= (color/contrast-ratio (color/hex->rgb (own-hex theme-name :text)) panel) 6)
          (str "code text" label))
      (doseq [token-kind [:comment :string :keyword :number :special :definition :reader]]
        (is (>= (color/contrast-ratio (color/hex->rgb (own-hex theme-name token-kind)) panel) 3)
            (str (name token-kind) " tokens" label)))))
  (doseq [theme-name [:day :night]]
    (is (>= (color/contrast-ratio (color/oklch->rgb (palette/color-of theme-name :ink))
                                  (color/oklch->rgb (palette/color-of theme-name :panel)))
            7))))

(deftest page-geometry
  (let [short-page (page/geometry [{:path "a.clj" :line-count 40 :longest-line 60}] 0.45)
        long-page (page/geometry [{:path "b.clj" :line-count 600 :longest-line 120}] 0.45)]
    (testing "a page's size comes from its code at one world unit per line"
      (is (= [1 40] ((juxt :columns :rows) short-page)))
      (is (close? (+ (* 2 page/padding) (* 0.45 65)) (:width short-page) 0.000001))
      (is (close? (+ (* 2 page/padding) page/header-height 40) (:height short-page) 0.000001)))
    (testing "long files wrap into columns and long lines stop at the widest column"
      (is (= [3 200 100] ((juxt :columns :rows :code-characters) long-page))))
    (testing "points map back to the lines drawn there"
      (doseq [panel-line [0 1 199 200 401 599]]
        (let [[x y] (page/line-center long-page panel-line)]
          (is (= panel-line (page/line-at long-page [(+ x 2) (+ y 0.3)]))
              (str "panel line " panel-line)))))
    (is (nil? (page/line-at long-page [0 (/ (:height long-page) 2)])) "the name band holds no code")
    (is (nil? (page/line-at long-page [(- (page/column-left long-page 1) 1) 0])) "nor the gap between columns"))
  (let [files [{:path "a.clj" :line-count 3} {:path "a.cljs" :line-count 2}]]
    (is (= 7 (page/panel-lines files)))
    (is (= [{:path "a.clj" :line 1 :heading? true} {:path "a.clj" :line 1} {:path "a.clj" :line 3}
            {:path "a.cljs" :line 1 :heading? true} {:path "a.cljs" :line 2} nil]
           (map #(page/panel-line-origin files %) [0 1 3 4 6 7])))
    (is (= [1 6] [(page/origin-panel-line files "a.clj" 1) (page/origin-panel-line files "a.cljs" 2)]))
    (is (nil? (page/origin-panel-line files "b.clj" 1))))
  (is (= {:path "solo.clj" :line 1} (page/panel-line-origin [{:path "solo.clj" :line-count 9}] 0)))
  (is (= ["src/app" "" ""] (map source/directory-of ["src/app/core.clj" "build.clj" ""]))))

(defn- sample-graph [node-count]
  (let [ids (mapv #(str "app.module" (quot % 6) ".part" %) (range node-count))]
    {:nodes (mapv (fn [id]
                    (let [module (subs id 0 (string/index-of id ".part"))]
                      {:id id
                       :radius (+ 20 (* 160 (layout/unit-random id)))
                       :group module
                       :parent-group (str "area" (mod (count module) 3))}))
                  ids)
     :links (vec (for [index (range 1 node-count)
                       :let [target (nth ids (int (* index (layout/unit-random (str "link" index)))))]]
                   {:from (nth ids index) :to target}))}))

(defn- distance [[from-x from-y from-z] [to-x to-y to-z]]
  (Math/sqrt (+ (Math/pow (- from-x to-x) 2) (Math/pow (- from-y to-y) 2) (Math/pow (- from-z to-z) 2))))

(deftest layout-is-deterministic-and-roomy
  (is (= (layout/stable-hash "malli.core") (layout/stable-hash "malli.core")))
  (is (not= (layout/stable-hash "malli.core") (layout/stable-hash "malli.cora")))
  (is (= 3550712520 (layout/stable-hash "codecloud")) "the same hash on every platform")
  (let [{:keys [nodes links]} (sample-graph 60)
        settled (layout/settle! (layout/create-simulation nodes links {}) 900)
        again (layout/settle! (layout/create-simulation nodes links {}) 900)
        positions (layout/positions settled)
        radius-of (into {} (map (juxt :id :radius)) nodes)]
    (is (not (layout/moving? settled)))
    (is (= positions (layout/positions again)))
    (testing "pages keep their distance"
      (doseq [first-node nodes
              second-node nodes
              :when (neg? (compare (:id first-node) (:id second-node)))]
        (is (>= (distance (positions (:id first-node)) (positions (:id second-node)))
                (* 0.85 (+ (radius-of (:id first-node)) (radius-of (:id second-node)))))
            (str (:id first-node) " and " (:id second-node)))))
    (testing "pages of one folder sit closer than pages of different folders"
      (let [folder-of (into {} (map (juxt :id :group)) nodes)
            pairs (for [first-node nodes second-node nodes
                        :when (neg? (compare (:id first-node) (:id second-node)))]
                    [(= (folder-of (:id first-node)) (folder-of (:id second-node)))
                     (distance (positions (:id first-node)) (positions (:id second-node)))])
            average (fn [values] (/ (reduce + values) (count values)))]
        (is (< (average (map second (filter first pairs)))
               (* 0.7 (average (map second (remove first pairs))))))))
    (testing "a warm start begins where the last layout ended"
      (let [warm (layout/create-simulation nodes links positions)
            [x y z] (positions (:id (first nodes)))
            [warm-x warm-y warm-z] (layout/position warm (:id (first nodes)))]
        (is (every? #(< (Math/abs %) 0.000001) [(- x warm-x) (- y warm-y) (- z warm-z)]))
        (is (< @(:alpha warm) 0.5))))))

(deftest highlighting-tokens
  (let [[first-line second-line third-line] (highlight/tokenize-lines
                                             ["(defn greet \"Says hi"
                                              "  to someone.\" [name] ; note"
                                              "  #{:a ::b} #\"x\\\"y\" \\( 42 nil @state)"])]
    (is (= [[:bracket "("] [:special "defn"] [:space " "] [:definition "greet"] [:space " "] [:string "\"Says hi"]]
           first-line))
    (is (= [:string "  to someone.\""] (first second-line)))
    (is (= [:comment "; note"] (peek second-line)))
    (is (some #{[:bracket "#{"]} third-line))
    (is (some #{[:keyword "::b"]} third-line))
    (is (some #{[:string "#\"x\\\"y\""]} third-line))
    (is (some #{[:string "\\("]} third-line))
    (is (some #{[:number "42"]} third-line))
    (is (some #{[:constant "nil"]} third-line))
    (is (some #{[:reader "@"]} third-line)))
  (testing "every character lands in exactly one token"
    (let [line "(let [{:keys [a b]} m] (str/join \",\" #?(:clj [a] :cljs [b])) ##Inf #_ignored)"]
      (is (= line (apply str (map second (first (highlight/tokenize-lines [line]))))))))
  (testing "minimap runs keep columns, merge neighbors, and stay inside the page"
    (let [tokens [[:bracket "("] [:special "defn"] [:space " "] [:definition "greet"] [:space " "]
                  [:bracket "["] [:text "name"] [:bracket "]"]]]
      (is (= [[0 1 8] [1 4 5] [6 5 6] [12 1 8] [13 4 0] [17 1 8]] (highlight/line-runs tokens 100 16)))
      (is (= [[0 1 8] [1 4 5] [6 12 6]] (highlight/line-runs tokens 100 3)))
      (is (= [[0 1 8] [1 4 5] [6 4 6]] (highlight/line-runs tokens 10 16))))
    (is (= [[4 1 0]] (highlight/line-runs [[:space "\t"] [:text "x"]] 100 16)))
    (is (= [[0 2 1]] (highlight/line-runs [[:comment ";"] [:comment ";"]] 100 16)))))

(deftest zoom-math
  (is (= [0 1 2 4 5 5] (map zoom/editor-zoom-steps [100 105 125 150 175 200])))
  (is (= 5 (zoom/editor-zoom-steps 400)))
  (is (close? 0.6 (zoom/text-scale-after-steps 4) 0.0000001))
  (testing "zooming out of the editor shrinks its text, then opens the cloud"
    (is (= :shrink (:action (zoom/zoom-out-press 0 150))))
    (is (close? 0.7 (:text-scale (zoom/zoom-out-press 2 150)) 0.0000001))
    (is (= :open-cloud (:action (zoom/zoom-out-press 3 150))))
    (is (close? (/ 2 3) (:text-scale (zoom/zoom-out-press 3 150)) 0.0000001))
    (is (= {:action :open-cloud :text-scale 1} (zoom/zoom-out-press 0 100)))
    (is (= :shrink (:action (zoom/zoom-out-press 3 200))))
    (is (= :open-cloud (:action (zoom/zoom-out-press 4 200)))))
  (testing "a few deliberate notches past the limit fill the dive pressure"
    (let [after-notches (reduce (fn [pressure _] (zoom/press-dive pressure 100 0.05)) 0 (range 4))]
      (is (= 1 after-notches)))
    (is (< (zoom/press-dive 0 100 0) 0.5))
    (is (zero? (zoom/leak-dive 0.5 2))))
  (is (close? 10 (zoom/pixels-per-unit (zoom/distance-for-scale 10 50 900) 50 900) 0.000001)))

(deftest page-opacity
  (let [frame {:focus-distance 100 :viewport {:width 800 :height 600} :lifted-ids #{}}
        centered {:id "a" :visual {:emphasis 1.0} :appearance 1 :depth 100 :pixels-per-unit 10 :on-screen? true
                  :screen-x 400 :screen-y 300 :half-width-pixels 120 :half-height-pixels 90}
        in-front (assoc centered :id "b" :depth 60)]
    (testing "a page in the middle of the screen at the focus distance holds the pivot"
      (is (close? 1 (opacity/holding-pivot centered frame) 0.000001))
      (is (zero? (opacity/holding-pivot (assoc centered :screen-x 700) frame))))
    (testing "a page in front of a lifted page steps aside and lets the pointer through"
      (let [lifted-frame (assoc frame :lifted-ids #{"a"} :lift-depth 100)
            {lifted :opacity} (opacity/page-opacity centered lifted-frame)
            {stepped :opacity see-through? :see-through?} (opacity/page-opacity in-front lifted-frame)]
        (is (< stepped (* 0.2 lifted)))
        (is (true? see-through?))))
    (testing "a page covering the examined page steps aside"
      (let [examined (assoc centered :holding 1)
            {:keys [see-through?]} (opacity/page-opacity in-front (assoc frame :examined examined))]
        (is (true? see-through?))
        (is (false? (:see-through? (opacity/page-opacity centered (assoc frame :examined examined)))))))
    (is (close? 1 (opacity/covered-share centered in-front) 0.000001))
    (is (zero? (opacity/covered-share centered (assoc in-front :screen-x 1400))))))

(deftest sky-modes
  (is (= 0.0 (sky/clock-night-amount 12)))
  (is (= 1.0 (sky/clock-night-amount 23)))
  (is (= 1.0 (sky/clock-night-amount 3)))
  (is (< 0 (sky/clock-night-amount 18.5) 1))
  (is (= 1.0 (sky/night-amount :auto :dark 12)))
  (is (= 0.0 (sky/night-amount :auto :light 23)))
  (is (= [:day :twilight :night :clock :auto] (map sky/next-mode [:auto :day :twilight :night :clock])))
  (is (= 1.0 (sky/dusk-amount 0.5)))
  (testing "twilight holds the sunset with night pages"
    (is (= 1.0 (sky/dusk-amount (sky/night-amount :twilight :light 12))))
    (is (= 1.0 (sky/ink-amount (sky/night-amount :twilight :light 12)))))
  (testing "pages and panels turn early in the dusk and never rest halfway"
    (is (= 0.0 (sky/ink-amount 0.0) (sky/ink-amount 0.15)))
    (is (= 1.0 (sky/ink-amount 0.4) (sky/ink-amount 1.0)))
    (is (< 0 (sky/ink-amount 0.26) 1))))

(def ^:private small-snapshot
  {:id :working-tree
   :namespaces {"app.core" {:name "app.core" :kind :namespace :importance 1.0 :requires ["app.db"] :required-by []
                            :files [{:path "src/app/core.clj" :line-count 80 :longest-line 70}]}
                "app.db" {:name "app.db" :kind :namespace :importance 0.2 :requires [] :required-by ["app.core"]
                          :cycle-id 0
                          :files [{:path "src/app/db.clj" :line-count 30 :longest-line 50}]}
                "app.ui.views" {:name "app.ui.views" :kind :namespace :importance 0.0 :requires [] :required-by []
                                :parse-error? true
                                :files [{:path "src/app/ui/views.cljs" :line-count 12 :longest-line 40}]}
                "build.clj" {:name "build.clj" :kind :script :importance 0.1 :requires [] :required-by []
                             :files [{:path "build.clj" :line-count 20 :longest-line 30}]}}
   :edges [{:from "app.core" :to "app.db" :kind :load :cyclic? false}
           {:from "app.ui.views" :to "app.db" :kind :load :cyclic? false}]})

(deftest scene-view-model
  (is (= {:prefix "app.ui." :leaf "views"} (scene/label-parts {:name "app.ui.views" :kind :namespace})))
  (is (= {:prefix "" :leaf "user"} (scene/label-parts {:name "user" :kind :namespace})))
  (is (= {:prefix "scripts/" :leaf "deploy.clj"} (scene/label-parts {:name "scripts/deploy.clj" :kind :script})))
  (let [everything (set (keys (:namespaces small-snapshot)))
        bases (scene/page-bases small-snapshot :folders everything 0.5)
        visuals (scene/page-visuals small-snapshot {:focused-id "app.db"} bases)]
    (testing "a page's look of the moment adds to its bases and keeps them"
      (is (= bases (update-vals visuals #(dissoc % :color-spec :folder-hue :lifted? :emphasis :dirty?)))))
    (testing "a clicked page lifts and dims what is unrelated"
      (is (true? (get-in visuals ["app.db" :lifted?])))
      (is (= 0.9 (get-in visuals ["app.core" :emphasis])))
      (is (< (get-in visuals ["build.clj" :emphasis]) 0.5)))
    (is (true? (get-in visuals ["app.db" :cyclic?])))
    (is (true? (get-in visuals ["app.ui.views" :parse-error?])))
    (is (> (get-in visuals ["app.core" :geometry :height]) (get-in visuals ["app.db" :geometry :height])))
    (testing "pages gather into one cloud per folder"
      (is (= {"src/app" ["app.core" "app.db"] "src/app/ui" ["app.ui.views"] "" ["build.clj"]}
             (update-vals (scene/directory-visuals visuals "demo") :member-ids)))
      (is (= {:prefix "" :leaf "demo"} (select-keys (get (scene/directory-visuals visuals "demo") "") [:prefix :leaf])))
      (is (= {:prefix "src/app/" :leaf "ui"} (scene/directory-label "src/app/ui" "demo"))))
    (is (= [{:id "app.core" :group "src/app" :parent-group "src"}]
           (map #(select-keys % [:id :group :parent-group])
                (filter #(= "app.core" (:id %)) (scene/layout-nodes visuals)))))
    (testing "a page with an unsaved file carries a mark"
      (is (true? (get-in (scene/page-visuals small-snapshot {:dirty-paths #{"src/app/db.clj"}} bases) ["app.db" :dirty?])))
      (is (false? (get-in visuals ["app.db" :dirty?]))))
    (testing "a page whose file has a buffer takes the buffer's shape, keeping its columns while edited"
      (let [longer (vec (repeat 60 "(def x 1)"))
            buffers {"src/app/db.clj" {:lines longer}}
            shaped (scene/shaped-by-buffers visuals {:buffers buffers :snapshot small-snapshot} 0.5)
            edited (scene/shaped-by-buffers visuals {:buffers buffers :snapshot small-snapshot
                                                     :editing {:namespace-id "app.db" :columns 1}} 0.5)]
        (is (= 60 (get-in shaped ["app.db" :geometry :line-count])))
        (is (= [(get-in visuals ["app.db" :geometry :width]) (get-in visuals ["app.db" :geometry :height])]
               (get-in shaped ["app.db" :base-size])))
        (is (identical? (get visuals "app.core") (get shaped "app.core")))
        (is (= 1 (get-in edited ["app.db" :geometry :columns])))
        (is (true? (get-in edited ["app.db" :edited?])))
        (is (identical? visuals (scene/shaped-by-buffers visuals {:buffers buffers :snapshot (assoc small-snapshot :id "c1")} 0.5))))))
  (testing "filtered namespaces leave the scene with their edges"
    (let [shown #{"app.core" "app.db"}]
      (is (= shown (set (keys (scene/page-bases small-snapshot :folders shown 0.5)))))
      (is (= [["app.core" "app.db"]] (map (juxt :from :to) (scene/edge-visuals small-snapshot {} shown))))
      (is (= [{:from "app.core" :to "app.db"}] (scene/layout-links small-snapshot shown)))))
  (is (= #{"app.ui.views"} (scene/search-matches small-snapshot "UI view")))
  (is (= ["app.core" "app.db" "app.ui.views"]
         (scene/search-results small-snapshot "app" (set (keys (:namespaces small-snapshot))))))
  (is (= ["app.core"] (scene/search-results small-snapshot "app" #{"app.core" "build.clj"})))
  (is (= :required-by (:role (first (scene/edge-visuals small-snapshot {:focused-id "app.db"}
                                                        (set (keys (:namespaces small-snapshot))))))))
  (testing "edges recede on a ranked wall until their page is hovered"
    (let [shown (set (keys (:namespaces small-snapshot)))
          emphasis-of (fn [ui-state] (:emphasis (first (scene/edge-visuals small-snapshot ui-state shown))))]
      (is (< (emphasis-of {:arrangement :lines}) (emphasis-of {:arrangement :folders})))
      (is (== 1 (emphasis-of {:arrangement :lines :hovered-id "app.core"}))))))

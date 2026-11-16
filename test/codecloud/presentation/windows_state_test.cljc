(ns codecloud.presentation.windows-state-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.walk :as walk]
            [codecloud.presentation.page :as page]
            [codecloud.presentation.scene :as scene]
            [codecloud.presentation.ui-state :as ui-state]
            [codecloud.presentation.windows-state :as windows-state]))

(defn- sent [effects message-type]
  (filter #(= message-type (get-in % [1 :type])) effects))

(defn- after [state & events]
  (:state (ui-state/handle-all state events)))

(defn- in-doubles
  "`places` with every number a double, so a computed 120.0 equals 120 on
  the JVM as it does in JavaScript."
  [places]
  (walk/postwalk #(if (number? %) (double %) %) places))

(deftest windows-know-their-files-and-addresses
  (is (= :markdown (windows-state/file-kind "docs/Guide.MD")))
  (is (= :html (windows-state/file-kind "site\\index.htm")))
  (is (= :pdf (windows-state/file-kind "paper.pdf")))
  (is (= :image (windows-state/file-kind "pictures/plot.svg")))
  (is (nil? (windows-state/file-kind "src/core.clj")))
  (is (nil? (windows-state/file-kind ".md")) "a name that is all extension names no kind")
  (is (= "http://localhost:1971" (windows-state/web-address "localhost:1971")))
  (is (= "http://localhost:1971" (windows-state/web-address " 1971 ")) "a port alone is this computer's")
  (is (= "http://localhost:8888/lab" (windows-state/web-address ":8888/lab")))
  (is (nil? (windows-state/web-address "70000")) "past the last port is no address")
  (is (= "https://clojure.org/guides" (windows-state/web-address " https://clojure.org/guides ")))
  (is (nil? (windows-state/web-address "not a page")))
  (is (= "localhost:1971/notebook" (windows-state/window-title {:kind :web :source "HTTP://localhost:1971/notebook"})))
  (is (= "guide.md" (windows-state/window-title {:kind :markdown :source "docs/guide.md"})))
  (is (= "localhost:1971" (windows-state/window-label {:kind :web :source "http://localhost:1971/notebook?cell=2"})))
  (is (= "guide.md" (windows-state/window-label {:kind :markdown :source "docs/guide.md"}))))

(deftest a-file-opens-in-one-window
  (let [{opened :state effects :effects} (ui-state/handle ui-state/initial-state
                                                          [:open-window {:kind :markdown :source "docs/guide.md"}])]
    (is (= [{:id "window-1" :kind :markdown :source "docs/guide.md"}] (:windows opened)))
    (is (= [{:type :view/window-open :id "window-1" :kind :markdown :source "docs/guide.md"}]
           (map second (sent effects :view/window-open))))
    (is (= [[:fly-to-window "window-1"]] (filter #(= :fly-to-window (first %)) effects)))
    (is (= 1 (count (sent effects :view/save-preference))) "the windows are remembered")
    (testing "opening it again flies to its window"
      (let [{again :state again-effects :effects} (ui-state/handle opened [:open-window {:kind :markdown :source "docs/guide.md"}])]
        (is (= (:windows opened) (:windows again)))
        (is (= [[:fly-to-window "window-1"]] again-effects))))
    (testing "a new window takes the next number"
      (is (= ["window-1" "window-2"]
             (map :id (:windows (after opened [:open-window {:kind :web :source "localhost:3000"}]))))))
    (testing "an address that names no page opens nothing and says so"
      (let [refused (after opened [:open-window {:kind :web :source "not a page"}])]
        (is (= (:windows opened) (:windows refused)))
        (is (seq (:notices refused)))))))

(deftest windows-move-reload-and-close
  (let [opened (after ui-state/initial-state
                      [:open-window {:kind :web :source "localhost:3000"}]
                      [:pin {:kind :window :id "window-1"}])]
    (is (= [0 4 -2] (:offset (first (:windows (after opened [:move-window "window-1" [0 4 -2]])))))
        "a moved window keeps its offset")
    (is (= "http://localhost:4000/notes"
           (:source (first (:windows (after opened [:set-window-address "window-1" "localhost:4000/notes"]))))))
    (is (= 1 (count (sent (:effects (ui-state/handle opened [:reload-window "window-1"])) :view/window-open)))
        "reloading asks the host again")
    (let [{closed :state effects :effects} (ui-state/handle opened [:close-window "window-1"])]
      (is (empty? (:windows closed)))
      (is (= {:cells [] :shown? false :splits windows-state/even-splits} (:grid closed)) "a closed window leaves the grid")
      (is (= 1 (count (sent effects :view/window-close)))))))

(deftest windows-hide-from-the-cloud-and-stay-open
  (let [opened (after ui-state/initial-state
                      [:open-window {:kind :web :source "localhost:3000"}]
                      [:open-window {:kind :pdf :source "paper.pdf"}])
        {hidden :state effects :effects} (ui-state/handle opened [:toggle-window-shown "window-1"])
        saved (fn [effects] (get-in (first (sent effects :view/save-preference)) [1 :value]))]
    (is (= [true nil] (map :hidden? (:windows hidden))) "the window stays open, hidden from the cloud")
    (is (= [{:id "window-1" :kind :web :source "http://localhost:3000" :hidden? true}
            {:id "window-2" :kind :pdf :source "paper.pdf"}]
           (saved effects))
        "and the cloud remembers it hidden")
    (is (= [{:id "window-1" :kind :web :source "http://localhost:3000" :hidden? true}]
           (windows-state/valid-windows [{:id "window-1" :kind :web :source "http://localhost:3000" :hidden? true}]))
        "a remembered hidden window comes back hidden")
    (is (nil? (:hidden? (first (:windows (after hidden [:toggle-window-shown "window-1"])))))
        "the box shows it again")
    (let [{flown :state flown-effects :effects} (ui-state/handle hidden [:fly-to-window "window-1"])]
      (is (nil? (:hidden? (first (:windows flown)))) "flying to a hidden window shows it first")
      (is (some #{[:fly-to-window "window-1"]} flown-effects)))
    (is (nil? (:hidden? (first (:windows (after hidden [:open-window {:kind :web :source "localhost:3000"}])))))
        "opening its address again shows it too")
    (is (= [{:kind :window :id "window-1"}] (:cells (:grid (after hidden [:pin {:kind :window :id "window-1"}]))))
        "a hidden window still pins to the grid")))

(deftest the-grid-pins-up-to-four
  (let [pinned (after ui-state/initial-state
                      [:pin {:kind :page :id "app.core"}]
                      [:pin {:kind :page :id "app.ui"}]
                      [:pin {:kind :page :id "app.core"}])]
    (is (= {:cells [{:kind :page :id "app.core"} {:kind :page :id "app.ui"}] :shown? true :splits windows-state/even-splits}
           (:grid pinned))
        "pinning shows the grid, and pinning a page twice keeps one cell")
    (is (= #{"app.core" "app.ui"} (windows-state/pinned-page-ids pinned)))
    (is (windows-state/pinned? pinned {:kind :page :id "app.ui"}))
    (let [full (after pinned [:pin {:kind :page :id "app.a"}] [:pin {:kind :page :id "app.b"}] [:pin {:kind :page :id "app.c"}])]
      (is (= 4 (count (get-in full [:grid :cells]))))
      (is (some #(re-find #"holds four" %) (map str (:notices full)))))
    (testing "hiding the grid sends its pages back to the cloud and keeps its cells"
      (let [hidden (after pinned [:toggle-grid])]
        (is (false? (get-in hidden [:grid :shown?])))
        (is (empty? (windows-state/pinned-page-ids hidden)))
        (is (= 2 (count (get-in hidden [:grid :cells]))))))
    (testing "unpinning the last cell hides the grid"
      (is (= {:cells [] :shown? false :splits windows-state/even-splits}
             (:grid (after pinned [:unpin {:kind :page :id "app.core"}] [:unpin {:kind :page :id "app.ui"}])))))
    (testing "a page edited in the grid takes its own columns back when unpinned"
      (let [editing (assoc pinned :editing {:namespace-id "app.core" :path "src/app/core.clj" :columns 1 :rows 40})]
        (is (nil? (get-in (after editing [:unpin {:kind :page :id "app.core"}]) [:editing :columns])))))
    (testing "an empty grid explains how to fill it"
      (is (seq (:notices (after ui-state/initial-state [:toggle-grid])))))))

(deftest cells-fill-the-area-as-editor-groups-do
  (let [area {:left 0 :top 0 :width 250 :height 130}
        rectangles (fn [cell-count] (in-doubles (windows-state/cell-rectangles cell-count area 10 windows-state/even-splits)))]
    (is (= [] (rectangles 0)))
    (is (= (in-doubles [area]) (rectangles 1)))
    (is (= (in-doubles [{:left 0 :top 0 :width 120 :height 130} {:left 130 :top 0 :width 120 :height 130}])
           (rectangles 2)))
    (is (= (in-doubles [{:left 0 :top 0 :width 120 :height 130}
                        {:left 130 :top 0 :width 120 :height 60}
                        {:left 130 :top 70 :width 120 :height 60}])
           (rectangles 3)))
    (is (= (in-doubles [{:left 0 :top 0 :width 120 :height 60} {:left 130 :top 0 :width 120 :height 60}
                        {:left 0 :top 70 :width 120 :height 60} {:left 130 :top 70 :width 120 :height 60}])
           (rectangles 4)))))

(deftest borders-move-and-cells-trade-places
  (let [area {:left 0 :top 0 :width 250 :height 130}
        splits {:column 0.25 :row 0.75}]
    (testing "moved borders share the area unevenly"
      (is (= (in-doubles [{:left 0 :top 0 :width 60 :height 130}
                          {:left 70 :top 0 :width 180 :height 90}
                          {:left 70 :top 100 :width 180 :height 30}])
             (in-doubles (windows-state/cell-rectangles 3 area 10 splits)))))
    (testing "the gaps between cells are the borders that move"
      (is (= [] (windows-state/borders 1 area 10 splits)))
      (is (= (in-doubles [{:axis :column :left 60 :top 0 :width 10 :height 130}
                          {:axis :row :left 70 :top 90 :width 180 :height 10}])
             (in-doubles (windows-state/borders 3 area 10 splits))))
      (is (= (in-doubles {:axis :row :left 0 :top 90 :width 250 :height 10})
             (in-doubles (second (windows-state/borders 4 area 10 splits))))))
    (testing "a border follows the pointer, kept a fifth from either side"
      (is (= 0.5 (windows-state/share-at :column area 10 [125 40])))
      (is (= 0.8 (windows-state/share-at :row area 10 [40 129])))
      (is (= 0.2 (windows-state/share-at :column area 10 [3 40])))))
  (let [pinned (after ui-state/initial-state [:pin {:kind :page :id "app.core"}] [:pin {:kind :page :id "app.util"}])
        {resized :state effects :effects} (ui-state/handle pinned [:set-grid-splits {:column 0.3}])
        traded (after resized [:move-cell 0 1])]
    (testing "a moved border stays, and the grid remembers it"
      (is (= {:column 0.3 :row 0.5} (get-in resized [:grid :splits])))
      (is (= [:grid] (map #(get-in % [1 :key]) (sent effects :view/save-preference))))
      (is (= {:column 0.8 :row 0.5} (get-in (after pinned [:preview-grid-splits {:column 2}]) [:grid :splits]))))
    (testing "a cell dropped on another trades places with it"
      (is (= ["app.util" "app.core"] (map :id (get-in traded [:grid :cells]))))
      (is (= (:grid traded) (:grid (after traded [:move-cell 0 5]))) "a cell past the last stays put"))))

(deftest windows-float-in-a-row-beside-the-cloud
  (let [homes (windows-state/home-positions [{:id "window-1" :kind :web} {:id "window-2" :kind :markdown}]
                                            [[-50 -20 -10] [50 20 10]] 0.25 40)
        same? (fn [expected actual] (and (= (count expected) (count actual)) (every? true? (map == expected actual))))]
    (is (same? [(+ 50 40 160) 0 0] (get homes "window-1")) "the first stands half its width past the gap")
    (is (same? [(+ 50 40 320 40 107.5) 0 0] (get homes "window-2")))))

(deftest remembered-windows-and-grids-come-back-whole
  (let [remembered-windows [{:id "window-1" :kind :pdf :source "paper.pdf" :offset [1 2 3]}
                            {:id "window-1" :kind :web :source "http://localhost:1"}
                            {:id "window-2" :kind :video :source "film.mp4"}
                            {:id "window-3" :kind :image :source "plot.png" :offset "far"}]
        windows (windows-state/valid-windows remembered-windows)]
    (is (= [{:id "window-1" :kind :pdf :source "paper.pdf" :offset [1 2 3]} {:id "window-3" :kind :image :source "plot.png"}]
           windows))
    (is (= {:cells [{:kind :window :id "window-1"} {:kind :page :id "app.core"}] :shown? true :splits {:column 0.5 :row 0.5}}
           (windows-state/valid-grid {:cells [{:kind :window :id "window-1"} {:kind :window :id "window-9"}
                                              {:kind :page :id "app.core"} {:kind :page :id "app.core"}]
                                      :shown? true}
                                     windows)))
    (is (= {:column 0.2 :row 0.6} (:splits (windows-state/valid-grid {:cells [] :splits {:column 0.05 :row 0.6}} windows))))
    (testing "the host's first settings ask for every window, and settings sent again ask for none already loaded"
      (let [{first-state :state first-effects :effects} (ui-state/handle ui-state/initial-state
                                                                         [:host {:type :host/init :windows remembered-windows}])
            loaded (after first-state [:host {:type :host/window :id "window-1" :version 1 :url "paper-address"}])
            {again-effects :effects} (ui-state/handle loaded [:host {:type :host/init :windows remembered-windows}])]
        (is (= ["window-1" "window-3"] (map #(get-in % [1 :id]) (sent first-effects :view/window-open))))
        (is (= {:version 1 :url "paper-address"} (get-in loaded [:window-contents "window-1"])))
        (is (= ["window-3"] (map #(get-in % [1 :id]) (sent again-effects :view/window-open))))))
    (testing "content for a window that is gone is let go"
      (is (= {} (:window-contents (after ui-state/initial-state [:host {:type :host/window :id "window-7" :version 1 :url "x"}])))))))

(deftest the-title-can-keep-quiet
  (let [{quiet :state effects :effects} (ui-state/handle ui-state/initial-state [:toggle-title-summary])]
    (is (true? (:title-summary-hidden? quiet)))
    (is (= [{:type :view/save-preference :key :title-summary-hidden? :value true}] (map second effects)))
    (is (true? (:title-summary-hidden? (after ui-state/initial-state [:host {:type :host/init :title-summary-hidden? true}]))))))

(deftest pinned-pages-read-as-one-column
  (let [files [{:path "src/app/long.clj" :line-count 300 :longest-line 60}]
        visual {:files files :geometry (page/geometry files 0.6)}
        shaped (scene/pinned-shapes {"app.long" visual "app.other" visual} #{"app.long" "app.gone"} 0.6)]
    (is (< 1 (get-in visual [:geometry :columns])) "the page wraps into columns in the cloud")
    (is (= 1 (get-in shaped ["app.long" :geometry :columns])))
    (is (true? (get-in shaped ["app.long" :pinned?])))
    (is (= visual (get shaped "app.other")) "pages outside the grid keep their shape")
    (is (not (contains? shaped "app.gone")))))

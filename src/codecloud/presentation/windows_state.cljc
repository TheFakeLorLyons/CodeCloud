(ns codecloud.presentation.windows-state
  "Windows beside the cloud, and the grid that pins pages and windows to
  the screen. A window shows a web page, an HTML file, Markdown, a PDF,
  or an image, which the host loads and keeps current. The grid holds up
  to four cells, each a page or a window, laid out as VS Code lays out
  editor groups."
  (:require [clojure.string :as string]
            [codecloud.presentation.handling :refer [changed send-effect unchanged with-notice]]))

;; Windows

(def window-sizes
  "Each kind of window's size in CSS pixels, [width height], shown one to
  one when the cloud's code shows at the editor's size."
  {:web [1280 800]
   :html [1280 800]
   :markdown [860 1040]
   :pdf [860 1080]
   :image [960 720]})

(def ^:private kinds-by-extension
  {"html" :html "htm" :html
   "md" :markdown "markdown" :markdown
   "pdf" :pdf
   "png" :image "jpg" :image "jpeg" :image "gif" :image "webp" :image "svg" :image "avif" :image "bmp" :image})

(def file-extensions
  "Extensions of the files a window shows."
  (sort (keys kinds-by-extension)))

(defn file-kind
  "The kind of window that shows `file-path`, by its extension, or nil
  when no window shows such files."
  [file-path]
  (let [file-name (last (string/split (or file-path "") #"[/\\]"))
        dot (string/last-index-of file-name ".")]
    (when (and dot (pos? dot))
      (get kinds-by-extension (string/lower-case (subs file-name (inc dot)))))))

(defn- parse-number [digits]
  #?(:clj (Long/parseLong digits) :cljs (js/parseInt digits 10)))

(defn web-address
  "`text` as a web address, adding http:// to a bare host and port such as
  localhost:1971, and reading a port alone, such as 1971 or :1971 and any
  path after it, as this computer's, or nil when it names no web page."
  [text]
  (let [trimmed (string/trim (or text ""))
        [_ port-digits path] (re-matches #":?(\d{1,5})(/\S*)?" trimmed)
        most-port 65535]
    (cond
      (re-matches #"(?i)https?://\S+" trimmed) trimmed
      port-digits (let [port (parse-number port-digits)]
                    (when (<= 1 port most-port)
                      (str "http://localhost:" port path)))
      (re-matches #"[\w.\-]+(:\d+)?(/\S*)?" trimmed) (str "http://" trimmed)
      :else nil)))

(defn window-title [{:keys [kind source]}]
  (if (= :web kind)
    (string/replace (or source "") #"(?i)^https?://" "")
    (last (string/split (or source "") #"/"))))

(defn window-label
  "A window's name in a label from afar, a web page's host or a file's
  name, short enough to stand among the labels of pages."
  [{:keys [kind] :as window}]
  (cond-> (window-title window)
    (= :web kind) (-> (string/split #"[/?#]") first)))

(defn- next-window-id [windows]
  (let [numbers (keep #(some-> (re-matches #"window-(\d+)" (str (:id %))) second parse-number) windows)]
    (str "window-" (inc (reduce max 0 numbers)))))

(defn- offset? [value]
  (and (vector? value) (= 3 (count value)) (every? number? value)))

(defn valid-windows
  "The windows a host remembered, keeping each one well formed and the
  first of any id, else none."
  [remembered]
  (if (sequential? remembered)
    (->> remembered
         (filter (fn [{:keys [id kind source]}]
                   (and (string? id) (contains? window-sizes kind) (string? source) (not (string/blank? source)))))
         (map (fn [{:keys [offset hidden?] :as window}]
                (cond-> (select-keys window [:id :kind :source])
                  (offset? offset) (assoc :offset offset)
                  (true? hidden?) (assoc :hidden? true))))
         (reduce (fn [kept window] (if (some #(= (:id window) (:id %)) kept) kept (conj kept window))) []))
    []))

(defn window-of [state window-id]
  (some #(when (= window-id (:id %)) %) (:windows state)))

(defn home-positions
  "Where each window floats until moved, in a row from the right side of
  the cloud's box, given as its least and most corners, at the cloud's
  middle height and depth. Each window is `units-per-pixel` world units
  for each of its CSS pixels, and `gap` world units from its neighbors."
  [windows [[_least-x least-y least-z] [most-x most-y most-z]] units-per-pixel gap]
  (let [middle-y (* 0.5 (+ least-y most-y))
        middle-z (* 0.5 (+ least-z most-z))]
    (first (reduce (fn [[positions left] {:keys [id kind]}]
                     (let [width (* units-per-pixel (first (get window-sizes kind)))]
                       [(assoc positions id [(+ left (* 0.5 width)) middle-y middle-z]) (+ left width gap)]))
                   [{} (+ most-x gap)]
                   windows))))

;; The grid

(def grid-limit
  "Most cells the grid holds."
  4)

(def even-splits
  "Where the borders between the grid's cells stand until moved, as the
  share of the grid's width left of the border between columns and the
  share of its height above the border between rows."
  {:column 0.5 :row 0.5})

(defn- valid-share
  "`share` kept between a fifth and four fifths, so no cell shrinks past
  use, or `fallback` when `share` is no number."
  [share fallback]
  (let [least 0.2
        most 0.8]
    (if (number? share) (min most (max least share)) fallback)))

(defn valid-splits [splits]
  (let [splits (if (map? splits) splits {})]
    {:column (valid-share (:column splits) (:column even-splits))
     :row (valid-share (:row splits) (:row even-splits))}))

(defn valid-grid
  "The grid a host remembered, keeping cells of pages and of `windows`,
  without repeats and at most `grid-limit` of them, and where its borders
  stood."
  [remembered windows]
  (let [window-ids (set (map :id windows))
        cells (when (map? remembered) (:cells remembered))
        kept (->> (if (sequential? cells) cells [])
                  (filter (fn [{:keys [kind id]}]
                            (and (string? id) (or (= :page kind) (and (= :window kind) (contains? window-ids id))))))
                  (map #(select-keys % [:kind :id]))
                  distinct
                  (take grid-limit)
                  vec)]
    {:cells kept
     :shown? (boolean (and (seq kept) (true? (:shown? remembered))))
     :splits (valid-splits (when (map? remembered) (:splits remembered)))}))

(defn shown-cells
  "The grid's cells while it shows, else none."
  [{:keys [grid]}]
  (if (:shown? grid) (:cells grid) []))

(defn pinned-page-ids
  "Ids of the pages the shown grid holds."
  [state]
  (into #{} (keep #(when (= :page (:kind %)) (:id %))) (shown-cells state)))

(defn pinned? [state cell]
  (boolean (some #{(select-keys cell [:kind :id])} (get-in state [:grid :cells]))))

(defn- split-sizes
  "The widths of the columns and the heights of the rows of `area`, `gap`
  pixels apart, as [left right top bottom], the border between columns
  at the share :column of the width and the one between rows at the
  share :row of the height."
  [{:keys [width height]} gap {:keys [column row]}]
  (let [left-width (* column (- width gap))
        top-height (* row (- height gap))]
    [left-width (- width gap left-width) top-height (- height gap top-height)]))

(defn cell-rectangles
  "Screen rectangles {:left :top :width :height} of `cell-count` cells
  filling `area`, `gap` pixels apart, divided where `splits` puts the
  borders. One cell fills the area, two stand side by side, three put
  one on the left of two stacked on the right, and four make two rows of
  two."
  [cell-count {:keys [left top width height] :as area} gap splits]
  (let [[left-width right-width top-height bottom-height] (split-sizes area gap splits)
        right-left (+ left left-width gap)
        lower-top (+ top top-height gap)]
    (case cell-count
      0 []
      1 [{:left left :top top :width width :height height}]
      2 [{:left left :top top :width left-width :height height}
         {:left right-left :top top :width right-width :height height}]
      3 [{:left left :top top :width left-width :height height}
         {:left right-left :top top :width right-width :height top-height}
         {:left right-left :top lower-top :width right-width :height bottom-height}]
      [{:left left :top top :width left-width :height top-height}
       {:left right-left :top top :width right-width :height top-height}
       {:left left :top lower-top :width left-width :height bottom-height}
       {:left right-left :top lower-top :width right-width :height bottom-height}])))

(defn borders
  "The gaps between `cell-count` cells laid out as cell-rectangles lays
  them, each {:axis :left :top :width :height}, :axis :column for the gap
  between columns and :row for the one between rows. The gap between rows
  spans the right column under three cells and the whole grid under four."
  [cell-count {:keys [left top width height] :as area} gap splits]
  (let [[left-width right-width top-height] (split-sizes area gap splits)
        row-top (+ top top-height)]
    (cond-> []
      (>= cell-count 2) (conj {:axis :column :left (+ left left-width) :top top :width gap :height height})
      (= cell-count 3) (conj {:axis :row :left (+ left left-width gap) :top row-top :width right-width :height gap})
      (= cell-count 4) (conj {:axis :row :left left :top row-top :width width :height gap}))))

(defn share-at
  "The share of `area` that screen point [x y] puts the border on `axis`
  at, across the width for :column and down the height for :row."
  [axis {:keys [left top width height]} gap [x y]]
  (valid-share (if (= :column axis)
                 (/ (- x left (* 0.5 gap)) (- width gap))
                 (/ (- y top (* 0.5 gap)) (- height gap)))
               (get even-splits axis)))

;; Changes

(defn- remember-windows [state]
  (send-effect :view/save-preference {:key :windows :value (mapv #(select-keys % [:id :kind :source :offset :hidden?])
                                                                (:windows state))}))

(defn- with-window-hidden
  "The state with window `window-id` hidden from the cloud, or shown in it
  again, as `hidden?` says. A hidden window stays open, and in the grid
  when pinned there."
  [state window-id hidden?]
  (update state :windows (fn [windows]
                           (mapv #(cond
                                    (not= window-id (:id %)) %
                                    hidden? (assoc % :hidden? true)
                                    :else (dissoc % :hidden?))
                                 windows))))

(defn- remember-grid [state]
  (send-effect :view/save-preference {:key :grid :value (:grid state)}))

(defn- load-request [{:keys [id kind source]}]
  (send-effect :view/window-open {:id id :kind kind :source source}))

(defn restored
  "The state with the windows and grid the host remembered, and requests
  for what the windows without content show. Settings sent again leave
  loaded windows as they are."
  [state {:keys [windows grid]}]
  (let [kept-windows (valid-windows windows)
        restored-state (assoc state :windows kept-windows :grid (valid-grid grid kept-windows))]
    (apply changed restored-state (map load-request (remove #(contains? (:window-contents state) (:id %)) kept-windows)))))

(defn open-window
  "Opens a window on `source` of `kind`, a path relative to the cloud's
  folder or a web address, and flies to it, or flies to the window
  already showing it."
  [state {:keys [kind source]}]
  (let [shown-source (if (= :web kind) (web-address source) source)
        existing (some #(when (and (= kind (:kind %)) (= shown-source (:source %))) %) (:windows state))]
    (cond
      (or (not (contains? window-sizes kind)) (string/blank? shown-source))
      (unchanged (with-notice state (str "CodeCloud cannot show " (pr-str (or source "")) " in a window.")))

      existing (let [shown (-> state (assoc :windows-menu-open? false) (with-window-hidden (:id existing) false))]
                 (changed shown (when (:hidden? existing) (remember-windows shown)) [:fly-to-window (:id existing)]))

      :else (let [window {:id (next-window-id (:windows state)) :kind kind :source shown-source}
                  opened (-> state (update :windows conj window) (assoc :windows-menu-open? false))]
              (changed opened (remember-windows opened) (load-request window) [:fly-to-window (:id window)])))))

(defn- without-cell [grid cell]
  (let [cells (filterv #(not= (select-keys cell [:kind :id]) %) (:cells grid))]
    (assoc grid :cells cells :shown? (boolean (and (:shown? grid) (seq cells))))))

(defn close-window [state window-id]
  (let [closed (-> state
                   (update :windows (fn [windows] (filterv #(not= window-id (:id %)) windows)))
                   (update :window-contents dissoc window-id)
                   (update :grid without-cell {:kind :window :id window-id})
                   (assoc :menu nil))]
    (changed closed (remember-windows closed) (remember-grid closed) (send-effect :view/window-close {:id window-id}))))

(defn- move-window [state window-id offset]
  (let [moved (update state :windows (fn [windows]
                                        (mapv #(if (= window-id (:id %)) (assoc % :offset offset) %) windows)))]
    (changed moved (remember-windows moved))))

(defn- set-window-address [state window-id text]
  (if-let [address (web-address text)]
    (let [readdressed (update state :windows (fn [windows]
                                                (mapv #(if (= window-id (:id %)) (assoc % :source address) %) windows)))]
      (changed readdressed (remember-windows readdressed) (load-request (window-of readdressed window-id))))
    (unchanged (with-notice state (str (pr-str text) " is not a web address.")))))

(defn- reload-window [state window-id]
  (if-let [window (window-of state window-id)]
    (changed (assoc state :menu nil) (load-request window))
    (unchanged state)))

(defn show-web-page
  "Opens a window on web address `address` and flies to it, or loads the
  page again in the window already showing it, so a server started again
  shows afresh."
  [state address]
  (if-let [{:keys [id]} (some #(when (and (= :web (:kind %)) (= address (:source %))) %) (:windows state))]
    (let [{reloaded :state reload-effects :effects} (reload-window state id)]
      (apply changed reloaded (conj reload-effects [:fly-to-window id])))
    (open-window state {:kind :web :source address})))

(defn- toggle-window-shown [state window-id]
  (if-let [{:keys [hidden?]} (window-of state window-id)]
    (let [toggled (with-window-hidden state window-id (not hidden?))]
      (changed toggled (remember-windows toggled)))
    (unchanged state)))

(defn- fly-to-window
  "Flies to window `window-id`, showing it in the cloud first when it was
  hidden there."
  [state window-id]
  (let [closed (assoc state :windows-menu-open? false :menu nil)]
    (if (:hidden? (window-of state window-id))
      (let [shown (with-window-hidden closed window-id false)]
        (changed shown (remember-windows shown) [:fly-to-window window-id]))
      (changed closed [:fly-to-window window-id]))))

(defn- reveal-window [state window-id]
  (if-let [{:keys [kind source]} (window-of state window-id)]
    (changed (assoc state :menu nil) (send-effect :view/window-reveal {:kind kind :source source}))
    (unchanged state)))

(defn pin
  "Pins a page or window to the grid and shows the grid, or says the grid
  is full."
  [state cell]
  (let [cell (select-keys cell [:kind :id])
        {:keys [cells]} (:grid state)]
    (cond
      (some #{cell} cells)
      (let [shown (-> state (assoc-in [:grid :shown?] true) (assoc :menu nil))]
        (changed shown (remember-grid shown)))

      (>= (count cells) grid-limit)
      (unchanged (with-notice (assoc state :menu nil) "The grid holds four pages or windows. Unpin one to make room."))

      :else (let [pinned-state (-> state
                                   (update-in [:grid :cells] (fnil conj []) cell)
                                   (assoc-in [:grid :shown?] true)
                                   (assoc :menu nil))]
              (changed pinned-state (remember-grid pinned-state))))))

(defn unpin
  "Takes a page or window out of the grid. A page edited there keeps its
  caret and takes its columns in the cloud back."
  [state cell]
  (let [unpinned (cond-> (-> state (update :grid without-cell cell) (assoc :menu nil))
                   (and (= :page (:kind cell)) (= (:id cell) (get-in state [:editing :namespace-id])))
                   (assoc-in [:editing :columns] nil))]
    (changed unpinned (remember-grid unpinned))))

(defn- resized-grid
  "The state with the borders `splits` names moved, both or one of
  :column and :row."
  [state splits]
  (update-in state [:grid :splits] #(valid-splits (merge % splits))))

(defn- resize-grid [state splits]
  (let [resized (resized-grid state splits)]
    (changed resized (remember-grid resized))))

(defn- move-cell
  "Trades the places of the cells at indices `from` and `to`, so a cell
  dragged onto another takes its place."
  [state from to]
  (let [cells (vec (get-in state [:grid :cells]))]
    (if (and (contains? cells from) (contains? cells to) (not= from to))
      (let [moved (assoc-in state [:grid :cells] (assoc cells from (get cells to) to (get cells from)))]
        (changed moved (remember-grid moved)))
      (unchanged state))))

(defn- toggle-grid [state]
  (if (seq (get-in state [:grid :cells]))
    (let [toggled (update-in state [:grid :shown?] not)]
      (changed toggled (remember-grid toggled)))
    (unchanged (with-notice state "Pin a page or a window to the grid from its right-click menu."))))

(def handlers
  {:open-window open-window
   :close-window close-window
   :move-window move-window
   :set-window-address set-window-address
   :reload-window reload-window
   :reveal-window reveal-window
   :fly-to-window fly-to-window
   :toggle-window-shown toggle-window-shown
   :pick-window-file (fn [state] (changed (assoc state :windows-menu-open? false) (send-effect :view/window-pick-file {})))
   :toggle-windows-menu (fn [state] (unchanged (update state :windows-menu-open? not)))
   :close-windows-menu (fn [state] (unchanged (assoc state :windows-menu-open? false)))
   :pin pin
   :unpin unpin
   :toggle-grid toggle-grid
   :preview-grid-splits (fn [state splits] (unchanged (resized-grid state splits)))
   :set-grid-splits resize-grid
   :move-cell move-cell})

(def message-handlers
  "Handlers of the host's window messages, by type."
  {:host/window (fn [state {:keys [id] :as message}]
                  (unchanged (if (window-of state id)
                               (assoc-in state [:window-contents id] (dissoc message :type :id :protocol/version))
                               state)))
   :host/open-window (fn [state message] (open-window state message))})

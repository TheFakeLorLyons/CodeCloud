(ns codecloud.presentation.pairing
  "Pages beside their companions, as a C header beside its source file or
  a C++ module's interface beside its implementation.
  A row of paired pages sits on its root's folder cloud, the root being
  the page the others name as companion, and the layout places the row
  as one."
  (:require [codecloud.presentation.arrangement :as arrangement]))

(def header-modes
  "Where the pages of header files go, in menu order."
  [{:key :beside
    :label "Beside sources"
    :description "Each header or module interface next to the file it declares."}
   {:key :alone
    :label "On their own"
    :description "Headers placed like any other file."}
   {:key :hidden
    :label "Hidden"
    :description "Each header or module interface with a partner folds into it, and its lines follow."}])

(defn header-mode
  "The header mode named `mode-key`, falling back to :beside."
  [mode-key]
  (if (some #(= mode-key (:key %)) header-modes) mode-key :beside))

(defn- file-role [page]
  (some :file-role (:files page)))

(defn- companion [page]
  (some :companion (:files page)))

(defn declaration?
  "True for the page of a header, inline file, or module interface, given
  its node or its visual."
  [page]
  (contains? #{:header :inline :interface} (file-role page)))

(defn offers-header-modes?
  "True when a snapshot holds a header page or a page with a partner,
  which the header modes place."
  [snapshot]
  (boolean (some #(or (contains? #{:header :inline} (file-role %)) (companion %)) (vals (:namespaces snapshot)))))

(defn folding
  "Map from the id of each page with a partner, a header, inline file, or
  module interface, to its partner's: the pages hidden headers fold into.
  A page without a partner holds code found nowhere else and stays."
  [snapshot]
  (let [namespaces (:namespaces snapshot)]
    (into {}
          (keep (fn [[id node]]
                  (let [partner (companion node)]
                    (when (contains? namespaces partner) [id partner]))))
          namespaces)))

(defn reached-through-partners
  "Pairs [from partner] for each of `edges` running to a page with a
  partner, as a caller's include of a header reaches the source beside
  it. A use of the partner from the same page draws as that one line."
  [edges namespaces]
  (into #{}
        (keep (fn [{:keys [from to]}]
                (when-let [partner (companion (get namespaces to))]
                  [from partner])))
        edges))

(defn pairs?
  "True when `arrangement-key` sets paired pages side by side under
  `header-mode`. Ranked walls rank every page alone."
  [arrangement-key header-mode]
  (and (= :beside header-mode) (not (arrangement/ranked? arrangement-key))))

;; Rows

(def ^:private row-gap
  "World units between the pages of a row, a little more than between a
  page's columns, so the pages read as separate."
  4.0)

(def ^:private row-order
  "Where a file stands in its row: declarations first, inline definitions
  next, then the source."
  {:header 0 :interface 0 :inline 1 :source 2 :implementation 2})

(defn row-geometry
  "{:width :height :radius} of a row of pages of `geometries`, the radius
  half the row's diagonal as a page's is."
  [geometries]
  (let [width (+ (reduce + 0 (map :width geometries)) (* row-gap (max 0 (dec (count geometries)))))
        height (reduce max 0 (map :height geometries))]
    {:width width
     :height height
     :radius (* 0.5 (Math/sqrt (+ (* width width) (* height height))))}))

(defn- row-corner
  "Page-local [x y] of the top left corner of a row of pages of
  `geometries` centered on its root's place."
  [geometries]
  (let [{:keys [width height]} (row-geometry geometries)]
    [(* -0.5 width) (* 0.5 height)]))

(defn- row-shifts
  "Page-local [x y] from the middle of a row to the center of each page
  of `geometries`, laid left to right from the row's top left `corner`
  with their top edges aligned."
  [[corner-x corner-y] geometries]
  (first (reduce (fn [[shifts left] {:keys [width height]}]
                   [(conj shifts [(+ left (* 0.5 width)) (- corner-y (* 0.5 height))])
                    (+ left width row-gap)])
                 [[] corner-x]
                 geometries)))

(defn paired
  "`bases` with each page whose companion shows in the same folder set in
  a row with it. Every page of a row gains :pair-root, the companion's id,
  and :pair-shift, its place in the row, and the root gains :pair-members
  in row order and :row-corner. A header in another folder, as under
  include/, stays in its own folder's cloud."
  [bases]
  (let [root-of (fn [[id base]]
                  (let [root (companion base)
                        root-base (get bases root)]
                    (when (and root-base
                               (not= root id)
                               (nil? (companion root-base))
                               (= (:directory base) (:directory root-base)))
                      root)))
        root-by-satellite (into {} (keep (fn [entry] (when-let [root (root-of entry)] [(key entry) root]))) bases)
        satellites-by-root (group-by root-by-satellite (keys root-by-satellite))]
    (reduce-kv (fn [result root satellites]
                 (let [members (vec (sort-by (fn [id] [(get row-order (file-role (get bases id)) 2) id])
                                             (cons root satellites)))
                       geometries (map #(get-in bases [% :geometry]) members)
                       corner (row-corner geometries)]
                   (reduce (fn [result [id shift]]
                             (update result id assoc :pair-root root :pair-shift shift))
                           (update result root assoc :pair-members members :row-corner corner)
                           (map vector members (row-shifts corner geometries)))))
               bases
               satellites-by-root)))

(defn reshaped
  "`visuals` with each row holding a page an edit reshaped laid out again
  from the row's top left corner, so the page grows right and down and
  the pages after it make way."
  [visuals]
  (reduce-kv (fn [result _id {:keys [pair-members row-corner]}]
               (if (and pair-members (some #(get-in visuals [% :base-size]) pair-members))
                 (reduce (fn [result [member shift]] (assoc-in result [member :pair-shift] shift))
                         result
                         (map vector pair-members (row-shifts row-corner (map #(get-in visuals [% :geometry]) pair-members))))
                 result))
             visuals
             visuals))

(defn layout-visuals
  "The visuals the layout places: each row of paired pages as its root,
  claiming the room of the whole row as its pages are sized now."
  [visuals]
  (persistent!
   (reduce-kv (fn [placed id {:keys [pair-root pair-members] :as visual}]
                (cond
                  (and pair-root (not= id pair-root)) placed
                  pair-members (assoc! placed id (assoc visual :geometry (row-geometry (map #(get-in visuals [% :geometry])
                                                                                            pair-members))))
                  :else (assoc! placed id visual)))
              (transient {})
              visuals)))

(defn placed-links
  "`links` between the rows the layout places instead of their pages,
  without links inside a row."
  [links visuals]
  (let [placed-id (fn [id] (get-in visuals [id :pair-root] id))]
    (into []
          (comp (map (fn [{:keys [from to] :as link}] (assoc link :from (placed-id from) :to (placed-id to))))
                (remove #(= (:from %) (:to %)))
                (distinct))
          links)))

(defn pair-roots
  "Map from each page in a row, the root aside, to the row's root."
  [visuals]
  (into {}
        (keep (fn [[id {:keys [pair-root]}]] (when (and pair-root (not= id pair-root)) [id pair-root])))
        visuals))

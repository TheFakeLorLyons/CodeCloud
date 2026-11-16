(ns codecloud.render.results
  "Results of evaluations, shown on the pages they came from as a small
  tag after the last line of each evaluated form, kept in place every
  frame while its page's code is large enough to read."
  (:require [clojure.string :as string]
            [codecloud.render.dom :as dom]
            [codecloud.render.editing-view :as editing-view]
            [codecloud.render.pages :as pages]))

(defn create-layer
  "The layer holding result tags in the overlay, drawn over the labels in
  `labels-root` and under the panels."
  [^js labels-root]
  (let [root (dom/element [:div.result-layer {:aria-hidden "true"}])]
    (.after labels-root root)
    {:root root :tags (atom [])}))

(defn- tag-text
  "What a result's tag says, an arrow and the first line of the value, cut
  short, Loaded for a whole file, an ellipsis while the REPL works, or
  what went wrong."
  [{:keys [status text loading?]}]
  (let [longest-shown 80
        [first-line & more] (string/split-lines (or text ""))
        shown (if (> (count first-line) longest-shown) (str (subs first-line 0 longest-shown) "…") first-line)]
    (case status
      :pending "…"
      :value (if loading? "Loaded" (str "=> " shown (when more " …")))
      (str "× " shown))))

(defn- readable? [page-object]
  (let [least-line-pixels 8
        text-showing 0.5]
    (and (:on-screen? page-object)
         (>= (:pixels-per-unit page-object 0) least-line-pixels)
         (> (:text-share page-object 0) text-showing))))

(defn update-tags!
  "Places a tag for each inline result whose page shows readable code in
  the working tree, at the size of the code and within the page's right
  edge where it fits, and hides the others."
  [layer page-layer {:keys [inline-results snapshot]}]
  (let [placed (when (= :working-tree (:id snapshot))
                 (for [{:keys [namespace-id path line column] :as result} (vals inline-results)
                       :let [page-object (pages/page-of page-layer namespace-id)]
                       :when (and page-object (readable? page-object))
                       :let [point (editing-view/line-screen-point page-object path line column)]
                       :when point]
                   [result point page-object]))
        placed-count (count placed)]
    (doseq [[index [result [x y] page-object]] (map-indexed vector placed)]
      (let [^js tag (dom/pooled-element! (:root layer) (:tags layer) index [:span.inline-result])
            style (.-style tag)
            line-pixels (:pixels-per-unit page-object)
            font-pixels (max 8 (js/Math.round (* 0.8 line-pixels)))
            ;; The room from the line's end, less the tag's margin and
            ;; padding, to the page's right edge. A tag stays wide enough
            ;; for about a dozen characters.
            room (- (+ (:screen-x page-object) (:half-width-pixels page-object)) x (* 1.5 font-pixels))
            class-name (str "inline-result is-" (name (:status result)))]
        (dom/set-text! tag (tag-text result))
        (when (not= class-name (.-className tag))
          (set! (.-className tag) class-name))
        (set! (.-transform style) (str "translate(" (js/Math.round x) "px, " (js/Math.round y) "px) translateY(-50%)"))
        (set! (.-fontSize style) (str font-pixels "px"))
        (set! (.-maxWidth style) (str (js/Math.round (max (* 8 font-pixels) room)) "px"))
        (set! (.-opacity style) (str (:opacity page-object 1)))
        (set! (.-display style) "")))
    (doseq [^js tag (drop placed-count @(:tags layer))]
      (set! (.. tag -style -display) "none"))))

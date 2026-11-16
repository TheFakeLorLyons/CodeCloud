(ns codecloud.render.hud.pointer
  "The tooltip and the dive ring at the pointer. The ring replaces the
  tooltip while it fills."
  (:require [clojure.string :as string]
            [codecloud.counting :as counting]
            [codecloud.model.source :as source]
            [codecloud.presentation.scene :as scene]
            [codecloud.render.dom :as dom]
            [codecloud.render.hud.words :as words]
            [codecloud.render.typing :as typing]))

(def ^:private ring-size
  "Width and height of the dive ring in pixels."
  48)

(def ^:private ring-radius
  "Radius of the dive ring's circle in pixels."
  20)

(def tooltip-panel
  {:id :tooltip
   :mount (fn [_dispatch] {:root (dom/element [:div.hud-tooltip {:role "tooltip"}]) :shown (atom nil)})
   :update (fn [_panel _state] nil)})

(def dive-panel
  {:id :dive
   :mount (fn [_dispatch]
            (let [middle (/ ring-size 2)]
              {:root (dom/element
                      [:div.hud-dive
                       [:svg {:viewBox (str "0 0 " ring-size " " ring-size) :width ring-size :height ring-size
                              :aria-hidden "true"}
                        [:circle.dive-track {:cx middle :cy middle :r ring-radius}]
                        [:circle.dive-fill {:cx middle :cy middle :r ring-radius}]]
                       [:span.dive-label "Keep scrolling to open in the editor"]])}))
   :update (fn [_panel _state] nil)})

(defn- problem-line [{:keys [severity message source code]}]
  (let [reporter (string/join " " (remove string/blank? [source (when code (str "(" code ")"))]))]
    [:span {:class ["tooltip-problem" (str "is-" (name (or severity :information)))]}
     [:span.tooltip-problem-message message]
     (when (seq reporter) [:span.tooltip-problem-source reporter])]))

(defn- tooltip-content [{:keys [kind node origin folder rank ranked-by moved? unit-words editable? problems]}]
  (case kind
    :namespace (let [{:keys [metrics parse-error?]} node]
                 (list [:strong (words/name-heading (scene/label-parts node))]
                       (when rank [:span.tooltip-rank (str "#" rank " in " ranked-by)])
                       [:span (str (counting/counted (:lines metrics) "line" "lines") ", "
                                   (counting/counted (:functions metrics) "function" "functions") ", used "
                                   (counting/counted (:references metrics) "time" "times") " elsewhere")]
                       (when (pos? (:complection metrics 0))
                         [:span (str (words/points (:complection metrics)) " of complection")])
                       (cond
                         (scene/cyclic? node) [:span.tooltip-cycle "↻ Cyclic dependency"]
                         (scene/looped? node) [:span.tooltip-loop "↻ Cyclic import"]
                         :else nil)
                       (when parse-error? [:span.tooltip-parse-error "⚠ Parse error"])
                       [:span.tooltip-hint (if moved?
                                             "Click to bring closer, right-click to put back"
                                             "Click to bring closer")]))
    :code (if (seq problems)
            (let [most-listed 4]
              (list* [:strong (str (source/file-name (:path origin)) ", line " (:line origin))]
                     (concat (map problem-line (take most-listed problems))
                             (when (> (count problems) most-listed)
                               [[:span.tooltip-hint (str "and " (- (count problems) most-listed) " more")]]))))
            (list [:strong (str (source/file-name (:path origin)) ", line " (:line origin))]
                  [:span.tooltip-hint (if editable?
                                        "Click to edit here, or scroll in to open in the editor"
                                        "Scroll in to open in the editor")]
                  [:span.tooltip-hint (str (if (typing/mac?) "Cmd" "Ctrl") "-click a name to go to its definition")]))
    :folder (list [:strong (words/name-heading folder)]
                  [:span (let [[singular plural] (or unit-words ["namespace" "namespaces"])]
                           (counting/counted (count (:member-ids folder)) singular plural))]
                  [:span.tooltip-hint "Click to bring closer"])
    nil))

(defn- place-tooltip!
  "Puts the tooltip beside the pointer, on whichever side keeps it in the
  window."
  [^js tooltip [pointer-x pointer-y] [width height]]
  (let [gap 16
        margin 8
        left (if (> (+ pointer-x gap width) (- (.-innerWidth js/window) margin))
               (- pointer-x gap width)
               (+ pointer-x gap))
        top (if (> (+ pointer-y gap height) (- (.-innerHeight js/window) margin))
              (- pointer-y gap height)
              (+ pointer-y gap))]
    (set! (.. tooltip -style -transform) (str "translate(" (max margin left) "px," (max margin top) "px)"))))

(defn- fill-ring! [^js dive [pointer-x pointer-y] dive-pressure]
  (let [middle (/ ring-size 2)
        circumference (* 2 js/Math.PI ring-radius)]
    (set! (.. dive -style -transform) (str "translate(" (- pointer-x middle) "px," (- pointer-y middle) "px)"))
    (.setAttribute (dom/find-in dive ".dive-fill") "stroke-dasharray"
                   (str (* dive-pressure circumference) " " circumference))))

(defn update-overlays!
  "Shows, moves, or hides the tooltip and dive ring each frame, rebuilding
  the tooltip only when its subject changes."
  [{tooltip :root tooltip-shown :shown} {dive :root} {:keys [pointer hover dive-pressure editing? menu-open?]}]
  (let [least-shown-pressure 0.02
        diving? (> dive-pressure least-shown-pressure)
        described (when-not (or diving? menu-open? (and editing? (empty? (:problems hover)))) hover)]
    (when (not= described (:described @tooltip-shown))
      (dom/replace-children! tooltip (some-> described tooltip-content))
      (dom/toggle-class! tooltip "is-open" (some? described))
      (reset! tooltip-shown {:described described :size [(.-offsetWidth tooltip) (.-offsetHeight tooltip)]}))
    (when described
      (place-tooltip! tooltip pointer (:size @tooltip-shown)))
    (dom/toggle-class! dive "is-open" diving?)
    (when diving?
      (fill-ring! dive pointer dive-pressure))))

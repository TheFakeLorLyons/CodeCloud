(ns codecloud.render.hud.dock
  "The dock on the right side of the view, where the side panes show one
  above another. Each pane minimizes to its header, the panes share the
  dock's height by the weights the viewer sets by dragging between them,
  and dragging the dock's left edge sets its width."
  (:require [codecloud.render.dom :as dom]
            [codecloud.render.icons :as icons]))

(def pane-ids
  "The panes the dock holds, top to bottom."
  [:inspector :views :arrange :coworkers :assistant])

(def ^:private default-weight
  "A pane's share of the dock's height before the viewer sets one,
  measured in pixels as the shares dragged into place are."
  300)

(defn minimize-button
  "The button minimizing side pane `pane` to its header, or expanding it."
  [dispatch pane]
  [:button.icon-button.pane-minimize {:type "button" :title "Minimize or expand" :aria-label "Minimize or expand"
                                      :on-click #(dispatch [:toggle-pane-minimized pane])}
   icons/chevron-up])

(defn- follow-drag!
  "Calls `move!` with how far the pointer pressed in `event` has moved,
  across and down, then `done!` once it rises."
  [^js event move! done!]
  (let [start-x (.-clientX event)
        start-y (.-clientY event)
        on-move (fn [^js move-event] (move! (- (.-clientX move-event) start-x) (- (.-clientY move-event) start-y)))
        stop! (fn stop! [_]
                (.removeEventListener js/window "pointermove" on-move)
                (.removeEventListener js/window "pointerup" stop!)
                (done!))]
    (.preventDefault event)
    (.addEventListener js/window "pointermove" on-move)
    (.addEventListener js/window "pointerup" stop!)))

(defn- expanded-slots
  "The slots showing a pane at more than its header, top to bottom, as
  [pane-id slot]."
  [slots]
  (filter (fn [[_pane ^js slot]]
            (and (.contains (.-classList slot) "is-shown") (not (.contains (.-classList slot) "is-minimized"))))
          (map (juxt identity slots) pane-ids)))

(defn- height-of [^js element]
  (.-height (.getBoundingClientRect element)))

(defn- start-width-drag!
  "Follows the dock's left edge as the pointer drags it, then keeps the
  width."
  [{:keys [^js root dispatch dragging?]} event]
  (let [start-width (.-width (.getBoundingClientRect root))
        narrowest 260
        width-at (fn [dx] (max narrowest (min (- js/window.innerWidth 32) (- start-width dx))))
        last-width (atom start-width)]
    (reset! dragging? true)
    (follow-drag! event
                  (fn [dx _dy]
                    (reset! last-width (width-at dx))
                    (.setProperty (.-style root) "--dock-width" (str @last-width "px")))
                  (fn []
                    (reset! dragging? false)
                    (dispatch [:resize-dock (js/Math.round @last-width)])))))

(defn- start-height-drag!
  "Moves the border above pane `pane` as the pointer drags it, sharing the
  height of the panes on either side, then keeps every expanded pane's
  share."
  [{:keys [slots dispatch dragging?]} pane event]
  (let [shown (vec (expanded-slots slots))
        index (first (keep-indexed (fn [position [shown-pane]] (when (= pane shown-pane) position)) shown))]
    (when (and index (pos? index))
      (let [lowest 80
            heights (atom (into {} (map (fn [[shown-pane slot]] [shown-pane (height-of slot)])) shown))
            [above-pane ^js above-slot] (nth shown (dec index))
            ^js below-slot (get slots pane)
            pair-height (+ (get @heights above-pane) (get @heights pane))
            start-above (get @heights above-pane)]
        (reset! dragging? true)
        (doseq [[shown-pane ^js slot] shown]
          (.setProperty (.-style slot) "--pane-weight" (str (get @heights shown-pane))))
        (follow-drag! event
                      (fn [_dx dy]
                        (let [above (max lowest (min (- pair-height lowest) (+ start-above dy)))]
                          (swap! heights assoc above-pane above pane (- pair-height above))
                          (.setProperty (.-style above-slot) "--pane-weight" (str above))
                          (.setProperty (.-style below-slot) "--pane-weight" (str (- pair-height above)))))
                      (fn []
                        (reset! dragging? false)
                        (dispatch [:resize-panes (into {} (map (fn [[shown-pane height]] [shown-pane (js/Math.round height)]))
                                                       @heights)])))))))

(defn create-dock [dispatch]
  (let [^js root (dom/element [:div.hud-dock [:div.dock-resize {:aria-hidden "true"}]])
        slots (into {}
                    (map (fn [pane]
                           (let [^js slot (dom/element [:div.dock-slot {:data-pane (name pane)}
                                                        [:div.pane-resize {:aria-hidden "true"}]])]
                             (.appendChild root slot)
                             [pane slot])))
                    pane-ids)
        dock {:root root :slots slots :dispatch dispatch :dragging? (atom false)}]
    (.addEventListener (dom/find-in root ".dock-resize") "pointerdown" #(start-width-drag! dock %))
    (doseq [[pane ^js slot] slots]
      (.addEventListener (dom/find-in slot ".pane-resize") "pointerdown" #(start-height-drag! dock pane %)))
    dock))

(defn add-pane!
  "Puts pane `pane`'s root element in its slot."
  [{:keys [slots]} pane ^js pane-root]
  (.add (.-classList pane-root) "dock-pane")
  (.appendChild (get slots pane) pane-root))

(defn update-dock!
  "Shows the slots of the panes open now, minimized as the state or the
  pane says, sized by the dock's width and the panes' shares, with a
  border to drag above each expanded pane after the first."
  [{:keys [^js root slots dragging?]} {:keys [dock]}]
  (let [{:keys [width weights minimized]} dock]
    (when-not @dragging?
      (.setProperty (.-style root) "--dock-width" (str width "px")))
    (doseq [[pane ^js slot] slots
            :let [^js pane-root (.-lastElementChild slot)
                  own-minimizing? (= :inspector pane)
                  minimized? (if own-minimizing?
                               (.contains (.-classList pane-root) "is-minimized")
                               (contains? minimized pane))]]
      (when-not own-minimizing?
        (dom/toggle-class! pane-root "is-minimized" minimized?))
      (dom/toggle-class! slot "is-shown" (.contains (.-classList pane-root) "is-open"))
      (dom/toggle-class! slot "is-minimized" minimized?)
      (when-not @dragging?
        (.setProperty (.-style slot) "--pane-weight" (str (get weights pane default-weight)))))
    (let [expanded (map first (expanded-slots slots))
          resizable (set (rest expanded))]
      (doseq [[pane ^js slot] slots]
        (dom/toggle-class! slot "has-resize" (contains? resizable pane)))
      (dom/toggle-class! root "has-expanded" (seq expanded)))))

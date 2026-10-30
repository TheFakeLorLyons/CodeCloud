(ns codecloud.render.labels
  "Names floating over distant pages and folder clouds at a fixed screen
  size. Important labels place first, and overlapping ones wait."
  (:require [codecloud.render.dom :as dom]))

(defn create-labels [^js container]
  (let [root (dom/element [:div.label-layer {:aria-hidden "true"}])]
    (.prepend container root)
    {:root root
     :by-key (atom {})
     :free (atom [])}))

(defn- estimated-size
  "Width and height of a label in CSS pixels, estimated from its length so
  placement never has to measure the page."
  [{:keys [badge prefix leaf kind]}]
  (let [[character-pixels height] (if (= :folder kind) [7.6 21] [6.9 18])
        side-padding 12
        badge-padding 10]
    [(+ side-padding
        (* character-pixels (+ (count badge) (count prefix) (count leaf)))
        (if badge badge-padding 0))
     height]))

(defn- overlaps-any?
  "True when the box [left right top bottom] overlaps one of `boxes`."
  [[left right top bottom] boxes]
  (reduce (fn [_ [other-left other-right other-top other-bottom]]
            (if (and (< left other-right) (< other-left right) (< top other-bottom) (< other-top bottom))
              (reduced true)
              false))
          false
          boxes))

(defn- place
  "Candidates that fit without overlap and pass `clear?`, most important
  first. Last frame's labels get a head start against flicker."
  [candidates clear? shown-keys]
  (let [label-limit 90
        keep-bonus 1.6
        ordered (->> candidates
                     (map (fn [{label-key :key :keys [priority] :as candidate}]
                            [(- (* priority (if (contains? shown-keys label-key) keep-bonus 1))) candidate]))
                     (sort-by first)
                     (map second))]
    (:placed
     (reduce (fn [{:keys [placed boxes] :as progress} {:keys [x y] :as candidate}]
               (if (>= (count placed) label-limit)
                 (reduced progress)
                 (let [[width height] (estimated-size candidate)
                       box [(- x (/ width 2)) (+ x (/ width 2)) (- y height) y]]
                   (if (or (overlaps-any? box boxes) (not (clear? candidate)))
                     progress
                     {:placed (conj placed candidate) :boxes (conj boxes box)}))))
             {:placed [] :boxes []}
             ordered))))

(defn- label-element []
  (dom/element [:span.cloud-label [:span.label-badge ""] [:span.label-prefix ""] [:span.label-leaf ""]]))

(defn- show-content!
  "Writes a label's words and kind into its element."
  [^js element {:keys [kind badge prefix leaf]}]
  (dom/set-text! (dom/find-in element ".label-badge") (or badge ""))
  (dom/set-text! (dom/find-in element ".label-prefix") prefix)
  (dom/set-text! (dom/find-in element ".label-leaf") leaf)
  (.setAttribute element "class" (str "cloud-label is-" (name kind))))

(defn update-labels!
  "Shows the `candidates` that fit, each {:key :kind :badge :prefix :leaf
  :x :y :priority :classes}, :x and :y being its bottom middle."
  [{:keys [^js root by-key free]} {:keys [candidates clear?]}]
  (let [shown @by-key
        placed (place candidates clear? (set (keys shown)))
        placed-keys (set (map :key placed))
        assigned (reduce (fn [assigned {label-key :key :keys [x y classes] :as candidate}]
                           (let [{existing :element shown-content :content} (get shown label-key)
                                 content (select-keys candidate [:kind :badge :prefix :leaf])
                                 ^js element (or existing
                                                 (let [reused (peek @free)]
                                                   (when reused (swap! free pop))
                                                   reused)
                                                 (let [created (label-element)]
                                                   (.appendChild root created)
                                                   created))]
                             (when (not= content shown-content)
                               (show-content! element content))
                             (when-not existing
                               (set! (.. element -style -display) ""))
                             (doseq [[class-name present?] classes]
                               (dom/toggle-class! element class-name present?))
                             (set! (.. element -style -transform)
                                   (str "translate(" (js/Math.round x) "px," (js/Math.round y) "px)"))
                             (assoc assigned label-key {:element element :content content})))
                         {}
                         placed)]
    (doseq [[label-key {:keys [^js element]}] shown
            :when (not (contains? placed-keys label-key))]
      (set! (.. element -style -display) "none")
      (swap! free conj element))
    (reset! by-key assigned)))

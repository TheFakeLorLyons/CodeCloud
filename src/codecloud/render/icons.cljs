(ns codecloud.render.icons
  "Line icons for the overlay, as hiccup on a 24 unit grid.")

(defn- icon [& shapes]
  (into [:svg {:viewBox "0 0 24 24" :width 20 :height 20 :fill "none" :stroke "currentColor"
               :stroke-width 1.8 :stroke-linecap "round" :stroke-linejoin "round" :aria-hidden "true"}]
        shapes))

(def search
  (icon [:circle {:cx 10.5 :cy 10.5 :r 6}]
        [:line {:x1 15 :y1 15 :x2 20 :y2 20}]))

(def sun
  (icon [:circle {:cx 12 :cy 12 :r 4}]
        [:path {:d "M12 2.5v2.2M12 19.3v2.2M2.5 12h2.2M19.3 12h2.2M5.3 5.3l1.6 1.6M17.1 17.1l1.6 1.6M5.3 18.7l1.6-1.6M17.1 6.9l1.6-1.6"}]))

(def twilight
  (icon [:path {:d "M6.5 15a5.5 5.5 0 0 1 11 0"}]
        [:path {:d "M3 15h18M7 19h10M12 4.5v2.2M4.8 7.8l1.5 1.5M19.2 7.8l-1.5 1.5"}]))

(def moon
  (icon [:path {:d "M19.5 14.5A8 8 0 0 1 9.5 4.5a8 8 0 1 0 10 10z"}]))

(def follow-theme
  (icon [:circle {:cx 12 :cy 12 :r 8}]
        [:path {:d "M12 4a8 8 0 0 1 0 16z" :fill "currentColor" :stroke "none"}]))

(def clock
  (icon [:circle {:cx 12 :cy 12 :r 8.5}]
        [:path {:d "M12 7.5V12l3 2"}]))

(def frame-all
  (icon [:path {:d "M4 9V5a1 1 0 0 1 1-1h4M15 4h4a1 1 0 0 1 1 1v4M20 15v4a1 1 0 0 1-1 1h-4M9 20H5a1 1 0 0 1-1-1v-4"}]))

(def legend
  (icon [:circle {:cx 12 :cy 12 :r 8.5}]
        [:path {:d "M12 10.5V16.5M12 7.6v.2"}]))

(def play
  (icon [:path {:d "M8 5.5v13l10.5-6.5z" :fill "currentColor"}]))

(def pause
  (icon [:path {:d "M8.5 5.5v13M15.5 5.5v13"}]))

(def close
  (icon [:path {:d "M6.5 6.5L17.5 17.5M17.5 6.5L6.5 17.5"}]))

(def open-file
  (icon [:path {:d "M14 4h6v6M20 4l-8.5 8.5M18 14v5a1 1 0 0 1-1 1H5a1 1 0 0 1-1-1V7a1 1 0 0 1 1-1h5"}]))

(def step-back
  (icon [:path {:d "M15 6l-6 6 6 6"}]))

(def step-forward
  (icon [:path {:d "M9 6l6 6-6 6"}]))

(def views
  (icon [:path {:d "M12 3.5l8.5 4.25L12 12 3.5 7.75z"}]
        [:path {:d "M3.5 12.25L12 16.5l8.5-4.25"}]
        [:path {:d "M3.5 16.5L12 20.75l8.5-4.25"}]))

(def pages-mode
  (icon [:rect {:x 5 :y 3.5 :width 14 :height 17 :rx 2}]
        [:path {:d "M8.5 8h7M8.5 12h5M10.5 16h5"}]))

(def names-mode
  (icon [:path {:d "M3.5 12.6V4.5a1 1 0 0 1 1-1h8.1l8 8a1 1 0 0 1 0 1.4l-6.7 6.7a1 1 0 0 1-1.4 0z"}]
        [:circle {:cx 8.5 :cy 8.5 :r 1.4}]))

(def eye
  (icon [:path {:d "M2.5 12s3.5-6.5 9.5-6.5 9.5 6.5 9.5 6.5-3.5 6.5-9.5 6.5S2.5 12 2.5 12z"}]
        [:circle {:cx 12 :cy 12 :r 2.8}]))

(def arrange
  (icon [:path {:d "M4 7h8.6M17.4 7H20M4 17h2.3M10.7 17H20"}]
        [:circle {:cx 15 :cy 7 :r 2.2}]
        [:circle {:cx 8.5 :cy 17 :r 2.2}]))

(def put-back
  (icon [:path {:d "M9 14l-4.5-4.5L9 5"}]
        [:path {:d "M4.5 9.5H14a5 5 0 0 1 0 10h-3"}]))

(def disclosure
  (icon [:path {:d "M9.5 7l5 5-5 5"}]))

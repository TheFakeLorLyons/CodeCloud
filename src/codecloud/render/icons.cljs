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
  (icon [:path {:d "M6.5 15A5.5 5.5 0 0 1 17.5 15"}]
        [:path {:d "M3 15h18M7 19h10M12 4.5v2.2M4.8 7.8l1.5 1.5M19.2 7.8l-1.5 1.5"}]))

(def moon
  (icon [:path {:d "M19.5 14.5A8 8 0 0 1 9.5 4.5a8 8 0 1 0 10 10z"}]))

(def follow-theme
  (icon [:circle {:cx 12 :cy 12 :r 8}]
        [:path {:d "M12 4a8 8 0 0 1 0 16z" :fill "currentColor" :stroke "none"}]))

(def clock
  (icon [:circle {:cx 12 :cy 12 :r 8.5}]
        [:path {:d "M12 7.5V12l3 2"}]))

(def simple
  (icon [:rect {:x 4 :y 5 :width 16 :height 14 :rx 2.5}]
        [:path {:d "M4 13.5h16"}]))

(def backrooms
  (icon [:path {:d "M3 4.5h18M3 19.5h18M3 4.5l5.5 5h7l5.5-5M3 19.5l5.5-5h7l5.5 5M8.5 9.5v5M15.5 9.5v5M10.5 7h3"}]))

(def library
  (icon [:path {:d "M4 5h4v14H4zM9.5 7.5H13V19H9.5zM14.6 8.4l3.4-.9 3 10.9-3.4.9zM2.5 19.5h19"}]))

(def space
  (icon [:path {:d "M12 3.5l7.5 4.2v8.6L12 20.5l-7.5-4.2V7.7z"}]
        [:path {:d "M4.5 7.7L12 12l7.5-4.3M12 12v8.5"}]))

(def check
  (icon [:path {:d "M5 12.5l4.5 4.5L19 7.5"}]))

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

(def new-file
  (icon [:path {:d "M13.5 3.5H7a2 2 0 0 0-2 2v13a2 2 0 0 0 2 2h10a2 2 0 0 0 2-2V9z"}]
        [:path {:d "M13.5 3.5V9H19"}]
        [:path {:d "M12 12.5v5M9.5 15h5"}]))

(def views
  (icon [:path {:d "M12 3.5l8.5 4.25L12 12 3.5 7.75z"}]
        [:path {:d "M3.5 12.25L12 16.5l8.5-4.25"}]
        [:path {:d "M3.5 16.5L12 20.75l8.5-4.25"}]))

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

(def chevron-up
  (icon [:path {:d "M6 14.5l6-6 6 6"}]))

(def disclosure
  (icon [:path {:d "M9.5 7l5 5-5 5"}]))

(def agents
  (icon [:circle {:cx 9 :cy 8.5 :r 3.2}]
        [:path {:d "M3 19.5a6 6 0 0 1 12 0"}]
        [:path {:d "M15.5 5.5a3.2 3.2 0 0 1 0 6M17 14.3a5.5 5.5 0 0 1 3.5 5.2"}]))

(def plus
  (icon [:path {:d "M12 5v14M5 12h14"}]))

(def pin
  (icon [:path {:d "M12 21s-6-5.6-6-10.5a6 6 0 0 1 12 0C18 15.4 12 21 12 21z"}]
        [:circle {:cx 12 :cy 10.5 :r 2.2}]))

(def follow
  (icon [:circle {:cx 12 :cy 12 :r 3}]
        [:path {:d "M12 3.5v3M12 17.5v3M3.5 12h3M17.5 12h3"}]))

(def load-requires
  (icon [:path {:d "M4 7.5h9M4 12h6M4 16.5h4"}]
        [:path {:d "M17 6.5v10.5M13.8 13.8L17 17l3.2-3.2"}]))

(def repl
  (icon [:rect {:x 3 :y 4.5 :width 18 :height 15 :rx 2.5}]
        [:path {:d "M7 9.5l3 2.5-3 2.5M12.5 15h4.5"}]))

(def disconnect
  (icon [:path {:d "M9 3.5v3.5M15 3.5v3.5M7 7h10v3.5a5 5 0 0 1-10 0zM12 15.5v5"}]
        [:path {:d "M4 4l16 16"}]))

(def interrupt
  (icon [:circle {:cx 12 :cy 12 :r 8.5}]
        [:rect {:x 9 :y 9 :width 6 :height 6 :rx 1 :fill "currentColor" :stroke "none"}]))

(def clear-log
  (icon [:path {:d "M4 6.5h16m-16 5h9m-9 5h6"}]
        [:path {:d "M15.5 14.5l4 4M19.5 14.5l-4 4"}]))

(def assistant
  (icon [:path {:d "M5 4.5h14a1.5 1.5 0 0 1 1.5 1.5v9a1.5 1.5 0 0 1-1.5 1.5h-8.5L6 20v-3.5H5A1.5 1.5 0 0 1 3.5 15V6A1.5 1.5 0 0 1 5 4.5z"}]
        [:path {:d "M12 7.3l1 2.4 2.4 1-2.4 1-1 2.4-1-2.4-2.4-1 2.4-1z" :fill "currentColor" :stroke-width 1}]))

(def new-conversation
  (icon [:path {:d "M10.5 5H6a1.5 1.5 0 0 0-1.5 1.5v12A1.5 1.5 0 0 0 6 20h12a1.5 1.5 0 0 0 1.5-1.5V14"}]
        [:path {:d "M17.5 3.8l2.7 2.7-7.4 7.4-3.4.7.7-3.4z"}]))

(def send
  (icon [:path {:d "M12 19V5.5M6.5 10.5L12 5l5.5 5.5"}]))

(def web
  (icon [:circle {:cx 12 :cy 12 :r 8.5}]
        [:path {:d "M3.5 12h17M12 3.5c2.4 2.4 3.6 5.2 3.6 8.5s-1.2 6.1-3.6 8.5c-2.4-2.4-3.6-5.2-3.6-8.5s1.2-6.1 3.6-8.5z"}]))

(def document
  (icon [:path {:d "M7 3.5h7l4 4v13H7z"}]
        [:path {:d "M14 3.5v4h4M9.5 12h6M9.5 15.5h6"}]))

(def image
  (icon [:rect {:x 3.5 :y 5 :width 17 :height 14 :rx 2}]
        [:circle {:cx 9 :cy 10 :r 1.6}]
        [:path {:d "M4 17.5l5-4.5 3.5 3 3-2.5 4.5 4"}]))

(def reload
  (icon [:path {:d "M19 12a7 7 0 1 1-2.05-4.95"}]
        [:path {:d "M19.5 4.5v4h-4"}]))

(def grid
  (icon [:rect {:x 4 :y 4 :width 7 :height 7 :rx 1.2}]
        [:rect {:x 13 :y 4 :width 7 :height 7 :rx 1.2}]
        [:rect {:x 4 :y 13 :width 7 :height 7 :rx 1.2}]
        [:rect {:x 13 :y 13 :width 7 :height 7 :rx 1.2}]))

(def windows
  (icon [:rect {:x 3.5 :y 7.5 :width 13 :height 12 :rx 1.6}]
        [:path {:d "M3.5 10.5h13M7.5 4.5h13v12"}]))

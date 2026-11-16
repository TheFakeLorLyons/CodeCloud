(ns codecloud.render.pdf-view
  "PDF pages drawn with pdf.js into a column of canvases, each drawn once
  it scrolls near the view, and the column built again when the file
  changes. pdf.js loads the first time a window shows a PDF, from the
  copies of its module and worker the extension ships, which the page's
  root element names."
  (:require [codecloud.render.dom :as dom]))

(defonce ^:private library (atom nil))

(defn- run-module!
  "Resolves once the module script at `address` has run. A script the
  bundle adds may load where the page's policy lets the bundle load."
  [address]
  (js/Promise. (fn [resolve reject]
                 (let [^js script (js/document.createElement "script")]
                   (set! (.-type script) "module")
                   (set! (.-src script) address)
                   (.addEventListener script "load" resolve)
                   (.addEventListener script "error" #(reject (js/Error. "pdf.js could not be loaded")))
                   (.appendChild (.-head js/document) script)))))

(defn- load-library!
  "Resolves to pdf.js, which sets itself on the global object as it runs,
  with its worker started. A webview starts workers only from its own
  origin, so the worker's script arrives as text and runs from a blob."
  []
  (or @library
      (reset! library
              (let [^js root (js/document.getElementById "codecloud")
                    worker-address (.getAttribute root "data-pdf-worker")]
                (-> (run-module! (.getAttribute root "data-pdf-module"))
                    (.then (fn [_] (js/fetch worker-address)))
                    (.then (fn [^js response] (.text response)))
                    (.then (fn [source]
                             (let [^js pdfjs (.-pdfjsLib js/globalThis)
                                   blob-address (js/URL.createObjectURL (js/Blob. #js [source] #js {:type "text/javascript"}))]
                               (set! (.. pdfjs -GlobalWorkerOptions -workerPort) (js/Worker. blob-address #js {:type "module"}))
                               pdfjs))))))))

(defn- draw-page!
  "Draws page `number` of `document` into `slot` at the slot's width,
  sharp at the screen's density and a little beyond, for a view a little
  closer than one to one."
  [^js document number ^js slot]
  (let [sharpness (min 3 (* 1.5 (or js/window.devicePixelRatio 1)))]
    (-> (.getPage document number)
        (.then (fn [^js pdf-page]
                 (let [unscaled (.getViewport pdf-page #js {:scale 1})
                       scale (/ (* sharpness (max 1 (.-clientWidth slot))) (.-width unscaled))
                       viewport (.getViewport pdf-page #js {:scale scale})
                       ^js canvas (dom/element [:canvas.window-pdf-page])]
                   (set! (.-width canvas) (js/Math.floor (.-width viewport)))
                   (set! (.-height canvas) (js/Math.floor (.-height viewport)))
                   (dom/replace-children! slot canvas)
                   (.-promise (.render pdf-page #js {:canvas canvas :viewport viewport})))))
        (.catch (fn [_error] (set! (.-drawn slot) false))))))

(defn- draw-near-view!
  "Draws the pages whose slots come within a screen of the visible part of
  `column`, once each."
  [^js column ^js document]
  (let [top (.-scrollTop column)
        bottom (+ top (.-clientHeight column))
        reach (.-clientHeight column)]
    (doseq [[index ^js slot] (map-indexed vector (array-seq (.querySelectorAll column ".window-pdf-slot")))]
      (when (and (not (.-drawn slot))
                 (< (- (.-offsetTop slot) reach) bottom)
                 (> (+ (.-offsetTop slot) (.-offsetHeight slot) reach) top))
        (set! (.-drawn slot) true)
        (draw-page! document (inc index) slot)))))

(defn- lay-out!
  "Fills `column` with a slot per page, each as tall as its page, and draws
  those near the view."
  [^js column ^js document]
  (-> (js/Promise.all (into-array (map #(.getPage document %) (range 1 (inc (.-numPages document))))))
      (.then (fn [pdf-pages]
               (dom/replace-children!
                column
                (for [^js pdf-page pdf-pages
                      :let [^js viewport (.getViewport pdf-page #js {:scale 1})]]
                  [:div.window-pdf-slot {:style {:aspect-ratio (str (.-width viewport) " / " (.-height viewport))}}]))
               (set! (.-scrollTop column) (or (.-keptScroll column) 0))
               (draw-near-view! column document)))))

(defn show!
  "Shows the PDF at `address` in `column`, a scrolling element, keeping the
  scroll when the file is shown again after a change."
  [^js column address]
  (set! (.-keptScroll column) (.-scrollTop column))
  (when-not (.-scrollFollowed column)
    (set! (.-scrollFollowed column) true)
    (.addEventListener column "scroll" (fn [] (some->> (.-pdfDocument column) (draw-near-view! column)))))
  (-> (load-library!)
      (.then (fn [^js pdfjs]
               (-> (js/fetch address #js {:cache "no-store"})
                   (.then (fn [^js response]
                            (if (.-ok response)
                              (.arrayBuffer response)
                              (throw (js/Error. (str "The PDF could not be read (" (.-status response) ")"))))))
                   (.then (fn [contents]
                            (.-promise (.getDocument pdfjs #js {:data (js/Uint8Array. contents) :isEvalSupported false})))))))
      (.then (fn [^js document]
               (some-> ^js (.-pdfDocument column) .destroy)
               (set! (.-pdfDocument column) document)
               (lay-out! column document)))
      (.catch (fn [error]
                (dom/replace-children! column [:p.window-message (str "This PDF could not be shown: "
                                                                      (or (.-message error) error))])))))

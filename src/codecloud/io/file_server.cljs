(ns codecloud.io.file-server
  "A read-only web server for one folder, so a window can load an HTML
  file with the scripts, styles, and images beside it. It listens on the
  loopback address only, and answers only under a random secret path, so
  other pages in the browser cannot guess their way in."
  (:require ["crypto" :as crypto]
            ["fs" :as fs]
            ["http" :as http]
            ["path" :as path]
            [clojure.string :as string]))

(def ^:private content-types
  {"html" "text/html; charset=utf-8" "htm" "text/html; charset=utf-8"
   "css" "text/css; charset=utf-8" "js" "text/javascript; charset=utf-8" "mjs" "text/javascript; charset=utf-8"
   "json" "application/json; charset=utf-8" "map" "application/json; charset=utf-8"
   "svg" "image/svg+xml" "png" "image/png" "jpg" "image/jpeg" "jpeg" "image/jpeg" "gif" "image/gif"
   "webp" "image/webp" "avif" "image/avif" "ico" "image/x-icon" "bmp" "image/bmp"
   "woff" "font/woff" "woff2" "font/woff2" "ttf" "font/ttf" "otf" "font/otf"
   "wasm" "application/wasm" "pdf" "application/pdf" "txt" "text/plain; charset=utf-8"
   "md" "text/markdown; charset=utf-8" "csv" "text/csv; charset=utf-8" "xml" "application/xml"
   "mp4" "video/mp4" "webm" "video/webm" "mp3" "audio/mpeg" "wav" "audio/wav"})

(defn content-type [file-path]
  (get content-types (string/lower-case (string/replace (path/extname file-path) #"^\." "")) "application/octet-stream"))

(defn requested-file
  "The file under `root` that request path `request-path` names, or nil
  when the path does not start with `secret` or leaves the folder. A
  folder stands for its index.html."
  [root secret request-path]
  (let [prefix (str "/" secret "/")
        without-query (first (string/split (or request-path "") #"[?#]"))]
    (when (string/starts-with? without-query prefix)
      (let [relative (try (js/decodeURIComponent (subs without-query (count prefix))) (catch :default _error nil))
            resolved (when relative (path/resolve root relative))
            inside? (when resolved
                      (or (= resolved (path/resolve root))
                          (string/starts-with? resolved (str (path/resolve root) path/sep))))]
        (when inside?
          (if (or (string/blank? relative) (string/ends-with? relative "/"))
            (path/join resolved "index.html")
            resolved))))))

(defn- answer! [^js response status headers body]
  (.writeHead response status (clj->js headers))
  (.end response body))

(defn- serve! [root secret ^js request ^js response]
  (let [method (.-method request)
        file-path (requested-file root secret (.-url request))]
    (cond
      (not (contains? #{"GET" "HEAD"} method)) (answer! response 405 {"Allow" "GET, HEAD"} "")
      (nil? file-path) (answer! response 404 {"Content-Type" "text/plain"} "Not found")
      :else (fs/stat file-path
                     (fn [error ^js stats]
                       (let [file-path (if (and (not error) (.isDirectory stats)) (path/join file-path "index.html") file-path)]
                         (fs/stat file-path
                                  (fn [error ^js stats]
                                    (if (or error (not (.isFile stats)))
                                      (answer! response 404 {"Content-Type" "text/plain"} "Not found")
                                      (do (.writeHead response 200 #js {"Content-Type" (content-type file-path)
                                                                        "Content-Length" (.-size stats)
                                                                        "Cache-Control" "no-store"})
                                          (if (= "HEAD" method)
                                            (.end response)
                                            (.pipe (fs/createReadStream file-path) response))))))))))))

(defn start!
  "Starts serving `root`, resolving to {:url :close!}, :url ending in a
  slash, under which each file's path relative to `root` names it."
  [root]
  (js/Promise.
   (fn [resolve reject]
     (let [secret-bytes 16
           secret (.toString (.randomBytes crypto secret-bytes) "hex")
           ^js server (.createServer http (fn [request response] (serve! root secret request response)))]
       (.once server "error" reject)
       (.listen server 0 "127.0.0.1"
                (fn []
                  (resolve {:url (str "http://127.0.0.1:" (.-port (.address server)) "/" secret "/")
                            :close! #(.close server)})))))))

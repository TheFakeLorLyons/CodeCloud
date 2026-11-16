(ns codecloud.io.file-server-test
  (:require ["fs" :as fs]
            ["os" :as os]
            ["path" :as path]
            [cljs.test :refer [async deftest is]]
            [codecloud.io.file-server :as file-server]))

(deftest requests-reach-only-files-under-the-secret
  (let [root (path/resolve "/srv/site")]
    (is (= (path/join root "docs" "page one.html")
           (file-server/requested-file root "secret" "/secret/docs/page%20one.html?reload=2")))
    (is (= (path/join root "index.html") (file-server/requested-file root "secret" "/secret/")))
    (is (= (path/join root "docs" "index.html") (file-server/requested-file root "secret" "/secret/docs/")))
    (is (nil? (file-server/requested-file root "secret" "/guess/docs/page.html")) "a wrong secret reaches nothing")
    (is (nil? (file-server/requested-file root "secret" "/secret/../outside.txt")))
    (is (nil? (file-server/requested-file root "secret" "/secret/%2e%2e/outside.txt")) "nor does climbing out, however spelled")
    (is (nil? (file-server/requested-file root "secret" "/secret/%E0%A4%A"))))
  (is (= "text/css; charset=utf-8" (file-server/content-type "theme/site.CSS")))
  (is (= "application/octet-stream" (file-server/content-type "archive.bin"))))

(deftest serves-a-folder-on-the-loopback-address
  (async done
    (let [root (fs/mkdtempSync (path/join (os/tmpdir) "codecloud-files-"))]
      (fs/writeFileSync (path/join root "page.html") "<h1>Orchard</h1>")
      (-> (file-server/start! root)
          (.then (fn [{:keys [url close!]}]
                   (is (re-matches #"http://127\.0\.0\.1:\d+/[0-9a-f]{32}/" url))
                   (-> (js/Promise.all #js [(js/fetch (str url "page.html"))
                                            (js/fetch (str url "missing.html"))
                                            (js/fetch (str url "page.html") #js {:method "POST"})
                                            (js/fetch (str (re-find #"http://127\.0\.0\.1:\d+/" url) "page.html"))])
                       (.then (fn [responses]
                                (let [[found missing posted unsecret] (array-seq responses)]
                                  (is (= 200 (.-status found)))
                                  (is (= "text/html; charset=utf-8" (.get (.-headers found) "content-type")))
                                  (is (= "no-store" (.get (.-headers found) "cache-control")))
                                  (is (= 404 (.-status missing)))
                                  (is (= 405 (.-status posted)))
                                  (is (= 404 (.-status unsecret)) "the server answers nothing without its secret")
                                  (.text found))))
                       (.then (fn [text] (is (= "<h1>Orchard</h1>" text))))
                       (.finally close!))))
          (.catch (fn [error] (is (nil? error))))
          (.finally (fn []
                      (fs/rmSync root #js {:recursive true :force true})
                      (done)))))))

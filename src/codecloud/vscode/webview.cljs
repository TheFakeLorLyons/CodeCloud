(ns codecloud.vscode.webview
  "The page a cloud's webview loads. Its content security policy admits
  the extension's own script, styles, and fonts; files of the cloud's
  folder for windows to read; images over HTTPS for Markdown; web pages
  in windows' frames; and pdf.js, which the extension's own script
  loads, its worker started from a blob."
  (:require ["crypto" :as crypto]
            [codecloud.vscode.api :as api]))

(defn- nonce []
  (let [nonce-bytes 18]
    (.toString (.randomBytes crypto nonce-bytes) "base64")))

(defn page-html [{:keys [csp-source script-uri style-uri pdf-module-uri pdf-worker-uri nonce]}]
  (str "<!DOCTYPE html>\n"
       "<html lang=\"en\">\n"
       "<head>\n"
       "<meta charset=\"UTF-8\">\n"
       "<meta http-equiv=\"Content-Security-Policy\" content=\"default-src 'none'; "
       "img-src " csp-source " data: blob: https:; style-src " csp-source "; font-src " csp-source "; "
       "connect-src " csp-source "; worker-src blob:; frame-src http: https:; "
       "script-src 'nonce-" nonce "' 'strict-dynamic';\">\n"
       "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n"
       "<link rel=\"stylesheet\" href=\"" style-uri "\">\n"
       "<title>CodeCloud</title>\n"
       "</head>\n"
       "<body>\n"
       "<div id=\"codecloud\" data-pdf-module=\"" pdf-module-uri "\" data-pdf-worker=\"" pdf-worker-uri "\"></div>\n"
       "<script nonce=\"" nonce "\" src=\"" script-uri "\"></script>\n"
       "</body>\n"
       "</html>\n"))

(defn prepare!
  "Lets the panel run the cloud view from the extension's dist and media
  folders, and read files from `content-paths` for its windows, and loads
  the page."
  [panel extension-uri content-paths]
  (let [dist (api/child-uri extension-uri "dist")
        media (api/child-uri extension-uri "media")]
    (api/configure-panel! panel (into [dist media] (map api/file-uri) content-paths))
    (api/set-panel-html! panel (page-html {:csp-source (api/csp-source panel)
                                           :script-uri (api/webview-uri panel (api/child-uri dist "webview.js"))
                                           :style-uri (api/webview-uri panel (api/child-uri media "webview.css"))
                                           :pdf-module-uri (api/webview-uri panel (api/child-uri dist "pdfjs.js"))
                                           :pdf-worker-uri (api/webview-uri panel (api/child-uri dist "pdf.worker.mjs"))
                                           :nonce (nonce)}))))

(ns codecloud.vscode.webview
  "The page a cloud's webview loads. Its content security policy admits
  only the extension's own script, styles, and fonts."
  (:require ["crypto" :as crypto]
            [codecloud.vscode.api :as api]))

(defn- nonce []
  (let [nonce-bytes 18]
    (.toString (.randomBytes crypto nonce-bytes) "base64")))

(defn page-html
  "HTML for the cloud view."
  [{:keys [csp-source script-uri style-uri nonce]}]
  (str "<!DOCTYPE html>\n"
       "<html lang=\"en\">\n"
       "<head>\n"
       "<meta charset=\"UTF-8\">\n"
       "<meta http-equiv=\"Content-Security-Policy\" content=\"default-src 'none'; "
       "img-src " csp-source " data: blob:; style-src " csp-source "; font-src " csp-source "; "
       "script-src 'nonce-" nonce "';\">\n"
       "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n"
       "<link rel=\"stylesheet\" href=\"" style-uri "\">\n"
       "<title>CodeCloud</title>\n"
       "</head>\n"
       "<body>\n"
       "<div id=\"codecloud\"></div>\n"
       "<script nonce=\"" nonce "\" src=\"" script-uri "\"></script>\n"
       "</body>\n"
       "</html>\n"))

(defn prepare!
  "Lets the panel run the cloud view from the extension's dist and media
  folders and loads the page."
  [panel extension-uri]
  (let [dist (api/child-uri extension-uri "dist")
        media (api/child-uri extension-uri "media")]
    (api/configure-panel! panel [dist media])
    (api/set-panel-html! panel (page-html {:csp-source (api/csp-source panel)
                                           :script-uri (api/webview-uri panel (api/child-uri dist "webview.js"))
                                           :style-uri (api/webview-uri panel (api/child-uri media "webview.css"))
                                           :nonce (nonce)}))))

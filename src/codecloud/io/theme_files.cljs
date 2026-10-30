(ns codecloud.io.theme-files
  "Editor theme files read as data: a theme's workbench colors and its
  TextMate rules, gathered through the files it includes."
  (:require ["path" :as path]
            [codecloud.io.files :as files]
            [codecloud.io.task :as task]
            [codecloud.languages.json :as json]))

(defn- parsed
  "The data of JSON `text`, read fast when it is strict JSON, and with
  comments and trailing commas otherwise, or nil when it is not JSON."
  [text]
  (try
    (js->clj (js/JSON.parse text))
    (catch :default _error
      (json/read-json text))))

(defn- read-json-file
  "Resolves to the data of a JSON file, or nil when it cannot be read."
  [file-path]
  (task/then (files/read-text file-path) #(some-> % parsed)))

(defn- rules-file-rules
  "The rules a separate token colors file holds, a list of rules itself
  or a map holding them under tokenColors or settings."
  [data]
  (cond
    (sequential? data) data
    (map? data) (some #(when (sequential? (get data %)) (get data %)) ["tokenColors" "settings"])))

(defn read-theme
  "Resolves to {:colors :token-colors} of the theme file at `file-path`,
  an included file's entries first so the including file's win, or nil
  when the file cannot be read as a theme. Token colors kept in a
  TextMate .tmTheme file are not read."
  [file-path]
  (let [most-includes 8]
    (letfn [(read-from [theme-path depth]
              (task/then
               (read-json-file theme-path)
               (fn [theme]
                 (when (map? theme)
                   (let [directory (path/dirname theme-path)
                         include (get theme "include")
                         token-colors (get theme "tokenColors")]
                     (task/then
                      (task/all [(when (and (string? include) (< depth most-includes))
                                   (read-from (path/join directory include) (inc depth)))
                                 (when (string? token-colors)
                                   (task/then (read-json-file (path/join directory token-colors)) rules-file-rules))])
                      (fn [[included file-rules]]
                        {:colors (merge (:colors included) (when (map? (get theme "colors")) (get theme "colors")))
                         :token-colors (into (vec (:token-colors included))
                                             (filter map?)
                                             (if (sequential? token-colors) token-colors file-rules))})))))))]
      (read-from file-path 0))))

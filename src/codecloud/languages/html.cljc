(ns codecloud.languages.html
  "Reads HTML pages: the scripts and style sheets they load, the local
  pages they link to, and the imports of their module scripts."
  (:require [clojure.string :as string]
            [codecloud.languages.javascript :as javascript]
            [codecloud.languages.scanner :as scanner]
            [codecloud.languages.structure :as structure]))

(defn- tag-at
  "{:name :attributes :line} for the tag opening at `index`, or nil for a
  closing tag."
  [tokens index]
  (let [name-token (scanner/token-at tokens (inc index))]
    (when (and (scanner/token-is? tokens index :bracket "<") (= :special (:kind name-token)))
      (loop [position (+ 2 index)
             attributes {}]
        (let [{:keys [kind text] :as token} (scanner/token-at tokens position)]
          (cond
            (or (nil? token) (and (= :bracket kind) (contains? #{">" "/>"} text)))
            {:name (string/lower-case (:text name-token)) :attributes attributes :line (:line name-token)}

            (= :keyword kind)
            (let [has-value? (= "=" (:text (scanner/token-at tokens (inc position))))
                  value-token (when has-value? (scanner/token-at tokens (+ 2 position)))]
              (recur (if has-value? (+ 3 position) (inc position))
                     (assoc attributes (string/lower-case text) (some-> value-token :text scanner/unquoted))))

            :else (recur (inc position) attributes)))))))

(defn- local-address
  "The file part of an address that may name a file of the project, or nil
  for one on the web, an anchor, or a template placeholder."
  [address]
  (when (and address
             (not (string/blank? address))
             (not (re-find #"^(?:[A-Za-z][A-Za-z0-9+.-]*:|//|#|\{\{|\$\{)" address)))
    (let [without-query (first (string/split address #"[?#]"))]
      (when-not (string/blank? without-query) without-query))))

(defn- tag-reference
  "The import a tag makes, or nil: a script, style sheet, preloaded
  module, or linked local page."
  [{tag-name :name :keys [attributes line]}]
  (let [relations (set (string/split (string/lower-case (get attributes "rel" "")) #"\s+"))
        loaded-as (string/lower-case (get attributes "as" ""))
        [address kind] (case tag-name
                         "script" [(get attributes "src") :script]
                         "link" (cond
                                  (contains? relations "stylesheet") [(get attributes "href") :style]
                                  (contains? relations "modulepreload") [(get attributes "href") :script]
                                  (and (contains? relations "preload") (= "script" loaded-as)) [(get attributes "href") :script]
                                  (and (contains? relations "preload") (= "style" loaded-as)) [(get attributes "href") :style]
                                  :else nil)
                         ("a" "area") [(get attributes "href") :link]
                         "iframe" [(get attributes "src") :link]
                         nil)]
    (when-let [file-address (local-address address)]
      {:specifier file-address :kind kind :resolver :html :line line :binds []})))

(defn imports
  "What a page's significant tokens load and link to, with the imports of
  the module scripts written inside it."
  [tokens]
  (into (into []
              (keep (fn [index] (some-> (tag-at tokens index) tag-reference)))
              (range (count tokens)))
        (map #(assoc % :resolver :javascript))
        (javascript/imports tokens)))

(defn read-page
  "Facts of an HTML page: what it loads and links to, and the uses of the
  names its module scripts import."
  [path text options]
  (structure/read-structured :html path text options
                             (comp imports scanner/significant)
                             (constantly structure/no-structure)))

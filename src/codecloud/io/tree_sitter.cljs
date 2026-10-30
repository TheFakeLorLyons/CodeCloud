(ns codecloud.io.tree-sitter
  "Syntax trees of C and C++ from tree-sitter's WebAssembly grammars,
  given back as plain data: the captures of a query, one map per match.
  The runtime and the grammars load once per process, when a cloud first
  holds such files."
  (:require ["path" :as path]
            ["web-tree-sitter" :as tree-sitter]
            [clojure.string :as string]
            [codecloud.io.task :as task]))

(def ^:private grammar-files
  "The WebAssembly grammar of each language, by file name."
  {:c "tree-sitter-c.wasm"
   :cpp "tree-sitter-cpp.wasm"})

(defonce ^:private loading
  ;; Directory to the promise of the parser loaded from it, shared by
  ;; every cloud.
  (atom {}))

(defn- capture-data
  "A capture as {:type :line :column :end-line :end-column}, counted from
  one, with the node's :text when the capture's name ends in name."
  [^js capture]
  (let [^js node (.-node capture)
        ^js start (.-startPosition node)
        ^js end (.-endPosition node)]
    (cond-> {:type (.-type node)
             :line (inc (.-row start))
             :column (inc (.-column start))
             :end-line (inc (.-row end))
             :end-column (inc (.-column end))}
      (string/ends-with? (.-name capture) "name") (assoc :text (.-text node)))))

(defn- match-data
  "A match as a map from each capture's name, as a keyword, to its data."
  [^js match]
  (into {}
        (map (fn [^js capture] [(keyword (.-name capture)) (capture-data capture)]))
        (.-captures match)))

(defn- parse-function
  "A function of a grammar id, a text, and a query's source to the query's
  matches in that text, with one parser per grammar and each query
  compiled once."
  [languages]
  (let [parsers (into {}
                      (map (fn [[grammar-id language]]
                             (let [^js parser (tree-sitter/Parser.)]
                               (.setLanguage parser language)
                               [grammar-id parser])))
                      languages)
        queries (atom {})]
    (fn [grammar-id text query-source]
      (let [query-key [grammar-id query-source]
            ^js query (or (get @queries query-key)
                          (let [compiled (tree-sitter/Query. (get languages grammar-id) query-source)]
                            (swap! queries assoc query-key compiled)
                            compiled))
            ^js tree (.parse ^js (get parsers grammar-id) text)]
        (try
          (mapv match-data (.matches query (.-rootNode tree)))
          (finally (.delete tree)))))))

(defn load-parser
  "Resolves to {:parse function} reading C and C++ with the WebAssembly
  in `directory`, or to {:error message} when it cannot load. The parse
  function takes a grammar id, a text, and a query's source."
  [directory]
  (or (get @loading directory)
      (let [located (fn [file-name] (path/join directory file-name))
            started (-> (.init tree-sitter/Parser #js {:locateFile located})
                        (task/then (fn [_]
                                     (task/all (map (fn [[grammar-id file-name]]
                                                      (task/then (.load tree-sitter/Language (located file-name))
                                                                 (fn [language] [grammar-id language])))
                                                    grammar-files))))
                        (task/then (fn [languages] {:parse (parse-function (into {} languages))}))
                        (task/recover (fn [error] {:error (or (.-message error) (str error))})))]
        (swap! loading assoc directory started)
        started)))

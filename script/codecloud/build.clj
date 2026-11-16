(ns codecloud.build
  "Command-line builds for CodeCloud, run from the project root with
  `clojure -M:build <task>` after `npm install`.

    release   extension host and webview, bundled into dist/
    harness   the browser harness in dev/, for working on the view outside VS Code
    cli       the command-line analyzer, dist/cli.js
    test      compile and run the unit tests on Node
    watch     rebuild the harness whenever a source file changes"
  (:require [cljs.build.api :as cljs-build]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as string]))

(def ^:private shared-options
  {:optimizations :simple
   :static-fns true
   :fn-invoke-direct true
   :infer-externs false
   :pretty-print false})

(def ^:private browser-defines
  {'cljs.core/*global* "window"})

(def ^:private build-options
  {:extension {:main 'codecloud.vscode.extension
               :target :nodejs
               :hashbang false
               :output-dir "out/extension"
               :output-to "out/extension/main.js"}
   :webview {:main 'codecloud.render.vscode-main
             :target :bundle
             :closure-defines browser-defines
             :output-dir "out/webview"
             :output-to "out/webview/main.js"}
   :harness {:main 'codecloud.render.harness-main
             :target :bundle
             :closure-defines browser-defines
             :output-dir "out/harness"
             :output-to "out/harness/main.js"}
   :cli {:main 'codecloud.cli
         :target :nodejs
         :output-dir "out/cli"
         :output-to "out/cli/main.js"}
   :test {:main 'codecloud.test-runner
          :target :nodejs
          :output-dir "out/test"
          :output-to "out/test/main.js"}})

(def ^:private three-module
  "ClojureScript requires three.js with require(), which three's package
  map sends to a CommonJS shim that calls Node's process object; pointing
  the bundler at the ES module build avoids it."
  "--alias:three=./node_modules/three/build/three.module.js")

(def ^:private bundle-arguments
  "esbuild arguments that turn each compiled build into one file, and
  wavescope-mcp, which the extension runs as a child process, into one ES
  module. Only the browser builds pull in an npm package, three.js, and
  the Node builds are bundled too so every build ships as one file. The
  wavescope bundle leaves a record of its inputs, from which its license
  notices name every package inside."
  {:extension ["out/extension/main.js" "--platform=node" "--format=cjs" "--target=node18"
               "--external:vscode" "--outfile=dist/extension.js"]
   :wavescope ["node_modules/wavescope-mcp/dist/index.js" "--platform=node" "--format=esm" "--target=node18"
               "--minify" "--metafile=out/wavescope-inputs.json" "--outfile=dist/wavescope.mjs"]
   :webview ["out/webview/main.js" "--platform=browser" "--format=iife" "--target=chrome120"
             three-module "--minify" "--outfile=dist/webview.js"]
   :harness ["out/harness/main.js" "--platform=browser" "--format=iife" "--target=chrome120"
             three-module "--outfile=dev/harness.js"]
   :cli ["out/cli/main.js" "--platform=node" "--format=cjs" "--target=node18"
         "--outfile=dist/cli.js"]})

(def ^:private parser-files
  "The WebAssembly the extension host and the CLI load to read C and C++,
  copied next to the bundles with the license it ships under."
  {"node_modules/web-tree-sitter/web-tree-sitter.wasm" "dist/web-tree-sitter.wasm"
   "grammars/tree-sitter-c.wasm" "dist/tree-sitter-c.wasm"
   "grammars/tree-sitter-cpp.wasm" "dist/tree-sitter-cpp.wasm"
   "grammars/LICENSE.txt" "dist/tree-sitter-LICENSE.txt"})

(defn- log [& parts]
  (println (string/join " " parts))
  (flush))

(defn- compile-build! [build-key]
  (log "Compiling" (name build-key))
  (cljs-build/build (merge shared-options (get build-options build-key))))

(defn- run-command! [& command]
  (let [{:keys [exit out err]} (apply shell/sh command)]
    (when-not (string/blank? out) (print out))
    (when-not (string/blank? err) (binding [*out* *err*] (print err)))
    (flush)
    exit))

(defn- esbuild-command
  "npm installs esbuild as a .cmd launcher on Windows and as a native
  executable elsewhere."
  []
  (if (string/starts-with? (string/lower-case (System/getProperty "os.name")) "windows")
    ["cmd" "/c" "node_modules\\.bin\\esbuild.cmd"]
    ["node_modules/.bin/esbuild"]))

(defn- bundle! [build-key]
  (log "Bundling" (name build-key))
  (let [exit-code (apply run-command! (concat (esbuild-command)
                                              ["--bundle" "--log-level=warning"]
                                              (get bundle-arguments build-key)))]
    (when-not (zero? exit-code)
      (throw (ex-info (str "esbuild failed for " (name build-key)) {:exit-code exit-code})))))

(defn- copy-parsers! []
  (doseq [[from to] parser-files]
    (io/copy (io/file from) (io/file to))))

;; wavescope's license notices

(def ^:private isc-terms
  "The ISC license's terms, for a bundled package whose package.json
  declares ISC but which ships no license file."
  (str "Permission to use, copy, modify, and/or distribute this software for any purpose with or "
       "without fee is hereby granted, provided that the above copyright notice and this permission "
       "notice appear in all copies.\n\n"
       "THE SOFTWARE IS PROVIDED \"AS IS\" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS "
       "SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE "
       "AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES "
       "WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, "
       "NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE "
       "OF THIS SOFTWARE."))

(defn- bundled-packages
  "Names of the npm packages in esbuild's record of a bundle's inputs,
  sorted."
  [inputs-record]
  (->> (re-seq #"node_modules/((?:@[^/\"]+/)?[^/\"]+)/" inputs-record)
       (map second)
       distinct
       sort))

(defn- package-field
  "A string field of a package.json's text."
  [package-json field]
  (second (re-find (re-pattern (str "\"" field "\"\\s*:\\s*\"([^\"]*)\"")) package-json)))

(defn- package-notice
  "One bundled package's name, version, and license, with its license
  file's text."
  [package-name]
  (let [directory (io/file "node_modules" package-name)
        package-json (slurp (io/file directory "package.json"))
        license (package-field package-json "license")
        license-file (some #(let [^java.io.File candidate (io/file directory %)] (when (.isFile candidate) candidate))
                           ["LICENSE" "LICENSE.md" "LICENSE.txt" "LICENCE" "license" "license.md"])]
    (str package-name " " (package-field package-json "version") ", " license " license\n\n"
         (cond
           license-file (string/trim (slurp license-file))
           (= "ISC" license) (str "The package declares the ISC license in its package.json and ships no "
                                  "license file; these are the ISC license's terms.\n\n" isc-terms)
           :else "The package ships no license file.")
         "\n")))

(defn- write-wavescope-notices!
  "Writes the license notices of every package in dist/wavescope.mjs
  beside it."
  []
  (let [rule-width 72
        separator (str "\n" (apply str (repeat rule-width "-")) "\n\n")]
    (spit "dist/wavescope-LICENSES.txt"
          (str "dist/wavescope.mjs bundles these packages; CodeCloud runs it as a child process.\n\n"
               (string/join separator (map package-notice (bundled-packages (slurp "out/wavescope-inputs.json"))))))))

(defn- bundle-wavescope! []
  (bundle! :wavescope)
  (write-wavescope-notices!))

(defn- build-and-bundle! [build-key]
  (compile-build! build-key)
  (bundle! build-key)
  (when (contains? #{:extension :cli} build-key)
    (copy-parsers!))
  (when (= :extension build-key)
    (bundle-wavescope!)))

(defn- run-tests! []
  (compile-build! :test)
  (copy-parsers!)
  (bundle-wavescope!)
  (log "Running tests")
  (run-command! "node" "out/test/main.js"))

(defn- watch-harness! []
  (log "Watching src for changes; the harness rebuilds after each save")
  (cljs-build/watch "src"
                    (merge shared-options
                           (:harness build-options)
                           {:watch-fn (fn [] (bundle! :harness) (log "Harness ready"))})))

(defn -main [& tasks]
  (.mkdirs (io/file "dist"))
  (let [requested (if (seq tasks) (map keyword tasks) [:release])
        exit-code (reduce (fn [exit-code task]
                            (case task
                              :release (do (build-and-bundle! :extension) (build-and-bundle! :webview) exit-code)
                              :extension (do (build-and-bundle! :extension) exit-code)
                              :webview (do (build-and-bundle! :webview) exit-code)
                              :harness (do (build-and-bundle! :harness) exit-code)
                              :cli (do (build-and-bundle! :cli) exit-code)
                              :test (max exit-code (run-tests!))
                              :watch (do (watch-harness!) exit-code)
                              (do (log "Unknown task" (name task)) 1)))
                          0
                          requested)]
    (shutdown-agents)
    (System/exit exit-code)))

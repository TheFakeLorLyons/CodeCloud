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
  "esbuild arguments that turn each compiled build into one file. Only the
  browser builds pull in an npm package, three.js, and the Node builds are
  bundled too so every build ships as one file."
  {:extension ["out/extension/main.js" "--platform=node" "--format=cjs" "--target=node18"
               "--external:vscode" "--outfile=dist/extension.js"]
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

(defn- build-and-bundle! [build-key]
  (compile-build! build-key)
  (bundle! build-key)
  (when (contains? #{:extension :cli} build-key)
    (copy-parsers!)))

(defn- run-tests! []
  (compile-build! :test)
  (copy-parsers!)
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

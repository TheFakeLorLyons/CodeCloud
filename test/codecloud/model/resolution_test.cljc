(ns codecloud.model.resolution-test
  (:require [clojure.test :refer [deftest is testing]]
            [codecloud.languages.manifests :as manifests]
            [codecloud.model.resolution :as resolution]))

(defn- facts-of
  "Facts for a file set, each file in the language its extension names,
  with the one importing file given its import."
  [paths importing-path import]
  (for [path paths]
    {:path path
     :unit-id path
     :language (condp re-find path
                 #"\.py$" :python
                 #"\.(?:ts|tsx)$" :typescript
                 #"\.(?:js|jsx|mjs)$" :javascript
                 #"\.(?:css|scss|sass)$" :css
                 #"\.html$" :html
                 #"\.java$" :java
                 #"\.clj$" :clojure
                 :shell)
     :requirements []
     :imports (if (= path importing-path) [(assoc import :line 1 :binds [])] [])}))

(defn- resolved-target
  "The target the import of `importing-path` reaches among `paths`, or
  [:external specifier] when it reaches none."
  ([paths importing-path import] (resolved-target paths importing-path import []))
  ([paths importing-path import manifest-facts]
   (let [resolved (resolution/resolve-imports (facts-of paths importing-path import) manifest-facts)
         requirement (first (:requirements (first (filter #(= importing-path (:path %)) resolved))))]
     (if (:external? requirement) [:external (:target requirement)] (:target requirement)))))

(deftest joins-paths
  (is (= "a/c.js" (resolution/joined "a/b" "../c.js")))
  (is (= "a/b/c.js" (resolution/joined "a/b" "./c.js")))
  (is (= "c.js" (resolution/joined "" "c.js")))
  (is (nil? (resolution/joined "a" "../../c.js"))))

(deftest resolves-python-imports
  (let [paths ["src/eeg/__init__.py" "src/eeg/filters.py" "src/eeg/io/serial.py" "tools/run.py" "tools/util.py"]]
    (doseq [[importing specifier names expected]
            [["src/eeg/filters.py" ".io.serial" nil "src/eeg/io/serial.py"]
             ["src/eeg/io/serial.py" "..filters" nil "src/eeg/filters.py"]
             ["src/eeg/io/serial.py" ".." ["filters"] "src/eeg/filters.py"]
             ["tools/run.py" "eeg.filters" nil "src/eeg/filters.py"]
             ["tools/run.py" "eeg" ["bandpass"] "src/eeg/__init__.py"]
             ["tools/run.py" "util" nil "tools/util.py"]
             ["src/eeg/filters.py" "numpy" nil [:external "numpy"]]]]
      (is (= expected (resolved-target paths importing {:specifier specifier :names names :kind :import :resolver :python}))
          (str importing " imports " specifier))))
  (testing "a module inside a package does not import its neighbors by bare name"
    (is (= [:external "filters"]
           (resolved-target ["pkg/__init__.py" "pkg/filters.py" "pkg/main.py"] "pkg/main.py"
                            {:specifier "filters" :kind :import :resolver :python})))))

(deftest resolves-script-imports
  (let [paths ["web/app.js" "web/plot.js" "web/util.ts" "web/lib/index.ts" "web/style.css"
               "packages/charts/src/index.ts" "packages/charts/package.json" "src/shared/math.ts"]
        package (manifests/read-manifest "packages/charts/package.json" "package.json"
                                         "{\"name\": \"@demo/charts\", \"main\": \"src/index.ts\"}")
        config (manifests/read-manifest "tsconfig.json" "tsconfig.json"
                                        "{\"compilerOptions\": {\"baseUrl\": \".\", // aliases\n \"paths\": {\"@shared/*\": [\"src/shared/*\"],}}}")]
    (doseq [[specifier expected] [["./plot.js" "web/plot.js"]
                                  ["./plot" "web/plot.js"]
                                  ["./util.js" "web/util.ts"]
                                  ["./lib" "web/lib/index.ts"]
                                  ["./style.css" "web/style.css"]
                                  ["@demo/charts" "packages/charts/src/index.ts"]
                                  ["@shared/math" "src/shared/math.ts"]
                                  ["three" [:external "three"]]]]
      (is (= expected (resolved-target paths "web/app.js" {:specifier specifier :kind :import :resolver :javascript}
                                       [package config]))
          specifier))))

(deftest resolves-style-sheets-pages-and-commands
  (testing "Sass partials and index files"
    (let [paths ["scss/app.scss" "scss/_mixins.scss" "scss/theme/_index.scss" "css/main.css" "css/base.css"]]
      (is (= "scss/_mixins.scss" (resolved-target paths "scss/app.scss" {:specifier "mixins" :kind :import :resolver :css})))
      (is (= "scss/theme/_index.scss" (resolved-target paths "scss/app.scss" {:specifier "theme" :kind :import :resolver :css})))
      (is (= "css/base.css" (resolved-target paths "css/main.css" {:specifier "base.css" :kind :import :resolver :css})))
      (is (= [:external "sass:math"] (resolved-target paths "scss/app.scss" {:specifier "sass:math" :kind :import :resolver :css})))))
  (testing "a page's root-relative address is found under the folders above it"
    (let [paths ["resources/public/index.html" "resources/public/js/main.js" "resources/public/docs/index.html"]]
      (is (= "resources/public/js/main.js"
             (resolved-target paths "resources/public/index.html" {:specifier "/js/main.js" :kind :script :resolver :html})))
      (is (= "resources/public/docs/index.html"
             (resolved-target paths "resources/public/index.html" {:specifier "docs/" :kind :link :resolver :html})))))
  (testing "commands run scripts by path, by module, and by namespace"
    (let [paths ["bin/run.sh" "bin/env.sh" "scripts/train.py" "src/eeg/stream.py"]]
      (is (= "bin/env.sh" (resolved-target paths "bin/run.sh" {:specifier "$DIR/env.sh" :kind :source :resolver :path})))
      (is (= "bin/env.sh" (resolved-target paths "bin/run.sh" {:specifier "$(dirname $0)/env.sh" :kind :source :resolver :path})))
      (is (= "scripts/train.py" (resolved-target paths "bin/run.sh" {:specifier "scripts/train.py" :kind :invoke :resolver :path})))
      (is (= "src/eeg/stream.py" (resolved-target paths "bin/run.sh" {:specifier "eeg.stream" :kind :invoke :resolver :python-module})))
      (is (= [:external "$HOME/x.sh"] (resolved-target paths "bin/run.sh" {:specifier "$HOME/x.sh" :kind :source :resolver :path}))))))

(deftest resolves-java-classes-from-clojure
  (let [paths ["src/app/core.clj" "src-java/app/Native.java" "test/Fixture.java"]]
    (is (= "src-java/app/Native.java"
           (resolved-target paths "src/app/core.clj" {:specifier "app.Native" :kind :interop :resolver :java-class})))
    (testing "an import that reaches nothing and asks to be dropped leaves no requirement"
      (is (nil? (resolved-target paths "src/app/core.clj"
                                 {:specifier "java.util.Date" :kind :interop :resolver :java-class :drop-unresolved? true}))))))

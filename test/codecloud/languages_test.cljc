(ns codecloud.languages-test
  (:require [clojure.string :as string]
            [clojure.test :refer [deftest is testing]]
            [codecloud.fixtures :as fixtures]
            [codecloud.languages :as languages]
            [codecloud.languages.catalog :as catalog]
            [codecloud.languages.lexer :as lexer]
            [codecloud.languages.lexicons :as lexicons]
            [codecloud.model.config :as config]
            [codecloud.model.facts :as facts]
            [codecloud.model.graph-test :as graph-test]
            [codecloud.model.snapshot :as snapshot]
            [codecloud.model.source :as source]
            [codecloud.presentation.layers :as layers]
            [codecloud.presentation.scene :as scene]
            [codecloud.presentation.wording :as wording]))

(deftest analyzed-languages-are-those-with-adapters
  (is (= (set (keys languages/adapters))
         (set (map :id (filter :analyzed? catalog/languages)))))
  (is (catalog/analyzed? :typescript))
  (is (catalog/analyzed? :cpp))
  (is (not (catalog/analyzed? :java)))
  (is (not (catalog/analyzed? :plain)))
  (is (= :javascript (catalog/language-family :typescript))))

(deftest finds-a-file's-language
  (testing "by extension, in any letter case"
    (is (= :clojure (:id (catalog/language-for "src/app/core.clj" nil))))
    (is (= :clojure (:id (catalog/language-for "tasks/release.BB" nil))))
    (is (= :cpp (:id (catalog/language-for "native/Board.hpp" nil))))
    (is (= :typescript (:id (catalog/language-for "web/util.tsx" nil)))))
  (testing "by file name"
    (is (= :ruby (:id (catalog/language-for "Rakefile" nil))))
    (is (= :shell (:id (catalog/language-for "home/.bashrc" nil)))))
  (testing "by the program a shebang names, without version digits"
    (is (= "bb" (catalog/interpreter-of "#!/usr/bin/env bb")))
    (is (= "python" (catalog/interpreter-of "#!/usr/bin/python3")))
    (is (= "bash" (catalog/interpreter-of "#!/bin/bash -e")))
    (is (= "node" (catalog/interpreter-of "#!/usr/bin/env -S node --no-warnings")))
    (is (nil? (catalog/interpreter-of "(ns not.a.shebang)")))
    (is (= :clojure (:id (catalog/language-for "scripts/deploy" "#!/usr/bin/env bb"))))
    (is (= :python (:id (catalog/language-for "bin/train" "#!/usr/bin/env python3")))))
  (is (nil? (catalog/language-for "notes/todo.txt" "remember the milk")))
  (testing "a file without an extension stays a candidate until its first line is read"
    (is (catalog/extensionless? "bin/deploy"))
    (is (not (catalog/extensionless? "Rakefile")))
    (is (not (catalog/extensionless? ".gitignore")))))

(deftest adapters-keep-the-facts-contract
  (doseq [[path text] graph-test/sources]
    (let [file-facts (languages/analyze-file path text {})]
      (is (empty? (facts/problems file-facts)) path)
      (is (= :clojure (:language file-facts)) path)))
  (testing "a file's unit is its namespace, or its own path when it declares none"
    (is (= "shop.db" (:unit-id (languages/analyze-file "src/shop/db.cljs" "(ns shop.db)" {}))))
    (is (= "build.clj" (:unit-id (languages/analyze-file "build.clj" "(println 1)" {})))))
  (testing "a file of a language without an adapter reads as plain text"
    (let [file-facts (languages/analyze-file "native/filter.go" "func main() {\n  run() // TODO\n}\n"
                                             {:flags {"todo" {:text ["TODO"] :symbols []}}})]
      (is (empty? (facts/problems file-facts)))
      (is (= :go (:language file-facts)))
      (is (= "native/filter.go" (:unit-id file-facts)))
      (is (= 3 (:line-count file-facts)))
      (is (= {"todo" 1} (:flag-counts file-facts)))))
  (testing "the checker names what breaks the contract"
    (is (= [":path must be a string." ":line-count must be a count."]
           (facts/problems {:language :clojure :unit-id "x" :line-count -1 :longest-line 0})))))

(deftest leaves-out-files-that-are-not-source
  (is (= :binary (languages/skip-reason "bin/tool" (str "\u007fELF" "\u0000\u0000"))))
  (is (= :minified (languages/skip-reason "web/vendor.min.js" "var a=1;")))
  (is (= :minified (languages/skip-reason "web/bundle.js" (apply str (repeat 3000 "x")))))
  (is (= :too-large (languages/skip-reason "data/huge.py" (apply str (repeat 1000001 "\n")))))
  (is (nil? (languages/skip-reason "src/ok.py" "import os\n")))
  (testing "an extensionless file reads only when its shebang names a known program"
    (is (nil? (languages/analyze-file "LICENSE" "MIT License\n" {})))
    (is (= :shell (:language (languages/analyze-file "bin/deploy" "#!/usr/bin/env bash\nrsync -a web/ server:\n" {}))))))

(defn- polyglot-facts
  "Facts of every file of the polyglot fixture that CodeCloud reads,
  manifests included, by path."
  []
  (into (sorted-map)
        (keep (fn [[path text]]
                (when (config/source-path? config/default-config path)
                  (when-let [file-facts (languages/analyze-file
                                         path text
                                         {:parse-tree (fixtures/recorded-parse-tree (str fixtures/polyglot-root "/" path))})]
                    [path file-facts]))))
        (fixtures/file-texts fixtures/polyglot-root)))

(defn- polyglot-snapshot []
  (snapshot/build-snapshot (vals (polyglot-facts)) {:id :working-tree :label "Working tree" :time 0}))

(deftest reads-the-polyglot-fixture
  (let [{expected-languages :languages :keys [skipped unread manifests pairs]} (fixtures/expected "polyglot.edn")
        texts (fixtures/file-texts fixtures/polyglot-root)
        read-by-name (into {} (filter (fn [[path]] (config/source-path? config/default-config path))) texts)
        all-facts (polyglot-facts)
        facts-by-path (into (sorted-map) (remove (comp :manifest val)) all-facts)]
    (is (= unread (set (remove (set (keys read-by-name)) (keys texts)))))
    (is (= skipped (set (remove (set (keys all-facts)) (keys read-by-name)))))
    (is (= manifests (set (keep (fn [[path file-facts]] (when (:manifest file-facts) path)) all-facts))))
    (is (= expected-languages (update-vals facts-by-path :language)))
    (doseq [[path file-facts] facts-by-path]
      (is (empty? (facts/problems file-facts)) path))
    (let [built (polyglot-snapshot)]
      (is (= pairs (into {}
                         (keep (fn [{:keys [path unit-id language]}]
                                 (when (and (contains? #{:c :cpp} language) (not= path unit-id)) [path unit-id])))
                         (languages/assign-units (vals facts-by-path)))))
      (is (= (frequencies (vals (apply dissoc expected-languages (keys pairs)))) (get-in built [:totals :languages])))
      (is (= (count facts-by-path) (get-in built [:totals :files])))
      (is (= :module (get-in built [:namespaces "web/index.html" :kind])))
      (is (= :namespace (get-in built [:namespaces "app.core" :kind])))
      (is (= :html (get-in built [:namespaces "web/index.html" :files 0 :language]))))))

(deftest resolves-the-polyglot-fixture's-imports
  (let [{:keys [edges cross-language loops external]} (fixtures/expected "polyglot.edn")
        built (polyglot-snapshot)]
    (is (= edges (set (map (juxt :from :to :kind) (:edges built)))))
    (is (= cross-language (set (keep (fn [{:keys [from to cross-language?]}] (when cross-language? [from to]))
                                     (:edges built)))))
    (testing "loops their languages allow are warnings, not cycles"
      (is (= loops (set (map (comp set :members) (:cycles built)))))
      (is (every? #(= :warning (:severity %)) (:cycles built)))
      (is (zero? (get-in built [:totals :cycles])))
      (is (= 2 (get-in built [:totals :cycle-warnings])))
      (is (every? :cycle-warning?
                  (filter (fn [{:keys [from to]}] (some #(and (contains? % from) (contains? % to)) loops))
                          (:edges built))))
      (is (= :warning (get-in built [:namespaces "src/eeg/board.py" :cycle-severity]))))
    (is (= external (into {}
                          (keep (fn [[id node]] (when (seq (:external-requires node)) [id (:external-requires node)])))
                          (:namespaces built))))))

(defn- tokens-of [path]
  (let [text (source/normalize-text (fixtures/read-text (str fixtures/polyglot-root "/" path)))
        lines (source/split-lines text)
        language (catalog/language-for path (first lines))]
    (lexer/tokenize-lines (lexicons/lexicon-for (:id language)) lines)))

(defn- line-kinds
  "The kinds of the tokens on one-based `line-number`, without spaces."
  [tokenized line-number]
  (into [] (comp (remove #(= :space (first %))) (map first)) (nth tokenized (dec line-number))))

(defn- line-tokens [tokenized line-number]
  (into [] (remove #(= :space (first %))) (nth tokenized (dec line-number))))

(defn- line-starting
  "One-based number of the first line whose text starts with `prefix`."
  [tokenized prefix]
  (inc (first (keep-indexed (fn [index tokens]
                              (when (string/starts-with? (apply str (map second tokens)) prefix) index))
                            tokenized))))

(deftest every-lexicon-keeps-every-character
  (doseq [[path text] (fixtures/file-texts fixtures/polyglot-root)
          :let [language (catalog/language-for path (first (string/split-lines text)))]
          :when language
          :let [lines (source/split-lines (source/normalize-text text))]]
    (is (= lines (mapv #(apply str (map second %)) (lexer/tokenize-lines (lexicons/lexicon-for (:id language)) lines)))
        path)))

(deftest lexes-the-polyglot-fixture
  (testing "comments and strings that cross lines stay whole"
    (let [html (tokens-of "web/index.html")]
      (is (= [:comment] (line-kinds html 13) (line-kinds html 14))))
    (let [colors (tokens-of "web/js/colors.js")]
      (is (= [:comment] (line-kinds colors 3) (line-kinds colors 4)))))
  (testing "HTML reads tags, attributes, and the script and style inside it"
    (let [html (tokens-of "web/index.html")]
      (is (= [[:bracket "<"] [:special "link"] [:keyword "rel"] [:text "="] [:string "\"stylesheet\""]
              [:keyword "href"] [:text "="] [:string "\"css/main.css\""] [:bracket ">"]]
             (line-tokens html 6)))
      (is (= [[:definition "body"] [:bracket "{"] [:keyword "margin"] [:text ":"] [:number "0"] [:text ";"] [:bracket "}"]]
             (line-tokens html 9)))
      (is (some #{[:string "\"ready </p>\""]} (line-tokens html 19)))
      (is (= [[:bracket "</"] [:special "script"] [:bracket ">"]] (line-tokens html 20)))))
  (testing "a heredoc reads as a string up to its delimiter"
    (let [shell (tokens-of "bin/run.sh")]
      (is (= [[:text "cat"] [:reader "<<EOF"]] (line-tokens shell 21)))
      (is (= [:string] (line-kinds shell 22)))
      (is (= [[:reader "EOF"]] (line-tokens shell 23)))
      (is (= [:keyword :string :reader :string] (line-kinds shell 24)))))
  (testing "a regular expression in JavaScript reads as one string"
    (is (some #{[:string "/\"(\\w+)\"/"]} (line-tokens (tokens-of "web/js/app.js") 6))))
  (testing "reserved words, definitions, and decorators in Python"
    (let [stream (tokens-of "src/eeg/stream.py")]
      (is (= [[:special "class"] [:definition "Stream"] [:text ":"]]
             (line-tokens stream (line-starting stream "class Stream"))))
      (is (= [:special :special :definition :bracket :text :bracket :text] (line-kinds stream 18))))))

(deftest finds-definitions-and-complection
  (let [{expected-definitions :definitions :keys [hotspots inbound forms]} (fixtures/expected "polyglot.edn")
        facts-by-path (polyglot-facts)
        built (polyglot-snapshot)]
    (doseq [[path definitions] expected-definitions]
      (is (= definitions (mapv (juxt :name :kind) (get-in facts-by-path [path :definitions]))) path))
    (testing "hotspots match hand counts"
      (is (= hotspots
             (into {}
                   (keep (fn [[path file-facts]]
                           (when (and (not= :clojure (:language file-facts)) (seq (:measured-forms file-facts)))
                             [path (mapv (juxt :name :score :excesses) (:measured-forms file-facts))])))
                   facts-by-path))))
    (testing "uses of imported names count toward the definitions they reach"
      (doseq [[path counts] inbound]
        (is (= counts (into {}
                            (keep (fn [{definition-name :name :keys [inbound-count]}]
                                    (when (pos? inbound-count) [definition-name inbound-count])))
                            (get-in built [:namespaces path :definitions])))
            path)))
    (testing "top-level forms span whole statements, blocks, and rules"
      (doseq [[path spans] forms]
        (is (= spans (mapv (juxt :line :end-line) (get-in facts-by-path [path :top-level-forms]))) path)))
    (testing "the cloud file's flags reach names written with dots"
      (let [flagged (languages/analyze-file "src/eeg/board.py" (get (fixtures/file-texts fixtures/polyglot-root) "src/eeg/board.py")
                                            {:flags {"sleeping" {:text [] :symbols ["time.sleep"]}}})]
        (is (= {"sleeping" 1} (:flag-counts flagged)))
        (is (= [["connect" 0.5 {:flags {"sleeping" 1}}]] (mapv (juxt :name :score :excesses) (:measured-forms flagged))))))))

(deftest names-pages-of-other-languages-by-path
  (let [built (polyglot-snapshot)
        module (get-in built [:namespaces "src/eeg/filters.py"])]
    (is (= {:prefix "src/eeg/" :leaf "filters.py"} (scene/label-parts module)))
    (is (= ["src" "eeg"] (layers/folder-path module)))
    (is (= "Python module" (wording/kind-description module)))
    (is (= ["module" "modules"] (wording/unit-words built)))
    (is (= ["namespace" "namespaces"]
           (wording/unit-words {:namespaces {"app.core" (get-in built [:namespaces "app.core"])}})))
    (is (= "py" (scene/language-tag (scene/several-languages? built) module)))
    (is (= "↻ Cyclic import" (wording/cycle-heading (:severity (first (:cycles built))))))))

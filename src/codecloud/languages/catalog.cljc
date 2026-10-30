(ns codecloud.languages.catalog
  "The languages CodeCloud knows, shared by host and view. A file's
  language comes from its name, then its extension, then its shebang.

  Each language has :id, :label, :tag, :extensions, :file-names,
  :shebangs, :line-comment, :project-markers, the :words the overlay uses,
  and :cycles (:error, :warning, or :allowed). Optional keys:
  :analyzed?, true when an adapter reads its imports and definitions,
  :syntax-tree?, true when that adapter reads through tree-sitter, and
  :family, the language whose hue and module loading it shares."
  (:require [clojure.string :as string]
            [codecloud.model.source :as source]))

(def ^:private import-words
  {:unit ["module" "modules"] :uses "Imports" :used-by "Imported by"})

(def languages
  "Every known language, those CodeCloud analyzes first."
  [{:id :clojure :label "Clojure" :tag "clj"
    :extensions #{"clj" "cljs" "cljc" "cljd" "cljr" "bb"}
    :file-names #{} :shebangs #{"bb" "clojure" "clj"}
    :line-comment ";;"
    :project-markers ["deps.edn" "project.clj" "shadow-cljs.edn" "bb.edn" "build.boot"]
    :words {:unit ["namespace" "namespaces"] :uses "Requires" :used-by "Required by"}
    :cycles :error
    :analyzed? true}
   {:id :python :label "Python" :tag "py"
    :extensions #{"py" "pyi" "pyw"}
    :file-names #{} :shebangs #{"python" "pypy"}
    :line-comment "#"
    :project-markers ["pyproject.toml" "setup.py" "setup.cfg"]
    :words import-words
    :cycles :warning
    :analyzed? true}
   {:id :javascript :label "JavaScript" :tag "js"
    :extensions #{"js" "mjs" "cjs" "jsx"}
    :file-names #{} :shebangs #{"node"}
    :line-comment "//"
    :project-markers ["package.json"]
    :words import-words
    :cycles :warning
    :analyzed? true}
   {:id :typescript :label "TypeScript" :tag "ts"
    :extensions #{"ts" "mts" "cts" "tsx"}
    :file-names #{} :shebangs #{"deno" "ts-node" "tsx"}
    :line-comment "//"
    :project-markers ["tsconfig.json"]
    :words import-words
    :cycles :warning
    :analyzed? true
    :family :javascript}
   {:id :css :label "CSS" :tag "css"
    :extensions #{"css" "scss" "sass" "less"}
    :file-names #{} :shebangs #{}
    :line-comment "//"
    :project-markers []
    :words {:unit ["style sheet" "style sheets"] :uses "Imports" :used-by "Imported by"}
    :cycles :allowed
    :analyzed? true}
   {:id :html :label "HTML" :tag "html"
    :extensions #{"html" "htm" "xhtml"}
    :file-names #{} :shebangs #{}
    :line-comment nil
    :project-markers []
    :words {:unit ["page" "pages"] :uses "Loads" :used-by "Loaded by"}
    :cycles :allowed
    :analyzed? true}
   {:id :shell :label "Shell" :tag "sh"
    :extensions #{"sh" "bash" "zsh" "ksh"}
    :file-names #{".bashrc" ".bash_profile" ".zshrc" ".profile"}
    :shebangs #{"sh" "bash" "zsh" "ksh" "dash" "ash"}
    :line-comment "#"
    :project-markers []
    :words {:unit ["script" "scripts"] :uses "Uses" :used-by "Used by"}
    :cycles :warning
    :analyzed? true}
   {:id :c :label "C" :tag "c"
    :extensions #{"c" "h"}
    :file-names #{} :shebangs #{}
    :line-comment "//"
    :project-markers ["CMakeLists.txt" "Makefile" "meson.build" "compile_commands.json"]
    :words {:unit ["file" "files"] :uses "Includes" :used-by "Included by"}
    :cycles :allowed
    :analyzed? true
    :syntax-tree? true}
   {:id :cpp :label "C++" :tag "c++"
    :extensions #{"cc" "cpp" "cxx" "c++" "hpp" "hh" "hxx" "h++" "inl" "ipp" "tpp"}
    :file-names #{} :shebangs #{}
    :line-comment "//"
    :project-markers ["CMakeLists.txt" "meson.build" "compile_commands.json"]
    :words {:unit ["file" "files"] :uses "Includes" :used-by "Included by"}
    :cycles :allowed
    :analyzed? true
    :syntax-tree? true
    :family :c}
   {:id :java :label "Java" :tag "java"
    :extensions #{"java"} :file-names #{} :shebangs #{}
    :line-comment "//"
    :project-markers ["pom.xml" "build.gradle" "build.gradle.kts"]
    :words {:unit ["file" "files"] :uses "Imports" :used-by "Imported by"}
    :cycles :allowed}
   {:id :kotlin :label "Kotlin" :tag "kt"
    :extensions #{"kt" "kts"} :file-names #{} :shebangs #{"kotlin"}
    :line-comment "//"
    :project-markers ["build.gradle.kts" "build.gradle"]
    :words {:unit ["file" "files"] :uses "Imports" :used-by "Imported by"}
    :cycles :allowed}
   {:id :csharp :label "C#" :tag "c#"
    :extensions #{"cs"} :file-names #{} :shebangs #{}
    :line-comment "//"
    :project-markers []
    :words {:unit ["file" "files"] :uses "Uses" :used-by "Used by"}
    :cycles :allowed}
   {:id :go :label "Go" :tag "go"
    :extensions #{"go"} :file-names #{} :shebangs #{}
    :line-comment "//"
    :project-markers ["go.mod"]
    :words {:unit ["file" "files"] :uses "Imports" :used-by "Imported by"}
    :cycles :error}
   {:id :rust :label "Rust" :tag "rs"
    :extensions #{"rs"} :file-names #{} :shebangs #{}
    :line-comment "//"
    :project-markers ["Cargo.toml"]
    :words {:unit ["module" "modules"] :uses "Uses" :used-by "Used by"}
    :cycles :allowed}
   {:id :ruby :label "Ruby" :tag "rb"
    :extensions #{"rb" "rake" "gemspec"} :file-names #{"Rakefile" "Gemfile"} :shebangs #{"ruby"}
    :line-comment "#"
    :project-markers ["Gemfile"]
    :words {:unit ["file" "files"] :uses "Requires" :used-by "Required by"}
    :cycles :warning}
   {:id :php :label "PHP" :tag "php"
    :extensions #{"php"} :file-names #{} :shebangs #{"php"}
    :line-comment "//"
    :project-markers ["composer.json"]
    :words {:unit ["file" "files"] :uses "Includes" :used-by "Included by"}
    :cycles :allowed}
   {:id :lua :label "Lua" :tag "lua"
    :extensions #{"lua"} :file-names #{} :shebangs #{"lua" "luajit"}
    :line-comment "--"
    :project-markers []
    :words {:unit ["module" "modules"] :uses "Requires" :used-by "Required by"}
    :cycles :warning}
   {:id :r :label "R" :tag "r"
    :extensions #{"r"} :file-names #{} :shebangs #{"Rscript"}
    :line-comment "#"
    :project-markers ["DESCRIPTION"]
    :words {:unit ["script" "scripts"] :uses "Sources" :used-by "Sourced by"}
    :cycles :allowed}
   {:id :julia :label "Julia" :tag "jl"
    :extensions #{"jl"} :file-names #{} :shebangs #{"julia"}
    :line-comment "#"
    :project-markers ["Project.toml"]
    :words {:unit ["file" "files"] :uses "Includes" :used-by "Included by"}
    :cycles :allowed}])

(def by-id (into {} (map (juxt :id identity)) languages))

(def plain
  "What a file of a type no language claims reads as: a page with its
  text, sized and searchable, and no edges."
  {:id :plain :label "Text" :tag "txt"
   :extensions #{} :file-names #{} :shebangs #{}
   :line-comment nil
   :project-markers []
   :words {:unit ["file" "files"] :uses "Uses" :used-by "Used by"}
   :cycles :allowed})

(defn language
  "The catalog entry for `language-id`, with the plain entry for an id
  the catalog does not know."
  [language-id]
  (get by-id language-id plain))

(def extensions
  "Every extension a known language claims, sorted."
  (vec (sort (mapcat :extensions languages))))

(def project-markers
  "Every file name that marks a project's folder, in catalog order."
  (vec (distinct (mapcat :project-markers languages))))

(defn interpreter-of
  "The program a shebang names without directory or version, such as
  \"python\" for #!/usr/bin/python3, or nil."
  [first-line]
  (when-let [[_ program argument] (re-find #"^#!\s*(\S+)(?:\s+(\S+))?" (or first-line ""))]
    (let [program-name (source/file-name program)
          named (if (= "env" program-name)
                  (if (= "-S" argument)
                    (second (re-find #"^#!\s*\S+\s+-S\s+(\S+)" first-line))
                    argument)
                  program-name)]
      (some-> named (string/replace #"[0-9.]+$" "")))))

(defn language-by-name
  "The language of `path` judged by its file name and extension alone, or
  nil."
  [path]
  (let [file-name (source/file-name path)
        extension (source/file-extension path)]
    (or (some #(when (contains? (:file-names %) file-name) %) languages)
        (some #(when (contains? (:extensions %) extension) %) languages))))

(defn language-for
  "The language of the file at `path` whose first line is `first-line`,
  judged by its file name, then its extension, then its shebang, or nil."
  [path first-line]
  (or (language-by-name path)
      (let [interpreter (interpreter-of first-line)]
        (some #(when (contains? (:shebangs %) interpreter) %) languages))))

(defn extensionless?
  "True for a path whose file name has no extension and no known name, as
  scripts run through a shebang often are."
  [path]
  (let [file-name (source/file-name path)]
    (and (nil? (source/file-extension path))
         (not (string/starts-with? file-name "."))
         (nil? (language-by-name path)))))

(defn language-family
  "The language whose hue and module loading `language-id` shares, which
  is itself unless its entry names another."
  [language-id]
  (or (:family (get by-id language-id)) language-id))

(defn analyzed?
  "True for a language an adapter reads for imports and definitions, and
  false for one CodeCloud shows as highlighted text alone."
  [language-id]
  (boolean (:analyzed? (language language-id))))

(defn cycle-severity
  "How loud an import cycle among files of `language-id` is: :error,
  :warning, or :allowed."
  [language-id]
  (:cycles (language language-id)))

(def clojure-platforms
  "The Clojure dialects an edge can load on, each of which refuses a
  cycle. Edges of other languages load on their language family."
  #{:clj :cljs :cljd :cljr})

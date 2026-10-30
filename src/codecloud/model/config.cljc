(ns codecloud.model.config
  "What a cloud file (*.cld) and deps.edn's :codecloud/config may declare,
  how both are checked, and how they combine, the cloud file winning."
  (:require [clojure.string :as string]
            [codecloud.collections :as collections]
            [codecloud.languages.catalog :as catalog]
            [codecloud.languages.manifests :as manifests]
            [codecloud.model.complection :as complection]
            [codecloud.model.importance :as importance]
            [codecloud.model.source :as source]
            [edamame.core :as edamame]))

(def default-config
  "What a cloud reads with nothing configured. Without :extensions or
  :languages it reads every file of a language the catalog knows."
  {:include []
   :ignore []
   :views {}
   :flags {}
   :complection {}
   :color-layers {}
   :weights importance/default-weights})

(def always-ignored
  "Directories that never hold a project's own source."
  [".git/" "node_modules/" "__pycache__/" ".venv/" ".tox/"])

(def cloud-file-name
  "Name of the cloud file CodeCloud creates when a folder has none."
  "codecloud.cld")

(def cloud-file-template
  "Contents of a newly created cloud file."
  (string/join
   "\n"
   [";; CodeCloud draws the folder holding this file as a cloud of pages, one"
    ";; for each namespace, module, or script."
    ";; VS Code shows the cloud when this file opens; the command"
    ";; \"CodeCloud: Edit Cloud Settings\" opens it as text."
    ";;"
    ";; Patterns use .gitignore syntax. :include, when given, limits the cloud"
    ";; to matching paths, and :ignore removes paths on top of what git ignores."
    ";; :views names parts of the cloud that each viewer can choose to see:"
    ";;   :views {:ui [\"src/app/ui/\" \"src/app/shared/\"]"
    ";;           :server {:include [\"src/app/server/\"] :ignore [\"**/legacy/\"]}}"
    ";; :flags names things that make code harder to follow. Strings are found"
    ";; anywhere in a definition's text, comments included, and a qualified"
    ";; symbol matches however the file's aliases spell it. Each find counts"
    ";; toward complection, as with"
    ";;   :flags {:todo [\"TODO\" \"FIXME\"] :blocking [Thread/sleep clojure.core.async/<!!]}"
    ";; :complection sets how much of each measurement a definition may have"
    ";; before it counts as complected. The defaults are"
    ";;   :complection {:lines 25 :branches 6 :nesting 7 :asynchronous 1 :state 2}"
    ";; :weights set how much each measurement counts toward a namespace's"
    ";; importance."
    ";; :color-layers adds layers to the colors in the arrange panel, each"
    ";; shading pages by a measurement from one color to another or coloring"
    ";; the pages its patterns match, as in"
    ";;   :color-layers {:long {:metric :lines :colors [\"green\" \"red\"]}"
    ";;                  :tests {:patterns [\"test/\"] :color \"pink\"}}"
    ";; CodeCloud reads every language it knows. :languages narrows the cloud"
    ";; to some of them, as with :languages [:clojure :python :shell], and"
    ";; :extensions, when present, lists exactly the file types to read, where"
    ";; a type no language claims becomes a page of plain text."
    ";; C and C++ includes search the folders that compile_commands.json,"
    ";; CMakeLists.txt, Makefiles, and meson.build name, and :include-paths"
    ";; adds more, as with :include-paths [\"include\" \"third_party\"]."
    "{:ignore [\"out/\" \"target/\" \".shadow-cljs/\" \".cpcache/\"]"
    " :views {}"
    " :flags {:todo [\"TODO\" \"FIXME\" \"HACK\"]}"
    " :weights {:functions 1.0"
    "           :references 1.0"
    "           :lines 1.0}}"
    ""]))

(defn read-edn
  "Reads one EDN value as {:value v} or {:error message}. Unknown tags
  read as their value; maps and vectors carry position metadata."
  [text]
  (try
    {:value (edamame/parse-string text {:readers (fn [_tag] identity)
                                        :auto-resolve (fn [alias] alias)})}
    (catch #?(:clj Exception :cljs :default) error
      {:error (or (ex-message error) (str error))})))

(defn- normalized-extension [extension]
  (-> (str extension)
      string/trim
      (string/replace #"^\." "")
      string/lower-case))

(defn- normalized-folder
  "A folder as resolution compares it: forward slashes, from the cloud's
  root, with no leading ./ and no trailing slash."
  [folder]
  (-> (string/trim folder)
      (string/replace "\\" "/")
      (string/replace #"^(?:\./)+" "")
      (string/replace #"/+$" "")))

(defn- pattern-list? [value]
  (and (sequential? value) (every? string? value)))

(defn- language-list? [value]
  (and (sequential? value) (every? #(or (keyword? %) (symbol? %) (string? %)) value)))

(defn- language-id-of
  "The catalog id a cloud file names a language by, such as :cpp for
  \"C++\" or c++, or nil for a name the catalog does not know."
  [language-name]
  (let [spelled (string/lower-case (name language-name))]
    (some (fn [{:keys [id label tag]}]
            (when (some #(= spelled %) [(name id) (string/lower-case label) tag]) id))
          catalog/languages)))

(defn- entry-name
  "The name a cloud file map key gives, or nil for a key that is not a
  keyword, symbol, or string."
  [entry-key]
  (cond
    (keyword? entry-key) (name entry-key)
    (symbol? entry-key) (name entry-key)
    (string? entry-key) entry-key
    :else nil))

(defn- view-selection
  "A view value as {:include :ignore}, or nil when it has the wrong shape.
  A bare vector lists include patterns."
  [view-value]
  (cond
    (pattern-list? view-value) {:include (vec view-value) :ignore []}
    (and (map? view-value)
         (every? #(or (nil? %) (pattern-list? %)) [(:include view-value) (:ignore view-value)]))
    {:include (vec (:include view-value)) :ignore (vec (:ignore view-value))}
    :else nil))

(defn- validate-views
  "Named views keyed by their name as a string, with a problem for each
  entry that could not be used."
  [views]
  (reduce-kv (fn [{:keys [views problems]} view-key view-value]
               (let [view-label (entry-name view-key)
                     selection (view-selection view-value)]
                 (if (and view-label (not (string/blank? view-label)) selection)
                   {:views (assoc views view-label selection) :problems problems}
                   {:views views
                    :problems (conj problems
                                    (str "The view " (pr-str view-key) " needs a name and a vector of "
                                         "patterns, or a map with :include and :ignore vectors."))})))
             {:views {} :problems []}
             views))

(defn- flag-patterns
  "A flag's patterns as {:text :symbols}, or nil when the value is not a
  vector of strings and symbols."
  [flag-value]
  (when (and (sequential? flag-value) (every? #(or (string? %) (symbol? %)) flag-value))
    {:text (vec (distinct (remove string/blank? (filter string? flag-value))))
     :symbols (vec (distinct (map str (filter symbol? flag-value))))}))

(defn- validate-flags
  "Flags keyed by their name as a string, with a problem for each entry
  that could not be used."
  [flags]
  (reduce-kv (fn [{:keys [flags problems]} flag-key flag-value]
               (let [flag-label (entry-name flag-key)
                     patterns (flag-patterns flag-value)]
                 (if (and flag-label (not (string/blank? flag-label)) patterns)
                   {:flags (assoc flags flag-label patterns) :problems problems}
                   {:flags flags
                    :problems (conj problems
                                    (str "The flag " (pr-str flag-key) " needs a name and a vector of "
                                         "strings to find in the text or symbols to find in the code."))})))
             {:flags {} :problems []}
             flags))

(defn- valid-weight? [[weight-key weight-value]]
  (and (contains? (set importance/weighted-metrics) weight-key)
       (number? weight-value)
       (not (neg? weight-value))))

(defn- valid-limit? [[measurement amount]]
  (and (contains? complection/default-thresholds measurement)
       (number? amount)
       (not (neg? amount))))

(def color-layer-measurements
  "Measurements a cloud file's heat layer may shade pages by."
  #{:lines :functions :definitions :references :dependents :dependencies :external-dependencies
    :complection :flags :importance})

(def color-names
  "Colors a cloud file's layers may name, besides #rrggbb."
  #{"red" "orange" "yellow" "green" "teal" "blue" "purple" "pink" "brown" "gray"})

(defn- color-text? [value]
  (and (string? value)
       (or (contains? color-names (string/lower-case value))
           (some? (re-matches #"#[0-9a-fA-F]{6}" value)))))

(defn- color-layer
  "A validated cloud file layer, {:metric :colors [low high]} or
  {:patterns :color}, or nil. Its key sits in the cloud namespace, apart
  from the built-in layers."
  [layer-key {:keys [label metric colors patterns color] :as entry}]
  (let [layer-name (entry-name layer-key)
        layer-label (or label layer-name)
        cloud-key (keyword "cloud" layer-name)]
    (when (and (map? entry) (string? layer-label))
      (cond
        (and (contains? color-layer-measurements metric)
             (sequential? colors) (= 2 (count colors)) (every? color-text? colors))
        {:key cloud-key :label layer-label :metric metric :colors (vec colors)}

        (and (pattern-list? patterns) (seq patterns) (color-text? color))
        {:key cloud-key :label layer-label :patterns (vec patterns) :colors [color]}

        :else nil))))

(defn- validate-color-layers
  "Color layers keyed by name, with a problem for each entry that could
  not be used."
  [layers]
  (reduce-kv (fn [{:keys [layers problems]} layer-key entry]
               (if-let [layer (and (entry-name layer-key) (color-layer layer-key entry))]
                 {:layers (assoc layers (:key layer) layer) :problems problems}
                 {:layers layers
                  :problems (conj problems
                                  (str "The color layer " (pr-str layer-key) " needs :metric, one of "
                                       (collections/spoken-list (map str (sort color-layer-measurements)))
                                       ", with :colors [low high], or :patterns with a :color. The named colors are "
                                       (collections/spoken-list (sort color-names)) ", and #rrggbb names any other."))}))
             {:layers {} :problems []}
             layers))

(def ^:private entry-rules
  "How each configuration entry is checked, in reporting order: :valid?,
  the :problem when not, and :clean, giving {:value :problems}."
  [{:key :include
    :valid? pattern-list?
    :problem ":include must be a vector of pattern strings."
    :clean (fn [include] {:value (vec include)})}
   {:key :ignore
    :valid? pattern-list?
    :problem ":ignore must be a vector of pattern strings."
    :clean (fn [ignore] {:value (vec ignore)})}
   {:key :views
    :valid? map?
    :problem ":views must be a map from a view's name to its patterns."
    :clean (fn [views] (let [{checked :views :keys [problems]} (validate-views views)]
                         {:value checked :problems problems}))}
   {:key :flags
    :valid? map?
    :problem ":flags must be a map from a flag's name to the strings and symbols it finds."
    :clean (fn [flags] (let [{checked :flags :keys [problems]} (validate-flags flags)]
                         {:value checked :problems problems}))}
   {:key :complection
    :valid? map?
    :problem ":complection must be a map such as {:lines 40 :branches 8}."
    :clean (fn [limits]
             {:value (into {} (filter valid-limit?) limits)
              :problems (when-not (every? valid-limit? limits)
                          [(str ":complection accepts non-negative numbers for "
                                (collections/spoken-list (map str (keys complection/default-thresholds))) ".")])})}
   {:key :weights
    :valid? map?
    :problem ":weights must be a map such as {:functions 1.0}."
    :clean (fn [weights]
             {:value (into {} (filter valid-weight?) weights)
              :problems (when-not (every? valid-weight? weights)
                          [(str ":weights accepts non-negative numbers for "
                                (collections/spoken-list (map str importance/weighted-metrics)) ".")])})}
   {:key :extensions
    :valid? pattern-list?
    :problem ":extensions must be a vector of strings such as [\"clj\" \"cljs\"]."
    :clean (fn [extensions] {:value (mapv normalized-extension extensions)})}
   {:key :include-paths
    :valid? pattern-list?
    :problem ":include-paths must be a vector of folders such as [\"include\"]."
    :clean (fn [folders] {:value (mapv normalized-folder folders)})}
   {:key :color-layers
    :valid? map?
    :problem ":color-layers must be a map from a layer's name to its :metric and :colors, or :patterns and :color."
    :clean (fn [layers] (let [{checked :layers :keys [problems]} (validate-color-layers layers)]
                          {:value checked :problems problems}))}
   {:key :languages
    :valid? language-list?
    :problem ":languages must be a vector of language names such as [:clojure :python]."
    :clean (fn [language-names]
             (let [unknown (remove language-id-of language-names)]
               {:value (vec (distinct (keep language-id-of language-names)))
                :problems (when (seq unknown)
                            [(str "CodeCloud does not know the language "
                                  (collections/spoken-list (map pr-str unknown))
                                  ". It knows "
                                  (collections/spoken-list (map (comp str :id) catalog/languages)) ".")])}))}])

(defn validate
  "Checks a raw configuration map. Returns {:config cleaned :problems
  [description]}; each problem names a value that was dropped and why."
  [raw-config]
  (if-not (map? raw-config)
    {:config {} :problems ["The configuration is not a map, so CodeCloud used its defaults."]}
    (reduce (fn [{:keys [config problems]} {entry-key :key :keys [valid? problem clean]}]
              (let [value (get raw-config entry-key)]
                (cond
                  (nil? value) {:config config :problems problems}
                  (not (valid? value)) {:config config :problems (conj problems problem)}
                  :else (let [{cleaned :value part-problems :problems} (clean value)]
                          {:config (assoc config entry-key cleaned)
                           :problems (into problems part-problems)}))))
            {:config {} :problems []}
            entry-rules)))

(defn cloud-file-config
  "Validated configuration from the text of a cloud file. A blank file is
  an empty configuration."
  [text]
  (if (string/blank? text)
    {:config {} :problems []}
    (let [{:keys [value error]} (read-edn text)]
      (if error
        {:config {} :problems [(str "The cloud file could not be read: " error)]}
        (validate value)))))

(defn deps-edn-config
  "Validated configuration from the :codecloud/config entry of a deps.edn
  file, or an empty configuration when the entry is absent."
  [text]
  (let [{:keys [value error]} (read-edn text)]
    (cond
      error {:config {} :problems [(str "deps.edn could not be read: " error)]}
      (and (map? value) (contains? value :codecloud/config))
      (update (validate (:codecloud/config value)) :problems
              (fn [problems] (mapv #(str "In deps.edn :codecloud/config, " %) problems)))
      :else {:config {} :problems []})))

(defn combine
  "Merges validated configurations over the defaults, lowest precedence
  first. Ignore lists accumulate and maps merge by key."
  [& configs]
  (let [merged-maps #{:weights :views :flags :complection :color-layers}]
    (reduce (fn [combined config]
              (reduce-kv (fn [inner entry-key value]
                           (cond
                             (= :ignore entry-key) (update inner :ignore #(vec (distinct (concat % value))))
                             (contains? merged-maps entry-key) (update inner entry-key merge value)
                             :else (assoc inner entry-key value)))
                         combined
                         config))
            default-config
            configs)))

(defn color-layer-specs
  "The color layers a configuration declares, in the order of their keys."
  [config]
  (vec (sort-by (comp name :key) (vals (:color-layers config)))))

(defn source-path-predicate
  "Predicate for the paths read by name: :extensions when given, else
  known languages, narrowed by :languages. Extensionless files and
  manifests stay candidates."
  [config]
  (let [listed-extensions (some-> (:extensions config) set)
        wanted-languages (some-> (:languages config) set)
        language-wanted? (fn [language]
                           (or (nil? wanted-languages) (nil? language)
                               (contains? wanted-languages (:id language))))]
    (fn [path]
      (let [language (catalog/language-by-name path)]
        (cond
          (contains? manifests/file-names (source/file-name path)) true
          listed-extensions
          (and (contains? listed-extensions (source/file-extension path))
               (language-wanted? language))
          language (language-wanted? language)
          :else (catalog/extensionless? path))))))

(defn source-path?
  "True when the configuration reads the file at `path`, judged by its
  name."
  [config path]
  ((source-path-predicate config) path))

(defn language-wanted?
  "True when the configuration reads files of `language-id` once their
  language is known."
  [config language-id]
  (or (nil? (:languages config))
      (= :plain language-id)
      (contains? (set (:languages config)) language-id)))

(defn read-extensions
  "The extensions a configuration reads, for history and file watching:
  those it lists, or those of every language it reads."
  [config]
  (or (:extensions config)
      (let [wanted (some-> (:languages config) set)]
        (vec (sort (mapcat :extensions (filter #(or (nil? wanted) (contains? wanted (:id %)))
                                               catalog/languages)))))))

(defn- view-key-text
  "How a view's name is written as a map key, a keyword when the name
  allows one and otherwise a string."
  [view-label]
  (if (re-matches #"[A-Za-z][A-Za-z0-9_\-.]*" view-label)
    (str ":" view-label)
    (pr-str view-label)))

(defn- selection-text [{:keys [include ignore]}]
  (str "{"
       (string/join " " (cond-> []
                          (seq include) (conj (str ":include " (pr-str (vec include))))
                          (seq ignore) (conj (str ":ignore " (pr-str (vec ignore))))))
       "}"))

(defn view-insertion
  "Where a new view goes: one-based {:line :column :text} before the
  closing brace of :views or the file's map, or {:error message}."
  [text view-label selection]
  (let [{:keys [value error]} (read-edn text)
        entry (str (view-key-text view-label) " " (selection-text selection))]
    (cond
      error {:error (str "The cloud file could not be read: " error)}
      (not (map? value)) {:error "The cloud file must hold a map before views can be saved into it."}
      :else
      (let [views (:views value)
            {:keys [col end-row end-col]} (meta (if (map? views) views value))
            indent (apply str (repeat col " "))]
        (cond
          (and (map? views) (empty? views)) {:line end-row :column (dec end-col) :text entry}
          (map? views) {:line end-row :column (dec end-col) :text (str "\n" indent entry)}
          (some? views) {:error "The cloud file's :views is not a map, so the view could not be added."}
          :else {:line end-row :column (dec end-col) :text (str "\n" indent ":views {" entry "}")})))))

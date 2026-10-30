(ns codecloud.model.ns-form
  "Reads `ns` declarations and top-level `require` calls into requirement
  maps, one for each library a namespace asks for."
  (:require [codecloud.model.forms :as forms]))

(defn- libspec?
  "True for a libspec: a symbol, a module string, or a vector whose second
  item is a keyword or absent."
  [form]
  (or (symbol? form)
      (string? form)
      (and (vector? form)
           (or (nil? (second form))
               (keyword? (second form))))))

(defn- option-map
  "Turns the key/value tail of a libspec vector into a map, dropping a
  dangling key that has no value."
  [option-forms]
  (into {}
        (comp (partition-all 2)
              (filter #(= 2 (count %)))
              (filter #(keyword? (first %)))
              (map vec))
        option-forms))

(defn- name-strings [forms]
  (into #{} (comp (filter symbol?) (map name)) forms))

(defn- requirement-from-libspec
  "Requirement for one libspec as {:target :js-module? :alias :alias-only?
  :refers :refer-all? :renames :macros? :directive}."
  [directive prefix libspec]
  (let [[library & option-forms] (if (vector? libspec) libspec [libspec])
        options (option-map option-forms)
        js-module? (string? library)
        target (cond
                 js-module? library
                 prefix (str prefix "." library)
                 :else (str library))
        use-directive? (contains? #{:use :use-macros} directive)
        refer-option (or (:refer options) (:only options))
        refer-all? (or (= :all refer-option)
                       (and use-directive? (nil? (:only options))))
        alias-symbol (or (:as options) (:as-alias options) (:default options))
        renames (into {}
                      (keep (fn [[original-name local-name]]
                              (when (and (symbol? original-name) (symbol? local-name))
                                [(name local-name) (name original-name)])))
                      (:rename options))]
    {:target target
     :js-module? js-module?
     :alias (some-> alias-symbol name)
     :alias-only? (and (some? (:as-alias options)) (nil? (:as options)))
     :refers (into (if (sequential? refer-option) (name-strings refer-option) #{})
                   (name-strings (:refer-macros options)))
     :refer-all? (boolean refer-all?)
     :renames renames
     :macros? (contains? #{:require-macros :use-macros} directive)
     :directive directive}))

(defn- requirements-from-argument
  [directive argument]
  (cond
    (libspec? argument)
    [(requirement-from-libspec directive nil argument)]

    (and (sequential? argument) (symbol? (first argument)))
    (let [prefix (first argument)]
      (into []
            (comp (filter libspec?)
                  (map #(requirement-from-libspec directive prefix %)))
            (rest argument)))

    :else []))

(defn requirements-of-directive
  "Requirement maps for one directive keyword and its arguments, skipping
  load flags such as :reload."
  [directive arguments]
  (if (contains? #{:require :use :require-macros :use-macros} directive)
    (into []
          (comp (remove keyword?)
                (mapcat #(requirements-from-argument directive %)))
          arguments)
    []))

(defn class-imports
  "Fully qualified names of the classes an `:import` clause names, from
  (:import [pkg A B] (pkg C) pkg.D)."
  [clause-arguments]
  (into []
        (mapcat (fn [argument]
                  (cond
                    (symbol? argument) [(str argument)]
                    (and (sequential? argument) (symbol? (first argument)))
                    (map #(str (first argument) "." %) (filter symbol? (rest argument)))
                    :else [])))
        clause-arguments))

(defn- ns-declaration? [form]
  (and (= "ns" (forms/head-name form))
       (symbol? (second form))))

(defn- in-ns-declaration? [form]
  (and (= "in-ns" (forms/head-name form))
       (let [argument (second form)]
         (and (seq? argument)
              (= 'quote (first argument))
              (symbol? (second argument))))))

(defn namespace-declaration
  "The `ns` or `(in-ns 'name)` declaration in `form` as {:name :doc
  :requirements :class-imports :in-ns?}, or nil for any other form."
  [form]
  (cond
    (ns-declaration? form)
    (let [[_ namespace-symbol & clauses] form
          doc (when (string? (first clauses)) (first clauses))
          clauses (if doc (rest clauses) clauses)
          attribute-map (when (map? (first clauses)) (first clauses))
          clauses (if attribute-map (rest clauses) clauses)]
      {:name (str namespace-symbol)
       :doc (or doc (:doc attribute-map) (:doc (meta namespace-symbol)))
       :in-ns? false
       :requirements (into []
                           (comp (filter #(and (sequential? %) (keyword? (first %))))
                                 (mapcat #(requirements-of-directive (first %) (rest %))))
                           clauses)
       :class-imports (into []
                            (comp (filter #(and (sequential? %) (= :import (first %))))
                                  (mapcat #(class-imports (rest %))))
                            clauses)})

    (in-ns-declaration? form)
    {:name (str (second (second form)))
     :doc nil
     :in-ns? true
     :requirements []}

    :else nil))

(defn- unquote-form
  "The form inside (quote x), or nil for an argument computed at run time."
  [form]
  (when (and (seq? form) (= 'quote (first form)))
    (second form)))

(defn require-call-requirements
  "Requirement maps for a top-level call such as (require '[app.db :as
  db]), or nil for any other form."
  [form]
  (let [head (forms/head-name form)
        directive ({"require" :require "use" :use "require-macros" :require-macros} head)]
    (when (and directive (not (ns-declaration? form)))
      (requirements-of-directive directive
                                 (keep #(if (keyword? %) % (unquote-form %))
                                       (rest form))))))

(defn alias-table
  "Map of alias string to target namespace for the requirements of one file."
  [requirements]
  (into {}
        (keep (fn [{:keys [alias target]}]
                (when alias [alias target])))
        requirements))

(defn refer-table
  "Map of locally visible name to the fully qualified name it refers to."
  [requirements]
  (into {}
        (mapcat (fn [{:keys [refers renames target]}]
                  (let [local-name-of (into {} (map (fn [[local-name original-name]]
                                                      [original-name local-name]))
                                            renames)]
                    (map (fn [referred-name]
                           [(get local-name-of referred-name referred-name)
                            (str target "/" referred-name)])
                         refers))))
        requirements))

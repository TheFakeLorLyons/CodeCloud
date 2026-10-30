(ns codecloud.render.dom
  "A small hiccup-to-DOM builder. Panels build once, then update text,
  classes, and attributes in place, which keeps focus."
  (:require [clojure.string :as string]))

(def ^:private svg-namespace "http://www.w3.org/2000/svg")

(def ^:private svg-tags
  #{"svg" "path" "circle" "line" "polyline" "polygon" "rect" "g" "ellipse"})

(defn- parse-tag
  "Splits :div#id.first.second into its tag name, id, and classes."
  [tag]
  (let [[tag-name & qualifiers] (string/split (name tag) #"(?=[#.])")]
    {:tag-name tag-name
     :id (some #(when (string/starts-with? % "#") (subs % 1)) qualifiers)
     :classes (keep #(when (string/starts-with? % ".") (subs % 1)) qualifiers)}))

(defn- class-string [class-value]
  (if (sequential? class-value)
    (string/join " " (remove nil? class-value))
    (str class-value)))

(defn- set-attribute! [^js node attribute-key value]
  (let [attribute-name (name attribute-key)]
    (cond
      (or (nil? value) (false? value)) nil
      (= :style attribute-key) (doseq [[style-key style-value] value]
                                 (.setProperty (.-style node) (name style-key) (str style-value)))
      (= :class attribute-key) (.setAttribute node "class" (class-string value))
      (string/starts-with? attribute-name "on-") (.addEventListener node (subs attribute-name 3) value)
      (true? value) (.setAttribute node attribute-name "")
      :else (.setAttribute node attribute-name (str value)))))

(defn element
  "DOM node for hiccup such as [:button.chip {:on-click handler}
  \"Label\"]. Strings become text, sequences fragments, nil nothing, and
  DOM nodes pass through."
  [hiccup]
  (cond
    (nil? hiccup) nil
    (string? hiccup) (js/document.createTextNode hiccup)
    (number? hiccup) (js/document.createTextNode (str hiccup))
    (instance? js/Node hiccup) hiccup
    (seq? hiccup) (let [fragment (js/document.createDocumentFragment)]
                    (doseq [child hiccup]
                      (when-let [child-node (element child)]
                        (.appendChild fragment child-node)))
                    fragment)
    :else (let [[tag & remaining] hiccup
                [attributes children] (if (map? (first remaining))
                                        [(first remaining) (rest remaining)]
                                        [{} remaining])
                {:keys [tag-name id classes]} (parse-tag tag)
                ^js node (if (contains? svg-tags tag-name)
                           (js/document.createElementNS svg-namespace tag-name)
                           (js/document.createElement tag-name))]
            (when id (.setAttribute node "id" id))
            (when (seq classes)
              (.setAttribute node "class" (string/join " " (concat classes (when (:class attributes)
                                                                            [(class-string (:class attributes))])))))
            (doseq [[attribute-key value] (if (seq classes) (dissoc attributes :class) attributes)]
              (set-attribute! node attribute-key value))
            (doseq [child children]
              (when-let [child-node (element child)]
                (.appendChild node child-node)))
            node)))

(defn replace-children!
  "Replaces every child of `parent` with the nodes built from `hiccup`."
  [^js parent hiccup]
  (set! (.-textContent parent) "")
  (when-let [built (element hiccup)]
    (.appendChild parent built))
  parent)

(defn set-text!
  "Sets an element's text when it differs, so unchanged text costs nothing."
  [^js node text]
  (let [text (str text)]
    (when (not= text (.-textContent node))
      (set! (.-textContent node) text))))

(defn toggle-class!
  "Adds or removes a class."
  [^js node class-name present?]
  (.toggle (.-classList node) class-name (boolean present?)))

(defn find-in
  "First descendant of `parent` matching a CSS selector."
  [^js parent selector]
  (.querySelector parent selector))

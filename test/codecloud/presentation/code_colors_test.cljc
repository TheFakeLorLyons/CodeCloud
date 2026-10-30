(ns codecloud.presentation.code-colors-test
  (:require [clojure.test :refer [deftest is testing]]
            [codecloud.presentation.code-colors :as code-colors]
            [codecloud.presentation.color :as color]
            [codecloud.presentation.palette :as palette]))

(def ^:private night-rules
  [{"settings" {"foreground" "#d6deeb" "background" "#011627"}}
   {"scope" "comment" "settings" {"foreground" "#8aa0a0" "fontStyle" "italic"}}
   {"scope" ["string" "string.quoted"] "settings" {"foreground" "#ecc48d"}}
   {"scope" "keyword" "settings" {"foreground" "#c792ea"}}
   {"scope" "keyword.control" "settings" {"fontStyle" "bold"}}
   {"scope" "constant - constant.numeric - constant.keyword" "settings" {"foreground" "#ff5874"}}
   {"scope" "constant.numeric" "settings" {"foreground" "#f78c6c"}}
   {"scope" "entity.name.function" "settings" {"foreground" "#82aaff"}}
   {"scope" "source.c keyword.control" "settings" {"foreground" "#ff0000"}}
   {"scope" "support.type" "settings" {"foreground" "#addb67"}}])

(def ^:private night-theme
  {:theme-id "Night Test"
   :dark? true
   :colors {"editor.background" "#011627" "editorLineNumber.foreground" "#ffffff80"}
   :token-colors night-rules
   :bracket-pairs? true})

(def ^:private light-theme
  {:theme-id "Paper Test"
   :dark? false
   :colors {"editor.background" "#fdf6e3" "editor.foreground" "#3b3b3b"}
   :token-colors [{"scope" "comment" "settings" {"foreground" "#6a737d"}}
                  {"scope" "string" "settings" {"foreground" "#0a6e31"}}
                  {"scope" "keyword" "settings" {"foreground" "#d73a49"}}
                  {"scope" "entity.name.function" "settings" {"foreground" "#6f42c1"}}
                  {"scope" "constant" "settings" {"foreground" "#005cc5"}}]
   :bracket-pairs? true})

(def ^:private contrast-theme
  {:theme-id "Bright Test"
   :dark? true
   :colors {"editor.background" "#000000" "editor.foreground" "#ffffff"}
   :token-colors [{"scope" "comment" "settings" {"foreground" "#7ca668"}}
                  {"scope" "string" "settings" {"foreground" "#ce9178"}}
                  {"scope" "keyword" "settings" {"foreground" "#569cd6"}}
                  {"scope" "punctuation" "settings" {"foreground" "#333333"}}]
   :bracket-pairs? false})

(defn- hex-of [colors sky token-kind]
  (:color (code-colors/style colors sky token-kind)))

(deftest reads-a-themes-rules
  (let [colors (code-colors/theme-colors night-theme)]
    (testing "a sky as dark as the theme keeps its readable colors"
      (is (= "#ecc48d" (hex-of colors :night :string)))
      (is (= "#f78c6c" (hex-of colors :night :number)))
      (is (= "#82aaff" (hex-of colors :night :definition)))
      (is (= "#addb67" (hex-of colors :night :keyword)) "a kind tries its scopes in order")
      (is (= "#d6deeb" (hex-of colors :night :text)) "plain text takes the theme's foreground"))
    (testing "a rule refining a scope keeps what it leaves unset"
      (is (= {:color "#c792ea"} (code-colors/style colors :night :special)))
      (is (= {:color "#c792ea" :bold? true} (code-colors/style colors :night :reader)))
      (is (true? (:italic? (code-colors/style colors :night :comment)))))
    (testing "a language family's grammar may name a kind its own way"
      (is (= "#d6deeb" (:color (code-colors/style colors :night :keyword :clojure)))
          "Clojure's keywords are constants, which this theme leaves uncolored")
      (is (= "#addb67" (:color (code-colors/style colors :night :keyword :python))))
      (is (= {:color "#c792ea"} (code-colors/style colors :night :special :c)) "a family without its own scopes")
      (is (= "#d6deeb" (:color (code-colors/style colors :night :reader :clojure)))))
    (testing "selectors naming enclosing scopes and excluded scopes leave a token alone"
      (is (not= "#ff0000" (hex-of colors :night :special)))
      (is (= "#ff5874" (hex-of colors :night :constant)))
      (is (= "#f78c6c" (hex-of colors :night :number))))
    (testing "a translucent color lies over the background"
      (let [[lightness] (color/rgb->oklch (color/hex->rgb (hex-of colors :night :gutter)))]
        (is (< 0.5 lightness 0.75))))))

(deftest applies-the-editors-customizations
  (let [colors (code-colors/theme-colors
                (assoc night-theme
                       :token-customizations {"comments" "#99aabb"
                                              "textMateRules" [{"scope" "string.quoted" "settings" {"foreground" "#aaffaa"}}]
                                              "[Night Test]" {"functions" {"foreground" "#ffcc66" "fontStyle" "italic"}}
                                              "[Other Theme]" {"strings" "#ff00ff"}
                                              "[*Night*][Paper Test]" {"numbers" "#ffaa88"}}
                       :color-customizations {"editor.foreground" "#eeeeee"
                                              "[Night Test]" {"editorCursor.foreground" "#ffcc00"}}))]
    (is (= "#99aabb" (hex-of colors :night :comment)))
    (is (= "#aaffaa" (hex-of colors :night :string))
        "a rule as specific as the theme's wins, and a block for another theme changes nothing")
    (is (= {:color "#ffcc66" :italic? true} (code-colors/style colors :night :definition)))
    (is (= "#ffaa88" (hex-of colors :night :number)) "a block may name several themes and use wildcards")
    (is (= "#eeeeee" (hex-of colors :night :text)))
    (is (= "#ffcc00" (hex-of colors :night :caret)))))

(deftest colors-bracket-pairs
  (let [night (code-colors/theme-colors night-theme)
        customized (code-colors/theme-colors (assoc night-theme :color-customizations
                                                    {"editorBracketHighlight.foreground2" "#00000000"
                                                     "editorBracketHighlight.foreground4" "#ff8800"}))]
    (is (= ["#ffd700" "#da70d6" "#179fff"] (get-in night [:night :bracket-levels])))
    (is (= "#da70d6" (code-colors/bracket-color night :night 4)) "depths cycle through the levels")
    (is (= ["#ffd700" "#179fff" "#ff8800"] (get-in customized [:night :bracket-levels]))
        "transparent levels drop out")
    (is (nil? (code-colors/bracket-color (code-colors/theme-colors contrast-theme) :night 0)))
    (is (nil? (code-colors/bracket-color code-colors/own :day 0)))))

(def ^:private every-glass
  "Glass of pages of every blend on each sky, and of cycles and parse
  errors."
  (into {}
        (map (fn [sky]
               [sky (map color/oklch->rgb
                         (concat [(palette/color-of sky :page-cycle) (palette/color-of sky :page-parse-error)]
                                 (for [hue (range 0 360 15)
                                       lightness [0.0 0.4 0.55 0.7 0.85 1.0]
                                       chroma [0 0.08 0.2]]
                                   (:panel (palette/page-oklch sky {:blend {sky [lightness chroma hue]}})))))]))
        [:day :night]))

(deftest every-color-reads-on-every-page
  (doseq [theme [night-theme light-theme contrast-theme]
          :let [colors (code-colors/theme-colors theme)]
          sky [:day :night]
          [token-kind least] {:text 4.5 :comment 3.0 :string 3.0 :special 3.0 :definition 3.0 :number 3.0
                              :gutter 2.2 :bracket 2.2 :caret 3.0}
          glass (get every-glass sky)]
    (is (>= (color/contrast-ratio (color/hex->rgb (hex-of colors sky token-kind)) glass) (- least 0.01))
        (str (:theme-id theme) " " (name token-kind) " on the " (name sky) " sky")))
  (testing "a light theme's colors stand as far above the night glass as below their background, keeping their hues"
    (let [colors (code-colors/theme-colors light-theme)
          lightness-of #(first (color/rgb->oklch (color/hex->rgb %)))
          [day-lightness _ day-hue] (color/rgb->oklch (color/hex->rgb (hex-of colors :day :special)))
          [night-lightness _ night-hue] (color/rgb->oklch (color/hex->rgb (hex-of colors :night :special)))
          night-glass-lightness (first (:panel (palette/page-oklch :night {})))]
      (is (= "#d73a49" (hex-of colors :day :special)))
      (is (< (Math/abs (double (- (- night-lightness night-glass-lightness)
                                  (- (lightness-of "#fdf6e3") day-lightness))))
             0.01))
      (is (< (Math/abs (double (- night-hue day-hue))) 8)))))

(deftest own-colors-stand-in-for-a-theme
  (is (= (hex-of code-colors/own :day :text) (hex-of code-colors/own :day :caret)))
  (is (= (hex-of code-colors/own :night :text) (hex-of code-colors/own :night :no-such-kind))))

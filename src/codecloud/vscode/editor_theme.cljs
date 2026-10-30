(ns codecloud.vscode.editor-theme
  "Code colors of the theme the editor shows, read from the files of the
  extension contributing it, with the editor's customizations over them."
  (:require [codecloud.io.task :as task]
            [codecloud.io.theme-files :as theme-files]
            [codecloud.presentation.code-colors :as code-colors]
            [codecloud.vscode.api :as api]))

(def ^:private ui-theme-kinds
  "The kind of theme each base of a theme contribution makes."
  {"vs" :light "vs-dark" :dark "hc-black" :high-contrast "hc-light" :high-contrast-light})

(def ^:private preferred-theme-settings
  "The workbench setting naming the theme the editor takes for each kind
  when it follows the system's look."
  {:light "preferredLightColorTheme"
   :dark "preferredDarkColorTheme"
   :high-contrast "preferredHighContrastColorTheme"
   :high-contrast-light "preferredHighContrastLightColorTheme"})

(defn- active-theme
  "The contribution of the theme the editor shows: the chosen theme when
  it is of the kind shown, else the preferred theme of that kind, or nil
  when no installed extension contributes either."
  []
  (let [themes (api/theme-contributions)
        named (fn [theme-id]
                (when (string? theme-id)
                  (or (some #(when (= theme-id (:id %)) %) themes)
                      (some #(when (= theme-id (:label %)) %) themes))))
        shown-kind (api/theme-kind-name)
        chosen (named (api/setting "workbench" "colorTheme" nil))]
    (if (and chosen (= shown-kind (get ui-theme-kinds (:ui-theme chosen))))
      chosen
      (or (named (api/setting "workbench" (get preferred-theme-settings shown-kind) nil))
          chosen))))

(defn read-code-colors
  "Resolves to the code colors of the theme the editor shows, with bracket
  pair colors as code in `language-id` has them, or to nil when the
  theme's files cannot be read or hold no token colors."
  [language-id]
  (-> (task/resolved nil)
      (task/then (fn [_]
                   (when-let [{:keys [id label ui-theme] theme-path :path} (active-theme)]
                     (task/then (theme-files/read-theme theme-path)
                                (fn [{:keys [colors token-colors]}]
                                  (when (seq token-colors)
                                    (code-colors/theme-colors
                                     {:theme-id (or id label)
                                      :dark? (contains? #{:dark :high-contrast}
                                                        (get ui-theme-kinds ui-theme (api/theme-kind-name)))
                                      :colors colors
                                      :token-colors token-colors
                                      :token-customizations (api/data-setting "editor" "tokenColorCustomizations")
                                      :color-customizations (api/data-setting "workbench" "colorCustomizations")
                                      :bracket-pairs? (true? (api/editor-setting language-id
                                                                                 "bracketPairColorization.enabled"
                                                                                 true))})))))))
      (task/recover (fn [_error] nil))))

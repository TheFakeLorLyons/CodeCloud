(ns codecloud.render.hud.views
  "The views panel, whose switches choose how pages and the title read,
  whose named views, languages, and folder tree turn pages on and off,
  whose list of windows shows each in the cloud or hides it there, whose
  list of what the cloud leaves out brings ignored folders and files in,
  and which saves the selection as a view. The tree is built once per
  snapshot and its boxes are checked in place, so a box keeps focus and
  the list keeps its scroll while pages come and go."
  (:require [codecloud.languages.catalog :as catalog]
            [codecloud.model.ignored :as ignored]
            [codecloud.presentation.scene :as scene]
            [codecloud.presentation.visibility :as visibility]
            [codecloud.presentation.windows-state :as windows-state]
            [codecloud.render.dom :as dom]
            [codecloud.render.hud.dock :as dock]
            [codecloud.render.icons :as icons]))

(defn- view-options [dispatch named-views]
  [:section.views-section
   [:button.view-option {:data-view "" :on-click #(dispatch [:clear-filter])} "Everything"]
   (for [view-label (sort (keys named-views))]
     [:button.view-option {:data-view view-label :on-click #(dispatch [:toggle-view view-label])}
      view-label])])

(defn- toggle-row [attribute value label on-toggle]
  [:label.visibility-row
   [:input.visibility-toggle {:type "checkbox" attribute value :on-change (fn [_event] (on-toggle))}]
   [:span.visibility-name label]
   [:span.visibility-count]])

(defn- language-section
  "One box per language present, when the cloud holds several."
  [dispatch snapshot]
  (let [languages (sort-by (comp :label catalog/language)
                           (distinct (map #(:language % :clojure) (vals (:namespaces snapshot)))))]
    (when (> (count languages) 1)
      [:section.visibility-section
       [:h3 "Languages"]
       [:ul.visibility-list
        (for [language-id languages]
          [:li (toggle-row :data-language (name language-id) (:label (catalog/language language-id))
                           #(dispatch [:toggle-language language-id]))])]])))

(defn- page-item [dispatch snapshot namespace-id]
  [:li.tree-page
   (toggle-row :data-page namespace-id (:leaf (scene/label-parts (get-in snapshot [:namespaces namespace-id])))
               #(dispatch [:toggle-page namespace-id]))])

(defn- folder-item
  "A folder's row, with its subfolders and pages below it while open."
  [dispatch snapshot open-folders cloud-name {:keys [path page-ids folders]}]
  (let [open? (contains? open-folders path)
        {:keys [leaf]} (scene/directory-label path cloud-name)]
    [:li.tree-folder
     [:div.tree-row
      [:button.tree-disclosure {:class (when open? "is-open")
                                :data-disclosure path
                                :aria-expanded (str open?)
                                :aria-label (str (if open? "Close " "Open ") leaf)
                                :on-click #(dispatch [:toggle-folder-open path])}
       icons/disclosure]
      (toggle-row :data-folder path leaf #(dispatch [:toggle-folder path]))]
     (when open?
       [:ul.tree-children
        (map #(folder-item dispatch snapshot open-folders cloud-name %) folders)
        (map #(page-item dispatch snapshot %) page-ids)])]))

(defn- save-view-form [dispatch]
  [:form.save-view {:on-submit (fn [^js event]
                                 (.preventDefault event)
                                 (let [^js input (.querySelector (.-target event) "input")]
                                   (dispatch [:save-view (.-value input)])
                                   (set! (.-value input) "")))}
   [:label {:for "codecloud-view-name"} "Save as a view"]
   [:div.save-view-row
    [:input#codecloud-view-name {:type "text" :placeholder "View name" :autocomplete "off" :spellcheck "false"}]
    [:button.action-button.is-primary {:type "submit"} "Save view"]]])

(defn- display-section
  "Switches for how pages read and how much the title says."
  [dispatch]
  [:section.views-section.display-section
   [:label.visibility-row
    [:input.names-only-toggle {:type "checkbox" :on-change (fn [_event] (dispatch [:toggle-display-mode]))}]
    [:span.visibility-name "Show names only (N)"]]
   [:label.visibility-row
    [:input.title-summary-toggle {:type "checkbox" :on-change (fn [_event] (dispatch [:toggle-title-summary]))}]
    [:span.visibility-name "Hide totals and hidden folders"]]
   [:label.visibility-row
    [:input.toolbar-colors-toggle {:type "checkbox" :on-change (fn [_event] (dispatch [:toggle-toolbar-colors]))}]
    [:span.visibility-name "Color the toolbar's icons"]]
   [:label.visibility-row {:title "Hides the toolbar, the cloud's name, and the timeline until the pointer reaches their edge"}
    [:input.auto-hide-toggle {:type "checkbox" :on-change (fn [_event] (dispatch [:toggle-auto-hide-bars]))}]
    [:span.visibility-name "Auto-hide the toolbars (T)"]]])

(defn- show-display!
  "Checks the display switches in place, so a toggled box keeps focus."
  [^js root {:keys [display-mode title-summary-hidden? auto-hide-bars? settings]}]
  (when-let [^js names-toggle (dom/find-in root ".names-only-toggle")]
    (set! (.-checked names-toggle) (= :names display-mode)))
  (when-let [^js summary-toggle (dom/find-in root ".title-summary-toggle")]
    (set! (.-checked summary-toggle) (true? title-summary-hidden?)))
  (when-let [^js colors-toggle (dom/find-in root ".toolbar-colors-toggle")]
    (set! (.-checked colors-toggle) (not= :plain (:toolbar-colors settings))))
  (when-let [^js auto-hide-toggle (dom/find-in root ".auto-hide-toggle")]
    (set! (.-checked auto-hide-toggle) (true? auto-hide-bars?))))

(defn- views-content [dispatch {:keys [snapshot named-views open-folders cloud-name]} tree]
  (list
   [:header.panel-header
    [:h2 "Views"]
    (dock/minimize-button dispatch :views)
    [:button.icon-button {:aria-label "Close" :on-click #(dispatch [:toggle-views-panel])} icons/close]]
   (display-section dispatch)
   (view-options dispatch named-views)
   (language-section dispatch snapshot)
   [:section.visibility-section
    [:h3 "Folders"]
    [:ul.visibility-tree (folder-item dispatch snapshot open-folders cloud-name tree)]]
   [:section.visibility-section.windows-section
    [:h3 "Windows"]
    [:ul.visibility-list.window-list]]
   [:section.visibility-section.ignored-section
    [:h3
     [:button.ignored-heading {:type "button"
                               :aria-expanded "false"
                               :title "Folders and files the cloud leaves out, each of which can be brought in, gray"
                               :on-click #(dispatch [:toggle-ignored-open])}
      icons/disclosure
      [:span "Ignored"]]]
    [:ul.visibility-tree.ignored-list]]
   (save-view-form dispatch)))

(defn- show-windows!
  "Lists the open windows, each with a box showing it in the cloud or
  hiding it there and a name flown to by a click, keeping the focus on
  the box it was on."
  [^js root dispatch windows]
  (let [kind-words {:web "Web page" :html "HTML" :markdown "Markdown" :pdf "PDF" :image "Image"}
        ^js section (dom/find-in root ".windows-section")
        ^js active (.-activeElement js/document)
        focused-window (when (and active (.contains section active)) (.getAttribute active "data-window"))]
    (dom/toggle-class! section "is-hidden" (empty? windows))
    (dom/replace-children!
     (dom/find-in section ".window-list")
     (for [{:keys [id kind hidden?] :as window} windows
           :let [title (windows-state/window-title window)]]
       [:li
        [:div.visibility-row {:class (when hidden? "is-off")}
         [:input.visibility-toggle (cond-> {:type "checkbox"
                                            :data-window id
                                            :aria-label (str "Show " title " in the cloud")
                                            :on-change (fn [_event] (dispatch [:toggle-window-shown id]))}
                                     (not hidden?) (assoc :checked true))]
         [:button.visibility-name.window-name {:type "button" :title "Fly to it" :on-click #(dispatch [:fly-to-window id])}
          title]
         [:span.visibility-count (get kind-words kind)]]]))
    (when focused-window
      (some-> (some #(when (= focused-window (.getAttribute ^js % "data-window")) %)
                    (array-seq (.querySelectorAll section "[data-window]")))
              .focus))))

(def ^:private reason-words
  {:git "Git" :cloud "CodeCloud" :type "Not read"})

(def ^:private reason-titles
  {:git "Git ignores it"
   :cloud "CodeCloud or the cloud file ignores it"
   :type "CodeCloud does not read this kind of file"})

(defn- entry-label
  "What an entry reads as, its name inside an open folder, else its path
  from the cloud's folder, a folder's without its closing slash."
  [path parent]
  (let [inner (subs path (count parent))]
    (if (ignored/folder-path? inner) (subs inner 0 (dec (count inner))) inner)))

(defn- ignored-rows
  "Rows for what `folder` holds that the cloud leaves out, each open
  folder's rows beneath it, a box bringing each into the cloud."
  [dispatch {:keys [ignored-listing open-ignored-folders shown-ignored] :as state} folder]
  (if-let [{:keys [entries more]} (get ignored-listing folder)]
    (concat
     (for [{:keys [path folder? reason]} entries
           :let [shown-by (ignored/shown-by shown-ignored path)
                 open? (contains? open-ignored-folders path)
                 label (entry-label path folder)]]
       [:li {:class (if folder? "tree-folder" "tree-page")}
        [:div.tree-row
         (when folder?
           [:button.tree-disclosure {:class (when open? "is-open")
                                     :data-ignored-disclosure path
                                     :aria-expanded (str open?)
                                     :aria-label (str (if open? "Close " "Open ") label)
                                     :on-click #(dispatch [:toggle-ignored-folder-open path])}
            icons/disclosure])
         [:label.visibility-row {:class (when-not shown-by "is-off") :title (get reason-titles reason)}
          [:input.visibility-toggle {:type "checkbox"
                                     :data-ignored path
                                     :aria-label (str "Show " label " in the cloud")
                                     :checked (some? shown-by)
                                     :disabled (= :folder shown-by)
                                     :on-change (fn [_event] (dispatch [:toggle-ignored-shown path]))}]
          [:span.visibility-name label]
          [:span.visibility-count (get reason-words reason)]]]
        (when (and folder? open?)
          [:ul.tree-children (ignored-rows dispatch state path)])])
     (cond
       (and (empty? entries) (= "" folder)) (list [:li.ignored-note "The cloud leaves nothing out."])
       (empty? entries) (list [:li.ignored-note "Nothing inside to show"])
       (pos? more) (list [:li.ignored-note (str "And " more " more")])))
    (list [:li.ignored-note "Reading…"])))

(def ^:private ignored-attributes ["data-ignored" "data-ignored-disclosure"])

(defn- show-ignored!
  "Opens or closes the list of what the cloud leaves out and fills it,
  keeping the focus on the control it was on, or on `refocus`, the
  [attribute value] of the control focused before the panel was built
  again."
  [^js root dispatch {:keys [ignored-open?] :as state} refocus]
  (let [^js section (dom/find-in root ".ignored-section")
        ^js heading (dom/find-in section ".ignored-heading")
        ^js active (.-activeElement js/document)
        focused (or (when (and active (.contains section active))
                      (some (fn [attribute] (when (.hasAttribute active attribute) [attribute (.getAttribute active attribute)]))
                            ignored-attributes))
                    (when (some #{(first refocus)} ignored-attributes) refocus))]
    (dom/toggle-class! heading "is-open" ignored-open?)
    (.setAttribute heading "aria-expanded" (str (true? ignored-open?)))
    (dom/replace-children! (dom/find-in section ".ignored-list")
                           (when ignored-open? (ignored-rows dispatch state "")))
    (when-let [[attribute value] focused]
      (some-> (some #(when (= value (.getAttribute ^js % attribute)) %)
                    (array-seq (.querySelectorAll section (str "[" attribute "]"))))
              .focus))))

(defn- folders-by-path [tree]
  (into {} (map (juxt :path identity)) (tree-seq map? :folders tree)))

(defn- show-share!
  "Checks a box for :all, clears it for :none, and marks it mixed for
  :some, with the shown and total counts beside it."
  [^js toggle share shown-count total-count]
  (let [^js row (.-parentNode toggle)]
    (set! (.-checked toggle) (= :all share))
    (set! (.-indeterminate toggle) (= :some share))
    (dom/set-text! (dom/find-in row ".visibility-count")
                   (if (= :all share) (str total-count) (str shown-count "/" total-count)))
    (dom/toggle-class! row "is-off" (= :none share))))

(defn- show-visibility!
  "Brings every box, count, and view button in line with what shows."
  [^js root {:keys [snapshot view-filter visible-ids]} folders]
  (let [shown-count (fn [ids] (count (filter visible-ids ids)))
        show-group! (fn [^js toggle ids]
                      (show-share! toggle (visibility/shown-share visible-ids ids) (shown-count ids) (count ids)))]
    (doseq [^js toggle (array-seq (.querySelectorAll root "[data-page]"))]
      (let [shown? (contains? visible-ids (.getAttribute toggle "data-page"))]
        (set! (.-checked toggle) shown?)
        (dom/toggle-class! (.-parentNode toggle) "is-off" (not shown?))))
    (doseq [^js toggle (array-seq (.querySelectorAll root "[data-folder]"))]
      (show-group! toggle (:all-ids (get folders (.getAttribute toggle "data-folder")))))
    (doseq [^js toggle (array-seq (.querySelectorAll root "[data-language]"))]
      (show-group! toggle (visibility/pages-in-language snapshot (keyword (.getAttribute toggle "data-language")))))
    (doseq [^js button (array-seq (.querySelectorAll root "[data-view]"))]
      (let [view-label (.getAttribute button "data-view")
            on? (if (= "" view-label)
                  (not (visibility/filtering? view-filter))
                  (contains? (set (:active-views view-filter)) view-label))]
        (dom/toggle-class! button "is-on" on?)
        (.setAttribute button "aria-pressed" (str on?))))
    (some-> (dom/find-in root ".save-view")
            (dom/toggle-class! "is-hidden" (not (visibility/filtering? view-filter))))))

(defn- focused-control [^js root]
  (let [^js active (.-activeElement js/document)]
    (when (and active (.contains root active))
      (some (fn [attribute] (when (.hasAttribute active attribute) [attribute (.getAttribute active attribute)]))
            ["data-page" "data-folder" "data-language" "data-view" "data-disclosure" "data-ignored"
             "data-ignored-disclosure"]))))

(defn- rebuild!
  "Builds the panel again, keeping its scroll and the focused control."
  [^js root dispatch state tree]
  (let [scroll (.-scrollTop root)
        [attribute value] (focused-control root)]
    (dom/replace-children! root (views-content dispatch state tree))
    (set! (.-scrollTop root) scroll)
    (when attribute
      (some-> (some #(when (= value (.getAttribute ^js % attribute)) %)
                    (array-seq (.querySelectorAll root (str "[" attribute "]"))))
              .focus))))

(def panel
  {:id :views
   :mount (fn [dispatch]
            (let [root (dom/element [:aside.hud-views {:aria-label "Views"}])]
              ;; Typing a view's name must not reach the cloud's keys.
              (.addEventListener root "keydown" (fn [^js event] (.stopPropagation event)))
              {:root root :dispatch dispatch :built (atom nil) :tree (atom nil) :shown (atom nil) :listed-windows (atom nil)
               :listed-ignored (atom nil)}))
   :update (fn [{:keys [root dispatch built tree shown listed-windows listed-ignored]}
                {:keys [views-open? snapshot named-views open-folders cloud-name visible-ids view-filter windows] :as state}]
             (dom/toggle-class! root "is-open" views-open?)
             (when (and views-open? snapshot)
               (let [build-key [snapshot named-views open-folders cloud-name]
                     ;; A box in the list of what the cloud leaves out
                     ;; brings in pages, which builds the panel again, so
                     ;; its focus comes back once the list is filled.
                     refocus (when (not= build-key @built) (focused-control root))]
                 (when (not= build-key @built)
                   (when-not (identical? snapshot (first @built))
                     (let [folder-tree (visibility/folder-tree snapshot)]
                       (reset! tree {:root folder-tree :by-path (folders-by-path folder-tree)})))
                   (reset! built build-key)
                   (reset! shown nil)
                   (reset! listed-windows nil)
                   (reset! listed-ignored nil)
                   (rebuild! root dispatch state (:root @tree)))
                 (when (not= windows @listed-windows)
                   (reset! listed-windows windows)
                   (show-windows! root dispatch windows))
                 (let [ignored-key (select-keys state [:ignored-open? :ignored-listing :open-ignored-folders :shown-ignored])]
                   (when (not= ignored-key @listed-ignored)
                     (reset! listed-ignored ignored-key)
                     (show-ignored! root dispatch state refocus)))
                 (when (not= [visible-ids view-filter] @shown)
                   (reset! shown [visible-ids view-filter])
                   (show-visibility! root state (:by-path @tree)))
                 (show-display! root state))))})

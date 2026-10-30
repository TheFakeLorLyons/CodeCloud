(ns codecloud.render.hud.views
  "The views panel: named views, languages, and the folder tree, each
  turning pages on and off, and saving the selection as a view. The tree
  is built once per snapshot and its boxes are checked in place, so a box
  keeps focus and the list keeps its scroll while pages come and go."
  (:require [codecloud.languages.catalog :as catalog]
            [codecloud.presentation.scene :as scene]
            [codecloud.presentation.visibility :as visibility]
            [codecloud.render.dom :as dom]
            [codecloud.render.icons :as icons]))

(defn- view-options [dispatch named-views]
  [:section.views-section
   [:button.view-option {:data-view "" :on-click #(dispatch [:clear-filter])} "Everything"]
   (for [view-label (sort (keys named-views))]
     [:button.view-option {:data-view view-label :on-click #(dispatch [:toggle-view view-label])}
      view-label])])

(defn- toggle-row
  "A checkbox row turning a page or a group on and off, with room for a
  count."
  [attribute value label on-toggle]
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

(defn- views-content [dispatch {:keys [snapshot named-views open-folders cloud-name]} tree]
  (list
   [:header.panel-header
    [:h2 "Views"]
    [:button.icon-button {:aria-label "Close" :on-click #(dispatch [:toggle-views-panel])} icons/close]]
   (view-options dispatch named-views)
   (language-section dispatch snapshot)
   [:section.visibility-section
    [:h3 "Folders"]
    [:ul.visibility-tree (folder-item dispatch snapshot open-folders cloud-name tree)]]
   (save-view-form dispatch)))

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

(defn- focused-control
  "The control inside `root` that has focus, as [attribute value]."
  [^js root]
  (let [^js active (.-activeElement js/document)]
    (when (and active (.contains root active))
      (some (fn [attribute] (when (.hasAttribute active attribute) [attribute (.getAttribute active attribute)]))
            ["data-page" "data-folder" "data-language" "data-view" "data-disclosure"]))))

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
              {:root root :dispatch dispatch :built (atom nil) :tree (atom nil) :shown (atom nil)}))
   :update (fn [{:keys [root dispatch built tree shown]}
                {:keys [views-open? snapshot named-views open-folders cloud-name visible-ids view-filter] :as state}]
             (dom/toggle-class! root "is-open" views-open?)
             (when (and views-open? snapshot)
               (let [build-key [snapshot named-views open-folders cloud-name]]
                 (when (not= build-key @built)
                   (when-not (identical? snapshot (first @built))
                     (let [folder-tree (visibility/folder-tree snapshot)]
                       (reset! tree {:root folder-tree :by-path (folders-by-path folder-tree)})))
                   (reset! built build-key)
                   (reset! shown nil)
                   (rebuild! root dispatch state (:root @tree)))
                 (when (not= [visible-ids view-filter] @shown)
                   (reset! shown [visible-ids view-filter])
                   (show-visibility! root state (:by-path @tree))))))})

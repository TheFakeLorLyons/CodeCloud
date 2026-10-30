(ns codecloud.render.hud.menu
  "The right-click menu for a page, a folder's cloud, or the sky."
  (:require [codecloud.model.selection :as selection]
            [codecloud.presentation.page :as page]
            [codecloud.presentation.scene :as scene]
            [codecloud.presentation.visibility :as visibility]
            [codecloud.render.dom :as dom]
            [codecloud.render.hud.words :as words]))

(defn- page-items [{:keys [snapshot focused-id moved-pages]} namespace-id]
  (let [node (get-in snapshot [:namespaces namespace-id])
        directory (page/directory node)]
    {:heading (scene/label-parts node)
     :items (cond-> [[(if (= namespace-id focused-id) "Set it back" "Bring it closer") [:click-namespace namespace-id]]
                     ["Fly to it" [:double-click-namespace namespace-id]]]
              (contains? moved-pages namespace-id) (conj ["Put it back" [:put-back namespace-id]])
              true (into [["Open in the editor" [:dive {:path (:path (page/primary-file node)) :line 1}]]
                          ["Show only it and its neighbors" [:show-only (visibility/neighborhood-patterns snapshot namespace-id)]]
                          ["Hide it" [:hide-page namespace-id]]
                          ["Hide its folder" [:hide-folder directory]]]))}))

(defn- folder-items [{:keys [lifted-folder cloud-name]} path]
  {:heading (scene/directory-label path cloud-name)
   :items [[(if (= path lifted-folder) "Set it back" "Bring it closer") [:click-folder path]]
           ["Fly to it" [:frame-directory path]]
           ["Show only this folder" [:show-only [(selection/directory-pattern path)]]]
           ["Hide this folder" [:hide-folder path]]]})

(defn- sky-items [{:keys [view-filter moved-pages]}]
  {:heading nil
   :items (cond-> [["Show the whole cloud" [:frame-all]]]
            (visibility/filtering? view-filter) (conj ["Show everything" [:clear-filter]])
            (seq moved-pages) (conj ["Put every page back" [:put-everything-back]])
            true (conj ["Arrange" [:toggle-arrange-panel]]
                       ["Views" [:toggle-views-panel]]))})

(defn- menu-items
  "{:heading :items} of the menu for what was right-clicked, each item a
  [label event] pair."
  [{:keys [kind id path]} state]
  (case kind
    :page (page-items state id)
    :folder (folder-items state path)
    (sky-items state)))

(defn- place-menu!
  "Puts the menu at the pointer, kept inside the window."
  [^js root {:keys [x y]} item-count]
  (let [menu-width 290
        item-height 34
        heading-height 44
        margin 8
        menu-height (+ heading-height (* item-height item-count))]
    (set! (.. root -style -left) (str (max margin (min x (- (.-innerWidth js/window) menu-width margin))) "px"))
    (set! (.. root -style -top) (str (max margin (min y (- (.-innerHeight js/window) menu-height margin))) "px"))))

(def panel
  {:id :menu
   :mount (fn [dispatch]
            {:root (dom/element [:div.hud-menu {:role "menu"}])
             :dispatch dispatch
             :shown (atom nil)})
   :update (fn [{:keys [root dispatch shown]} {:keys [menu] :as state}]
             (when (not= menu @shown)
               (reset! shown menu)
               (if menu
                 (let [{:keys [heading items]} (menu-items menu state)]
                   (dom/replace-children!
                    root
                    (list (when heading [:p.menu-heading (words/name-heading heading)])
                          (for [[label event] items]
                            [:button.menu-item {:role "menuitem" :on-click #(dispatch event)} label])))
                   (place-menu! root menu (count items))
                   (dom/toggle-class! root "is-open" true)
                   (js/setTimeout #(some-> (dom/find-in root ".menu-item") .focus) 0))
                 (dom/toggle-class! root "is-open" false))))})

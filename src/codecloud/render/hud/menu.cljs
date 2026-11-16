(ns codecloud.render.hud.menu
  "The right-click menu for a page, a folder's cloud, or the sky, and the
  menu of definitions a name could mean."
  (:require [codecloud.model.selection :as selection]
            [codecloud.model.source :as source]
            [codecloud.presentation.page :as page]
            [codecloud.presentation.scene :as scene]
            [codecloud.presentation.visibility :as visibility]
            [codecloud.presentation.windows-state :as windows-state]
            [codecloud.render.dom :as dom]
            [codecloud.render.hud.words :as words]
            [codecloud.render.typing :as typing]))

(defn- working-tree? [{:keys [snapshot]}]
  (= :working-tree (:id snapshot)))

(defn- page-items [{:keys [snapshot focused-id moved-pages] :as state} namespace-id]
  (let [node (get-in snapshot [:namespaces namespace-id])
        directory (page/directory node)]
    {:heading (scene/label-parts node)
     :items (cond-> [[(if (= namespace-id focused-id) "Set it back" "Bring it closer") [:click-namespace namespace-id]]
                     ["Fly to it" [:double-click-namespace namespace-id]]
                     (if (windows-state/pinned? state {:kind :page :id namespace-id})
                       ["Unpin from the grid" [:unpin {:kind :page :id namespace-id}]]
                       ["Pin to the grid" [:pin {:kind :page :id namespace-id}]])]
              (contains? moved-pages namespace-id) (conj ["Reset its position" [:put-back namespace-id]])
              true (into [["Open in the editor" [:dive {:path (:path (page/primary-file node)) :line 1}]]])
              (working-tree? state) (into (concat (map-indexed (fn [index {:keys [title]}]
                                                                 [title [:run-page-command index namespace-id]])
                                                               (get-in state [:settings :page-commands]))
                                                  [["Run a command…" [:pick-page-command namespace-id] (typing/shortcut "Ctrl+Alt+R")]
                                                   ["New file beside it" [:new-file directory]]]))
              true (into [["Show only it and its neighbors" [:show-only (visibility/neighborhood-patterns snapshot namespace-id)]]
                          ["Hide it" [:hide-page namespace-id]]
                          ["Hide its folder" [:hide-folder directory]]]))}))

(defn- folder-items [{:keys [lifted-folder cloud-name] :as state} path]
  {:heading (scene/directory-label path cloud-name)
   :items (cond-> [[(if (= path lifted-folder) "Set it back" "Bring it closer") [:click-folder path]]
                   ["Fly to it" [:frame-directory path]]]
            (working-tree? state) (conj ["New file in this folder" [:new-file path]])
            true (into [["Show only this folder" [:show-only [(selection/directory-pattern path)]]]
                        ["Hide this folder" [:hide-folder path]]]))})

(defn- window-items [state window-id]
  (let [{:keys [kind] :as window} (windows-state/window-of state window-id)]
    {:heading {:prefix "" :leaf (windows-state/window-title window)}
     :items [["Fly to it" [:fly-to-window window-id]]
             (if (windows-state/pinned? state {:kind :window :id window-id})
               ["Unpin from the grid" [:unpin {:kind :window :id window-id}]]
               ["Pin to the grid" [:pin {:kind :window :id window-id}]])
             ["Reload" [:reload-window window-id]]
             [(if (= :web kind) "Open in the browser" "Open in VS Code") [:reveal-window window-id]]
             ["Close" [:close-window window-id]]]}))

(defn- sky-items [{:keys [view-filter moved-pages grid] :as state}]
  {:heading nil
   :items (cond-> [["Show the whole cloud" [:frame-all]]]
            (seq (:cells grid)) (conj [(if (:shown? grid) "Hide the grid" "Show the grid") [:toggle-grid]])
            (visibility/filtering? view-filter) (conj ["Show everything" [:clear-filter]])
            (seq moved-pages) (conj ["Reset page positions" [:reset-page-positions]])
            (working-tree? state) (conj ["New file" [:new-file ""]])
            true (conj ["Show a web page or file" [:toggle-windows-menu]]
                       ["Arrange" [:toggle-arrange-panel]]
                       ["Views" [:toggle-views-panel]]))})

(defn- definition-items [{definition-name :name :keys [choices]}]
  {:heading {:prefix "" :leaf definition-name}
   :items (for [{choice-name :name :keys [path line] :as location} choices]
            [(str choice-name " in " (source/file-name path) ", line " line) [:show-definition location]])})

(defn- menu-items
  "{:heading :items} of the menu for what was right-clicked or which
  definition a name means, each item a [label event] pair."
  [{:keys [kind id path] :as menu} state]
  (case kind
    :page (page-items state id)
    :folder (folder-items state path)
    :definitions (definition-items menu)
    :window (window-items state id)
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
                          (for [[label event shortcut] items]
                            [:button.menu-item {:role "menuitem" :on-click #(dispatch event)}
                             label
                             (when shortcut [:kbd.menu-shortcut shortcut])])))
                   (place-menu! root menu (count items))
                   (dom/toggle-class! root "is-open" true)
                   (js/setTimeout #(some-> (dom/find-in root ".menu-item") .focus) 0))
                 (dom/toggle-class! root "is-open" false))))})

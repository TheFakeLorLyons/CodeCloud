(ns codecloud.presentation.ignored-state-test
  (:require [clojure.test :refer [deftest is testing]]
            [codecloud.presentation.ui-state :as ui-state]))

(defn- sent [effects message-type]
  (keep (fn [[effect-kind message]] (when (and (= :send effect-kind) (= message-type (:type message))) message)) effects))

(deftest the-list-of-what-the-cloud-leaves-out-opens-on-request
  (let [{:keys [state effects]} (ui-state/handle ui-state/initial-state [:toggle-ignored-open])]
    (is (false? (:ignored-open? ui-state/initial-state)) "it starts closed")
    (is (:ignored-open? state))
    (is (= [""] (map :path (sent effects :view/list-ignored))) "opening asks the host for the list")
    (let [listed (:state (ui-state/handle state [:host {:type :host/ignored :path ""
                                                        :entries [{:path "target/" :folder? true :reason :git}]}]))
          opened (ui-state/handle listed [:toggle-ignored-folder-open "target/"])]
      (is (= {:entries [{:path "target/" :folder? true :reason :git}] :more 0} (get-in listed [:ignored-listing ""])))
      (is (= ["target/"] (map :path (sent (:effects opened) :view/list-ignored)))
          "opening a folder asks what it holds")
      (testing "opening the list again asks again for every open folder"
        (let [closed (:state (ui-state/handle (:state opened) [:toggle-ignored-open]))
              reopened (ui-state/handle closed [:toggle-ignored-open])]
          (is (not (:ignored-open? closed)))
          (is (= ["" "target/"] (map :path (sent (:effects reopened) :view/list-ignored))))))
      (is (empty? (:effects (ui-state/handle (:state opened) [:toggle-ignored-folder-open "target/"])))
          "closing a folder asks nothing"))))

(deftest a-chosen-folder-or-file-is-remembered-for-the-cloud
  (let [{:keys [state effects]} (ui-state/handle ui-state/initial-state [:toggle-ignored-shown "target/"])]
    (is (= ["target/"] (:shown-ignored state)))
    (is (= [{:type :view/save-preference :key :shown-ignored :value ["target/"]}] (sent effects :view/save-preference)))
    (is (= [] (:shown-ignored (:state (ui-state/handle state [:toggle-ignored-shown "target/"]))))
        "choosing it again takes it back out"))
  (is (= ["target/"] (:shown-ignored (:state (ui-state/handle ui-state/initial-state
                                                              [:host {:type :host/init :shown-ignored ["target/" "../up/"]}]))))
      "the host's remembered choice comes back, keeping paths inside the cloud"))

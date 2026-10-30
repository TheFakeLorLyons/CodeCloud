(ns codecloud.test-runner
  "Runs every CodeCloud test namespace on Node and reports failure through
  the process exit code."
  (:require [cljs.test :as test]
            [codecloud.io.theme-files-test]
            [codecloud.io.tree-sitter-test]
            [codecloud.languages.c-family-test]
            [codecloud.languages-test]
            [codecloud.model.complection-test]
            [codecloud.model.config-test]
            [codecloud.model.graph-test]
            [codecloud.model.ignore-test]
            [codecloud.model.ns-form-test]
            [codecloud.model.parse-test]
            [codecloud.model.resolution-test]
            [codecloud.model.workspace-test]
            [codecloud.presentation.arrangement-test]
            [codecloud.presentation.code-colors-test]
            [codecloud.presentation.editing-test]
            [codecloud.presentation.layers-test]
            [codecloud.presentation.presentation-test]
            [codecloud.presentation.ui-state-test]
            [codecloud.presentation.visibility-test]))

(defmethod test/report [:cljs.test/default :end-run-tests] [summary]
  (set! (.-exitCode js/process) (if (test/successful? summary) 0 1)))

(defn main []
  (test/run-all-tests #"codecloud\..*-test"))

(set! *main-cli-fn* main)

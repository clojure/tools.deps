(ns clojure.tools.deps.script.test-parse
  (:require
    [clojure.test :refer [deftest is]]
    [clojure.tools.deps.script.parse :as parse]))

(deftest test-parse-config
  (is (= {:paths ["x"]} (parse/parse-config "{:paths [\"x\"]}")))
  (is (= "deps.edn" (parse/parse-config "deps.edn"))))
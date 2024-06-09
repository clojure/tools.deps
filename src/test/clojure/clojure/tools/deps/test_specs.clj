(ns clojure.tools.deps.test-specs
  (:require
    [clojure.test :refer [deftest is]]
    [clojure.tools.deps.specs :as specs]))


(deftest test-explain-deps
  (let [deps-map {:mvn/repos {"myrepo" {:url "https://repo1.maven.org/maven2/"
                                        :snapshots true}}}]
    (is (= (specs/explain-deps deps-map)
           "Found: true, expected: map?, in: [:mvn/repos \"myrepo\" 1 :snapshots]")))

  (let [deps-map {:tools/usage {:ns-default "some.ns"}}]
    (is (= (specs/explain-deps deps-map)
           "Found: \"some.ns\", expected: simple-symbol?, in: [:tools/usage :ns-default]"))))

(comment
  (test-explain-deps)
  )

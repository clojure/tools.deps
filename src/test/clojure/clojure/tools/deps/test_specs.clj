(ns clojure.tools.deps.test-specs
  (:require
    [clojure.test :refer [deftest is testing]]
    [clojure.tools.deps.specs :as specs]))


(deftest test-explain-deps
  (let [deps-map {:mvn/repos {"myrepo" {:url "https://repo1.maven.org/maven2/"
                                        :snapshots true}}}]
    (is (= (specs/explain-deps deps-map)
           "Found: true, expected: map?, in: [:mvn/repos \"myrepo\" 1 :snapshots]")))

  (let [deps-map {:tools/usage {:ns-default "some.ns"}}]
    (is (= (specs/explain-deps deps-map)
           "Found: \"some.ns\", expected: simple-symbol?, in: [:tools/usage :ns-default]"))))

(deftest empty-nil-deps-is-valid
  (testing "file exists but is empty (nil)"
    (is (specs/valid-deps? nil))))

(deftest TDEPS-238
  (testing "deps are invalid with extra nested vector in :exclusions"
    (let [invalid {:deps
                   {'org.clojure/core.memoize
                    {:mvn/version "1.0.257"
                     :exclusions [['org.clojure/data.priority-map]]}}}]
      (is (not (specs/valid-deps? invalid))))))

(comment
  (test-explain-deps)
  )

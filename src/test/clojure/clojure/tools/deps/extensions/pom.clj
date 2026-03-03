(ns clojure.tools.deps.extensions.pom
  (:require [clojure.test :refer :all]
            [clojure.tools.deps.extensions :as ext]
            [clojure.tools.deps.extensions.maven]
            [clojure.tools.deps.extensions.pom]
            [clojure.tools.deps.util.maven :as maven])
  (:import [java.io File]))

(deftest tdeps-276-build-helper-src-dir
  (let [paths (ext/coord-paths 'foo/foo {:deps/root "test-data/pom-extra-src" :deps/manifest :pom}
                :pom {:mvn/repos maven/standard-repos})]
    (is (contains? (set paths) (.getAbsolutePath (File. "test-data/pom-extra-src/s1"))))
    (is (contains? (set paths) (.getAbsolutePath (File. "test-data/pom-extra-src/s2"))))
    (is (contains? (set paths) (.getAbsolutePath (File. "test-data/pom-extra-src/r1"))))
    (is (contains? (set paths) (.getAbsolutePath (File. "test-data/pom-extra-src/r2"))))))

(comment
  (tdeps-276-build-helper-src-dir)
  )
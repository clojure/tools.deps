(ns clojure.tools.deps.script.test-make-classpath2
  (:require
    [clojure.test :refer [deftest is]]
    [clojure.tools.deps.script.make-classpath2 :as mc]
    [clojure.java.io :as jio]))

(def install-data
  {:paths ["src"]
   :deps {'org.clojure/clojure {:mvn/version "1.10.1"}}
   :aliases {:test {:extra-paths ["test"]}}
   :mvn/repos {"central" {:url "https://repo1.maven.org/maven2/"}
               "clojars" {:url "https://repo.clojars.org/"}}})

(defn submap?
  "Is m1 a subset of m2?"
  [m1 m2]
  (if (and (map? m1) (map? m2))
    (every? (fn [[k v]] (and (contains? m2 k)
                          (submap? v (get m2 k))))
      m1)
    (= m1 m2)))

(deftest outside-project
  (let [{:keys [basis]} (mc/run-core {:install-deps install-data})]
    (is (submap?
          {:libs {'org.clojure/clojure {:mvn/version "1.10.1"}
                  'org.clojure/spec.alpha {:mvn/version "0.2.176"}
                  'org.clojure/core.specs.alpha {:mvn/version "0.2.44"}}}
          basis))))

(deftest in-project
  (let [{:keys [basis]} (mc/run-core {:install-deps install-data
                                      :project-deps {:deps {'org.clojure/clojure {:mvn/version "1.9.0"}}}})]
    (is (submap?
          {:libs {'org.clojure/clojure {:mvn/version "1.9.0"}
                  'org.clojure/spec.alpha {:mvn/version "0.1.143"}
                  'org.clojure/core.specs.alpha {:mvn/version "0.1.24"}}}
          basis))))

;; alias  :e with :extra-deps extends the project deps
(deftest extra-deps
  (let [{:keys [basis]} (mc/run-core {:install-deps install-data
                                      :project-deps {:deps {'org.clojure/clojure {:mvn/version "1.9.0"}}
                                                     :aliases {:e {:extra-deps {'org.clojure/test.check {:mvn/version "0.9.0"}}}}}
                                      :repl-aliases [:e]})]
    (is (submap?
          {:libs {'org.clojure/clojure {:mvn/version "1.9.0"}
                  'org.clojure/spec.alpha {:mvn/version "0.1.143"}
                  'org.clojure/core.specs.alpha {:mvn/version "0.1.24"}
                  'org.clojure/test.check {:mvn/version "0.9.0"}}}
          basis))))

;; alias :t with :replace-deps replaces the project deps
(deftest tool-deps
  (doseq [k [:deps :replace-deps]] ;; FUTURE - remove :deps here (will warn for now)
    (let [{:keys [basis]} (mc/run-core {:install-deps install-data
                                        :project-deps {:deps {'org.clojure/clojure {:mvn/version "1.9.0"}}
                                                       :aliases {:t {k {'org.clojure/test.check {:mvn/version "0.9.0"}}}}}
                                        :repl-aliases [:t]})]
      (is (submap?
            {:paths ["src"]
             :deps {'org.clojure/test.check {:mvn/version "0.9.0"}}
             :libs {'org.clojure/clojure {:mvn/version "1.10.1"}
                    'org.clojure/spec.alpha {:mvn/version "0.2.176"}
                    'org.clojure/core.specs.alpha {:mvn/version "0.2.44"}
                    'org.clojure/test.check {:mvn/version "0.9.0"}}}
            basis)))))

;; alias :o with :override-deps overrides the version to use
(deftest override-deps
  (let [{:keys [basis]} (mc/run-core {:install-deps install-data
                                      :project-deps {:aliases {:o {:override-deps {'org.clojure/clojure {:mvn/version "1.6.0"}}}}}
                                      :repl-aliases [:o]})]
    (is (submap?
          {:libs {'org.clojure/clojure {:mvn/version "1.6.0"}}}
          basis))))

(defn select-cp
  [classpath key]
  (->> classpath (filter #(contains? (val %) key)) (apply conj {})))

;; paths and deps in alias replace
(deftest alias-paths-and-deps
  (doseq [p [:paths :replace-paths] ;; FUTURE - remove :paths, :deps here (will warn for now)
          d [:deps :replace-deps]]
    (let [{:keys [basis]} (mc/run-core {:install-deps install-data
                                        :project-deps {:paths ["a" "b"]
                                                       :aliases {:q {p ["a" "c"]
                                                                     d {'org.clojure/clojure {:mvn/version "1.6.0"}}}}}
                                        :repl-aliases [:q]})]
      (is (submap?
            {:paths ["a" "c"]
             :deps {'org.clojure/clojure {:mvn/version "1.6.0"}}}
            basis))
      (is (= #{"a" "c"} (-> basis :classpath (select-cp :path-key) keys set))))))

;; paths replace in chain
(deftest paths-replace
  (let [{:keys [basis]} (mc/run-core {:install-deps install-data
                                      :user-deps {:paths ["x"]}
                                      :project-deps {:paths ["y"]}
                                      :config-data {:paths ["z"]}})]
    (is (submap? {:paths ["z"]} basis))
    (is (= #{"z"} (-> basis :classpath (select-cp :path-key) keys set)))))

;; :paths in alias replaces, multiple alias :paths will be combined
(deftest alias-paths-replace
  (doseq [p [:paths :replace-paths]] ;; FUTURE - remove :paths here (will warn for now)
    (let [{:keys [basis]} (mc/run-core {:install-deps install-data
                                        :user-deps {:aliases {:p {p ["x" "y"]}}}
                                        :project-deps {:aliases {:q {p ["z"]}}}
                                        :repl-aliases [:p :q]})]
      (is (submap? {:paths ["x" "y" "z"]} basis))
      (is (= #{"x" "y" "z"} (-> basis :classpath (select-cp :path-key) keys set))))))

;; :extra-paths add
(deftest extra-paths-add
  (let [{:keys [basis]} (mc/run-core {:install-deps install-data
                                      :user-deps {:aliases {:p {:extra-paths ["x" "y"]}}}
                                      :project-deps {:aliases {:q {:extra-paths ["z"]}}}
                                      :repl-aliases [:p :q]})]
    (is (submap?
          {:paths ["src"]}
          basis))
    (is (= #{"src" "x" "y" "z"} (-> basis :classpath (select-cp :path-key) keys set)))))

;; java opts in aliases are additive
(deftest jvm-opts-add
  (let [core-ret (mc/run-core {:install-deps install-data
                               :user-deps {:aliases {:j1 {:jvm-opts ["-server" "-Xms100m"]}}}
                               :project-deps {:aliases {:j2 {:jvm-opts ["-Xmx200m"]}}}
                               :repl-aliases [:j1 :j2]})]
    (is (= ["-server" "-Xms100m" "-Xmx200m"] (-> core-ret :basis :argmap :jvm-opts)))))

;; main opts replace
(deftest main-opts-replace
  (let [core-ret (mc/run-core {:install-deps install-data
                               :user-deps {:aliases {:m1 {:main-opts ["a" "b"]}}}
                               :project-deps {:aliases {:m2 {:main-opts ["c"]}}}
                               :repl-aliases [:m1 :m2]})]
    (is (= ["c"] (-> core-ret :basis :argmap :main-opts)))))

;; local manifests returned
(deftest manifest-local
  (let [{:keys [manifests]} (mc/run-core {:install-deps install-data
                                          :project-deps {:deps {'io.github.clojure/data.json {:git/sha "f367490" :git/tag "v2.4.0"}
                                                                'io.github.clojure/data.codec {:git/sha "8ef09db", :git/tag "data.codec-0.1.1", :deps/manifest :pom}}}})]
    ;; returns a manifest for both projects (deps.edn and pom.xml respectively)
    (is (= 2 (count manifests)))))

;; repositories should be retained for generate-manifest2's use
(deftest repo-config-retained
  (let [{:keys [basis]} (mc/run-core {:install-deps install-data})] ;; install-data has central and clojars
    (is (= #{"central" "clojars"} (-> basis :mvn/repos keys set)))))

;; skip-cp flag prevents resolve-deps/make-classpath
(deftest skip-cp-flag
  (let [{:keys [basis]} (mc/run-core {:install-deps install-data
                                      :project-deps {:deps {'org.clojure/clojure {:mvn/version "1.10.0"}}}
                                      :skip-cp true})]
    (is (nil? basis))))

;; skip-cp flag still passes exec-args for -X or -T
(deftest skip-cp-exec
  (let [core-ret (mc/run-core {:install-deps install-data
                               :project-deps {:deps {'org.clojure/clojure {:mvn/version "1.10.0"}}
                                              :aliases {:x {:exec-fn 'clojure.core/prn :exec-args {:a 1}}}}
                               :exec-aliases [:x]
                               :skip-cp true})]
    (is (submap? {:exec-fn 'clojure.core/prn, :exec-args {:a 1}}
                 (-> core-ret :basis :argmap)))))

(deftest removing-deps
  (let [{:keys [basis]} (mc/run-core {:install-deps install-data
                                      :user-deps {:aliases
                                                  {:remove-clojure
                                                   {:classpath-overrides
                                                    '{org.clojure/clojure nil
                                                      org.clojure/spec.alpha nil
                                                      org.clojure/core.specs.alpha nil}}}}
                                      :repl-aliases [:remove-clojure]})
        {:keys [libs classpath-roots classpath]} basis]
    (is (= 3 (count libs))) ;; lib set is not changed by classpath-overrides
    (is (= ["src"] classpath-roots))
    (is (= {"src" {:path-key :paths}} classpath))))

(deftest tool-alias
  (let [{:keys [basis]}
        (mc/run-core {:install-deps install-data
                      :user-deps {:aliases {:t {:extra-deps {'org.clojure/data.json {:mvn/version "2.0.1"}}}}}
                      :project-deps {:deps {'cheshire/cheshire {:mvn/version "5.10.0"}}}
                      :tool-aliases [:t]
                      :tool-mode true})
        {:keys [libs classpath-roots classpath]} basis
        paths (filter #(get-in classpath [% :path-key]) classpath-roots)]
    ;; includes tool dep and not project deps
    (is (contains? libs 'org.clojure/data.json))
    (is (not (contains? libs 'cheshire/cheshire)))
    ;; paths only contains project root dir
    (is (= 1 (count paths)))
    (is (= (.getCanonicalPath (jio/file (first paths))) (.getCanonicalPath (jio/file "."))))))

;; clj -T a/fn
(deftest tool-bare
  (let [{:keys [basis]}
        (mc/run-core {:install-deps install-data
                      :user-deps {}
                      :project-deps {:deps {'cheshire/cheshire {:mvn/version "5.10.0"}}}
                      :tool-mode true})
        {:keys [libs classpath-roots classpath]} basis
        paths (filter #(get-in classpath [% :path-key]) classpath-roots)]
    (is (not (contains? libs 'cheshire/cheshire)))
    (is (= 1 (count paths)))
    (is (= (.getCanonicalPath (jio/file (first paths))) (.getCanonicalPath (jio/file "."))))))

;; clj -Tfoo
(deftest tool-by-name
  (let [{:keys [basis]}
        (mc/run-core {:install-deps install-data
                      :user-deps {}
                      :project-deps {:deps {'cheshire/cheshire {:mvn/version "5.10.0"}}}
                      :tool-mode true
                      :tool-name "foo"
                      :tool-resolver {"foo" {:replace-deps {'org.clojure/data.json {:mvn/version "2.0.1"}}
                                             :replace-paths ["."]
                                             :ns-default 'a.b}}})
        {:keys [libs classpath-roots classpath argmap]} basis
        paths (filter #(get-in classpath [% :path-key]) classpath-roots)]
    ;; execute-args in basis argmap
    (is (= (:ns-default argmap) 'a.b))
    ;; tool deps, not project deps
    (is (not (contains? libs 'cheshire/cheshire)))
    (is (contains? libs 'org.clojure/data.json))
    ;; ., not project paths
    (is (= (map #(.getCanonicalPath (jio/file %)) ["."])
          (map #(.getCanonicalPath (jio/file %)) paths)))))

;; clj -T:a:b
(deftest tool-with-aliases
  (let [{:keys [basis]}
        (mc/run-core {:install-deps install-data
                      :user-deps {}
                      :project-deps {:deps {'cheshire/cheshire {:mvn/version "5.10.0"}}
                                     :aliases {:a {:replace-paths ["x"]}
                                               :b {:replace-paths ["y"]}}}
                      :tool-mode true
                      :tool-aliases [:a :b]})
        {:keys [libs classpath-roots classpath]} basis
        paths (filter #(get-in classpath [% :path-key]) classpath-roots)]
    ;; tool deps, not project deps
    (is (not (contains? libs 'cheshire/cheshire)))
    (is (= (map #(.getCanonicalPath (jio/file %)) ["x" "y" "."])
          (map #(.getCanonicalPath (jio/file %)) paths)))))

(comment
  (clojure.test/run-tests)
)

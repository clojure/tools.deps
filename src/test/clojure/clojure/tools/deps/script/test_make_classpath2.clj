(ns clojure.tools.deps.script.test-make-classpath2
  (:require
    [clojure.test :refer [deftest is]]
    [clojure.tools.deps.script.make-classpath2 :as mc]
    [clojure.java.io :as jio])
  (:import
    [java.io File]))

(defn submap?
  "Is m1 a subset of m2?"
  [m1 m2]
  (if (and (map? m1) (map? m2))
    (every? (fn [[k v]] (and (contains? m2 k)
                          (submap? v (get m2 k))))
      m1)
    (= m1 m2)))

(deftest outside-project
  (let [{:keys [basis]} (mc/run-core {:config-user nil})]
    (is (submap?
          {:libs {'org.clojure/clojure {}
                  'org.clojure/spec.alpha {}
                  'org.clojure/core.specs.alpha {}}}
          basis))))

(deftest in-project
  (let [{:keys [basis]} (mc/run-core {:config-user nil
                                      :config-project {:deps {'org.clojure/clojure {:mvn/version "1.9.0"}}}})]
    (is (submap?
          {:libs {'org.clojure/clojure {:mvn/version "1.9.0"}
                  'org.clojure/spec.alpha {}
                  'org.clojure/core.specs.alpha {}}}
          basis))))

;; alias  :e with :extra-deps extends the project deps
(deftest extra-deps
  (let [{:keys [basis]} (mc/run-core {:config-project {:deps {'org.clojure/clojure {:mvn/version "1.9.0"}}
                                                       :aliases {:e {:extra-deps {'org.clojure/test.check {:mvn/version "0.9.0"}}}}}
                                      :repl-aliases [:e]})]
    (is (submap?
          {:libs {'org.clojure/clojure {:mvn/version "1.9.0"}
                  'org.clojure/spec.alpha {}
                  'org.clojure/core.specs.alpha {}
                  'org.clojure/test.check {:mvn/version "0.9.0"}}}
          basis))))

;; alias :t with :replace-deps replaces the project deps and paths
(deftest tool-deps
  (doseq [k [:deps :replace-deps]] ;; synonyms
    (let [{:keys [basis]} (mc/run-core {:config-project {:deps {'org.clojure/clojure {:mvn/version "1.9.0"}}
                                                         :aliases {:t {k {'org.clojure/test.check {:mvn/version "0.9.0"}}}}}
                                        :tool-aliases [:t]})]
      (is (submap?
            {:paths ["."]
             :deps {'org.clojure/test.check {:mvn/version "0.9.0"}}
             :libs {'org.clojure/test.check {:mvn/version "0.9.0"}}}
            basis)))))

;; simulate named tool
(deftest tool-deps-named
  (doseq [k [:deps :replace-deps]] ;; synonyms
    (let [{:keys [basis]} (mc/run-core {:config-project {:deps {'org.clojure/clojure {:mvn/version "1.9.0"}}}
                                        :tool-data {k {'org.clojure/test.check {:mvn/version "0.9.0"}}
                                                    :replace-paths ["."]}})]
      (is (submap?
            {:paths ["."]
             :deps {'org.clojure/test.check {:mvn/version "0.9.0"}}
             :libs {'org.clojure/test.check {:mvn/version "0.9.0"}}}
            basis)))))

;; alias :o with :override-deps overrides the version to use
(deftest override-deps
  (let [{:keys [basis]} (mc/run-core {:config-project {:aliases {:o {:override-deps {'org.clojure/clojure {:mvn/version "1.6.0"}}}}}
                                      :repl-aliases [:o]})]
    (is (submap?
          {:libs {'org.clojure/clojure {:mvn/version "1.6.0"}}}
          basis))))

(defn select-cp
  [classpath key]
  (->> classpath (filter #(contains? (val %) key)) (apply conj {})))

;; paths and deps in alias replace
(deftest alias-paths-and-deps
  (doseq [p [:paths :replace-paths]
          d [:deps :replace-deps]]
    (let [{:keys [basis]} (mc/run-core {:config-project {:paths ["a" "b"]
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
  (let [{:keys [basis]} (mc/run-core {:config-user {:paths ["x"]}
                                      :config-project {:paths ["y"]}
                                      :config-data {:paths ["z"]}})]
    (is (submap? {:paths ["z"]} basis))
    (is (= #{"z"} (-> basis :classpath (select-cp :path-key) keys set)))))

;; :paths in alias replaces, multiple alias :paths will be combined
(deftest alias-paths-replace
  (doseq [p [:paths :replace-paths]] ;; FUTURE - remove :paths here (will warn for now)
    (let [{:keys [basis]} (mc/run-core {:config-user {:aliases {:p {p ["x" "y"]}}}
                                        :config-project {:aliases {:q {p ["z"]}}}
                                        :repl-aliases [:p :q]})]
      (is (submap? {:paths ["x" "y" "z"]} basis))
      (is (= #{"x" "y" "z"} (-> basis :classpath (select-cp :path-key) keys set))))))

;; :extra-paths add
(deftest extra-paths-add
  (let [{:keys [basis]} (mc/run-core {:config-user {:aliases {:p {:extra-paths ["x" "y"]}}}
                                      :config-project {:aliases {:q {:extra-paths ["z"]}}}
                                      :repl-aliases [:p :q]})]
    (is (submap?
          {:paths ["src"]}
          basis))
    (is (= #{"src" "x" "y" "z"} (-> basis :classpath (select-cp :path-key) keys set)))))

;; java opts in aliases are additive
(deftest jvm-opts-add
  (let [core-ret (mc/run-core {:config-user {:aliases {:j1 {:jvm-opts ["-server" "-Xms100m"]}}}
                               :config-project {:aliases {:j2 {:jvm-opts ["-Xmx200m"]}}}
                               :repl-aliases [:j1 :j2]})]
    (is (= ["-server" "-Xms100m" "-Xmx200m"] (-> core-ret :basis :argmap :jvm-opts)))))

;; main opts replace
(deftest main-opts-replace
  (doseq [a [:repl-aliases :main-aliases]] ;; will WARN for -A, but not for -M
    (let [core-ret (mc/run-core {:config-user {:aliases {:m1 {:main-opts ["a" "b"]}}}
                                 :config-project {:aliases {:m2 {:main-opts ["c"]}}}
                                 a [:m1 :m2]})]
      (is (= ["c"] (-> core-ret :basis :argmap :main-opts))))))

;; local manifests returned
(deftest manifest-local
  (let [{:keys [manifests]} (mc/run-core {:config-project {:deps {'io.github.clojure/data.json {:git/sha "f367490" :git/tag "v2.4.0"}
                                                                  'io.github.clojure/data.codec {:git/sha "8ef09db", :git/tag "data.codec-0.1.1", :deps/manifest :pom}}}})]
    ;; returns a manifest for both projects (deps.edn and pom.xml respectively)
    (is (= 2 (count manifests)))))

;; repositories should be retained for generate-manifest2's use
(deftest repo-config-retained
  (let [{:keys [basis]} (mc/run-core {})] ;; root deps has central and clojars
    (is (= #{"central" "clojars"} (-> basis :mvn/repos keys set)))))

;; skip-cp flag prevents resolve-deps/make-classpath
(deftest skip-cp-flag
  (let [{:keys [basis]} (mc/run-core {:config-project {:deps {'org.clojure/clojure {:mvn/version "1.10.0"}}}
                                      :skip-cp true})]
    (is (= {} (select-keys basis [:libs :classpath :classpath-roots])))))

;; skip-cp flag still passes exec-args for -X or -T
(deftest skip-cp-exec
  (let [core-ret (mc/run-core {:config-project {:deps {'org.clojure/clojure {:mvn/version "1.10.0"}}
                                                :aliases {:x {:exec-fn 'clojure.core/prn :exec-args {:a 1}}}}
                               :exec-aliases [:x]
                               :skip-cp true})]
    (is (submap? {:exec-fn 'clojure.core/prn, :exec-args {:a 1}}
                 (-> core-ret :basis :argmap)))))

(deftest removing-deps
  (let [{:keys [basis]} (mc/run-core {:config-user {:aliases
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
        (mc/run-core {:config-user {:aliases {:t {:extra-deps {'org.clojure/data.json {:mvn/version "2.0.1"}}}}}
                      :config-project {:deps {'cheshire/cheshire {:mvn/version "5.10.0"}}}
                      :tool-aliases [:t]})
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
        (mc/run-core {:config-project {:deps {'cheshire/cheshire {:mvn/version "5.10.0"}}}
                      :tool-data {:replace-paths ["."]}})
        {:keys [libs classpath-roots classpath]} basis
        paths (filter #(get-in classpath [% :path-key]) classpath-roots)]
    (is (not (contains? libs 'cheshire/cheshire)))
    (is (= 1 (count paths)))
    (is (= (.getCanonicalPath (jio/file (first paths))) (.getCanonicalPath (jio/file "."))))))

;; clj -Tfoo
(deftest tool-by-name
  (let [{:keys [basis]}
        (mc/run-core {:config-project {:deps {'cheshire/cheshire {:mvn/version "5.10.0"}}}
                      :tool-data {:replace-deps {'org.clojure/data.json {:mvn/version "2.0.1"}}
                                  :replace-paths ["."]
                                  :ns-default 'a.b}})
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

(deftest tool-by-name-newer-clojure
  (let [{:keys [basis]}
        (mc/run-core {:config-root {:deps {'org.clojure/clojure {:mvn/version "1.9.0"}} :path ["src"]}
                      :config-project {:deps {'cheshire/cheshire {:mvn/version "5.10.0"}}}
                      :tool-data {:replace-deps {'org.clojure/clojure {:mvn/version "1.12.0"}}
                                  :replace-paths ["."]
                                  :ns-default 'a.b}})
        {:keys [libs classpath-roots classpath argmap]} basis
        paths (filter #(get-in classpath [% :path-key]) classpath-roots)]
    ;; execute-args in basis argmap
    (is (= (:ns-default argmap) 'a.b))
    ;; tool deps, not project deps
    (is (not (contains? libs 'cheshire/cheshire)))
    (is (submap? {:deps {'org.clojure/clojure {:mvn/version "1.12.0"}}} basis))))

;; clj -T:a:b
(deftest tool-with-aliases
  (let [{:keys [basis]}
        (mc/run-core {:config-project {:deps {'cheshire/cheshire {:mvn/version "5.10.0"}}
                                       :aliases {:a {:replace-paths ["x"]}
                                                 :b {:replace-paths ["y"]}}}
                      :tool-aliases [:a :b]})
        {:keys [libs classpath-roots classpath]} basis
        paths (filter #(get-in classpath [% :path-key]) classpath-roots)]
    ;; tool deps, not project deps
    (is (not (contains? libs 'cheshire/cheshire)))
    (is (= (map #(.getCanonicalPath (jio/file %)) ["x" "y" "."])
          (map #(.getCanonicalPath (jio/file %)) paths)))))

(deftest config-data
  (let [{:keys [basis]} (mc/run-core {:config-project {:deps {'org.clojure/clojure {:mvn/version "1.12.0"}}}
                                      :config-data {:deps {'org.clojure/data.json {:mvn/version "2.5.0"}}}})]
    (is (contains? (:libs basis) 'org.clojure/data.json))))

(deftest config-data-file
  (let [temp-file (File/createTempFile "deps" ".edn")
        _ (spit temp-file "{:deps {org.clojure/data.json {:mvn/version \"2.5.0\"}}}")
        {:keys [basis]} (mc/run-core {:config-project {:deps {'org.clojure/clojure {:mvn/version "1.12.0"}}}
                                      :config-data (.getAbsolutePath temp-file)})]
    (is (contains? (:libs basis) 'org.clojure/data.json))))

(comment
  (clojure.test/run-tests)
)
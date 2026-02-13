(ns clojure.tools.deps.test-deps
  (:require
    [clojure.java.io :as jio]
    [clojure.test :refer [deftest is are testing]]
    [clojure.tools.deps :as deps]
    [clojure.tools.deps.edn :as depsedn]
    [clojure.tools.deps.util :as util]
    [clojure.tools.deps.extensions :as ext]
    [clojure.tools.deps.extensions.faken :as fkn]
    [clojure.tools.deps.util.dir :as dir]
    [clojure.tools.deps.util.maven :as mvn])
  (:import
    [java.io File]))

(deftest test-find-edn-maps
  (let [edn-maps (deps/find-edn-maps)]
    (is (util/submap?
          {:root-edn (depsedn/root-deps)
           ;:user-edn (depsedn/user-deps)   ;; not necessarily present in CI
           :project-edn {:paths ["src/main/clojure" "src/main/resources"]
                         :deps {}}}
          edn-maps)))

  (let [deps-file (File/createTempFile "tmp" ".edn")
        tmp-dir (.getParentFile deps-file)
        deps {:paths ["x"]}]
    (spit deps-file deps)
    (dir/with-dir tmp-dir
      (let [edn-maps (deps/find-edn-maps (.getCanonicalPath deps-file))]
        (is (util/submap?
              {:root-edn (depsedn/root-deps)
               ;:user-edn (depsedn/user-deps)  ;; not necessarily present in CI
               :project-edn {:paths ["x"]}}
              edn-maps))))))

(def repo
  ;; "real"
  {'org.clojure/clojure {{:fkn/version "1.9.0"}
                         [['org.clojure/spec.alpha {:fkn/version "0.1.124"}]
                          ['org.clojure/core.specs.alpha {:fkn/version "0.1.10"}]]}
   'org.clojure/spec.alpha {{:fkn/version "0.1.124"} nil
                            {:fkn/version "0.1.1"} nil}
   'org.clojure/core.specs.alpha {{:fkn/version "0.1.10"} nil}

   ;; testing various scenarios
   'e1/a {{:fkn/version "1"} [['e1/b {:fkn/version "1"}]
                              ['e1/c {:fkn/version "2"}]]}
   'e1/b {{:fkn/version "1"} [['e1/c {:fkn/version "1"}]]}
   'e1/c {{:fkn/version "1"} nil
          {:fkn/version "2"} nil}
   'opt/a {{:fkn/version "1"} [['opt/b {:fkn/version "1" :optional true}]
                               ['opt/c {:fkn/version "1"}]]}
   'opt/b {{:fkn/version "1"} nil}
   'opt/c {{:fkn/version "1"} nil}})

(deftest test-top-optional-included
  (fkn/with-libs repo
    (is (= (set (keys (deps/resolve-deps {:deps {'opt/b {:fkn/version "1"}}} nil)))
          #{'opt/b}))))

(deftest test-transitive-optional-not-included
  (fkn/with-libs repo
    (is (= (set (keys (deps/resolve-deps {:deps {'opt/a {:fkn/version "1"}}} nil)))
          #{'opt/a 'opt/c}))))

(deftest test-basic-expand
  (fkn/with-libs repo
    (is (= (set (keys (deps/resolve-deps {:deps {'org.clojure/clojure {:fkn/version "1.9.0"}}} nil)))
          #{'org.clojure/clojure 'org.clojure/spec.alpha 'org.clojure/core.specs.alpha}))))

(deftest test-top-dominates
  (fkn/with-libs repo
    ;; dependent dep decides version
    (is (= (-> {:deps {'org.clojure/clojure {:fkn/version "1.9.0"}}}
             (deps/resolve-deps nil)
             (get 'org.clojure/spec.alpha)
             :fkn/version)
          "0.1.124"))

    ;; top dep wins
    (is (= (-> {:deps {'org.clojure/clojure {:fkn/version "1.9.0"}
                       'org.clojure/spec.alpha {:fkn/version "0.1.1"}}}
             (deps/resolve-deps nil)
             (get 'org.clojure/spec.alpha)
             :fkn/version)
          "0.1.1"))))

(deftest test-override-deps
  (fkn/with-libs repo
    ;; override dep wins
    (is (= (-> {:deps {'org.clojure/clojure {:fkn/version "1.9.0"}}}
             (deps/resolve-deps
               {:override-deps {'org.clojure/spec.alpha {:fkn/version "0.1.1"}}})
             (get 'org.clojure/spec.alpha)
             :fkn/version)
           "0.1.1"))))

(deftest test-default-deps
  (fkn/with-libs repo
    ;; default dep wins if none provided
    (is (= (-> {:deps {'org.clojure/clojure nil}}
               (deps/resolve-deps
                {:default-deps
                 {'org.clojure/clojure {:fkn/version "2.0.0"}}})
               (get 'org.clojure/clojure)
               :fkn/version)
           "2.0.0"))))

(defn libs->lib-ver
  [libmap]
  (reduce-kv
    (fn [lib-ver lib coord] (assoc lib-ver (-> lib name keyword) (:fkn/version coord)))
    {} libmap))

;; +a1 -> +b1 -> -c1
;;     -> +c2
(deftest test-dep-choice
  (fkn/with-libs repo
    (is (= (->> (deps/resolve-deps {:deps {'e1/a {:fkn/version "1"}}} nil) libs->lib-ver)
           {:a "1", :b "1", :c "2"}))))

;; -> +a1 -> +d1
;; -> +b1 -> -e1 -> -d2
;; -> +c1 -> +e2
(deftest test-dep-parent-missing
  (fkn/with-libs
    {'ex/a {{:fkn/version "1"} [['ex/d {:fkn/version "1"}]]}
     'ex/b {{:fkn/version "1"} [['ex/e {:fkn/version "1"}]]}
     'ex/c {{:fkn/version "1"} [['ex/e {:fkn/version "2"}]]}
     'ex/d {{:fkn/version "1"} nil
            {:fkn/version "2"} nil}
     'ex/e {{:fkn/version "1"} [['ex/d {:fkn/version "2"}]]
            {:fkn/version "2"} nil}}
    (let [r (->> (deps/resolve-deps {:deps {'ex/a {:fkn/version "1"}
                                            'ex/b {:fkn/version "1"}
                                            'ex/c {:fkn/version "1"}}} nil)
              libs->lib-ver)]
      (is (= r {:a "1", :b "1", :c "1", :d "1", :e "2"})))))

;; +a1 -> +b1 -> +x2 -> +y1
;; +c1 -> -x1 -> -z1
(deftest test-dep-choice2
  (fkn/with-libs
    {'ex/a {{:fkn/version "1"} [['ex/b {:fkn/version "1"}]]}
     'ex/b {{:fkn/version "1"} [['ex/x {:fkn/version "2"}]]}
     'ex/c {{:fkn/version "1"} [['ex/x {:fkn/version "1"}]]}
     'ex/x {{:fkn/version "2"} [['ex/y {:fkn/version "1"}]]
            {:fkn/version "1"} [['ex/z {:fkn/version "1"}]]}
     'ex/y {{:fkn/version "1"} nil}
     'ex/z {{:fkn/version "1"} nil}}
    (is (= {:a "1", :b "1", :c "1", :x "2", :y "1"}
          (let [res (deps/resolve-deps {:deps {'ex/a {:fkn/version "1"}, 'ex/c {:fkn/version "1"}}} nil)]
            (libs->lib-ver res))))))

;; c1 included via both a and b, with exclusions in one branch and without in the other
;; should always include d1
;; +a1 -> +c1 (excl d) -> d1
;; +b1 -> +c1 -> +d1
(deftest test-dep-same-version-different-exclusions
  (fkn/with-libs
    {'ex/a {{:fkn/version "1"} [['ex/c {:fkn/version "1" :exclusions ['ex/d]}]]}
     'ex/b {{:fkn/version "1"} [['ex/c {:fkn/version "1"}]]}
     'ex/c {{:fkn/version "1"} [['ex/d {:fkn/version "1"}]]}
     'ex/d {{:fkn/version "1"} nil}}
    (is (= {:a "1", :b "1", :c "1", :d "1"}
          (libs->lib-ver (deps/resolve-deps {:deps {'ex/a {:fkn/version "1"}, 'ex/b {:fkn/version "1"}}} nil))
          (libs->lib-ver (deps/resolve-deps {:deps {'ex/b {:fkn/version "1"}, 'ex/a {:fkn/version "1"}}} nil))))))

;; +a1 -> +b1 -> -c1 -> a1
;;     -> +c2 -> a1
(deftest test-circular-deps
  (fkn/with-libs {'ex/a {{:fkn/version "1"} [['ex/b {:fkn/version "1"}]
                                             ['ex/c {:fkn/version "2"}]]}
                  'ex/b {{:fkn/version "1"} [['ex/c {:fkn/version "1"}]]}
                  'ex/c {{:fkn/version "1"} [['ex/a {:fkn/version "1"}]]
                         {:fkn/version "2"} [['ex/a {:fkn/version "1"}]]}}
    (is (= {:a "1", :b "1", :c "2"}
           (let [res (deps/resolve-deps {:deps {'ex/a {:fkn/version "1"}}} nil)]
             (libs->lib-ver res))))))

;; +a1 -> -d1 -> -e1
;; +b1 -> +c1 -> +d2
;; e1 is found and selected due to d1, then cut when d2 is found
(deftest test-cut-previously-selected-child
  (fkn/with-libs {'ex/a {{:fkn/version "1"} [['ex/d {:fkn/version "1"}]]}
                  'ex/b {{:fkn/version "1"} [['ex/c {:fkn/version "1"}]]}
                  'ex/c {{:fkn/version "1"} [['ex/d {:fkn/version "2"}]]}
                  'ex/d {{:fkn/version "1"} [['ex/e {:fkn/version "1"}]]
                         {:fkn/version "2"} nil}
                  'ex/e {{:fkn.version "1"} nil}}
    (is (= {:a "1", :b "1", :c "1", :d "2"}
          (let [res (deps/resolve-deps {:deps {'ex/a {:fkn/version "1"}
                                               'ex/b {:fkn/version "1"}}} nil)]
            (libs->lib-ver res))))))

;; +a1 -> -d1 -> -e1 -> -f1
;; +b1 -> +c1 -> +g1 -> +d2 -> +e2
;; e1 is found and selected due to d1, then cut when d2 is found
(deftest test-cut-previously-selected-child-2
  (fkn/with-libs {'ex/a {{:fkn/version "1"} [['ex/d {:fkn/version "1"}]]}
                  'ex/b {{:fkn/version "1"} [['ex/c {:fkn/version "1"}]]}
                  'ex/c {{:fkn/version "1"} [['ex/g {:fkn/version "1"}]]}
                  'ex/d {{:fkn/version "1"} [['ex/e {:fkn/version "1"}]]
                         {:fkn/version "2"} [['ex/e {:fkn/version "2"}]]}
                  'ex/e {{:fkn/version "1"} [['ex/f {:fkn/version "1"}]]
                         {:fkn/version "2"} nil}
                  'ex/f {{:fkn/version "1"} nil}
                  'ex/g {{:fkn/version "1"} [['ex/d {:fkn/version "2"}]]}}
    (is (= {:a "1", :b "1", :c "1", :d "2", :e "2", :g "1"}
          (let [res (deps/resolve-deps {:deps {'ex/a {:fkn/version "1"}
                                               'ex/b {:fkn/version "1"}}} nil)]
            (libs->lib-ver res))))))

;; +a1 -> -h1 -> -c1 -> -d1
;; +b1 -> +e1 -> +c1 -> +d1
;;            -> +h2 -> c1 -> d1
;; h2 supersedes previous h1, but need to ensure d1 is included via c1 somewhere
(deftest test-cut-previously-selected-child-3
  (fkn/with-libs {'ex/a {{:fkn/version "1"} [['ex/h {:fkn/version "1"}]]}
                  'ex/b {{:fkn/version "1"} [['ex/e {:fkn/version "1"}]]}
                  'ex/c {{:fkn/version "1"} [['ex/d {:fkn/version "1"}]]}
                  'ex/d {{:fkn/version "1"} nil}
                  'ex/e {{:fkn/version "1"} [['ex/c {:fkn/version "1"}] ['ex/h {:fkn/version "2"}]]}
                  'ex/h {{:fkn/version "1"} [['ex/c {:fkn/version "1"}]]
                         {:fkn/version "2"} [['ex/c {:fkn/version "1"}]]}}
    (is (= {:a "1", :b "1", :c "1", :d "1", :e "1", :h "2"}
          (let [res (deps/resolve-deps {:deps {'ex/a {:fkn/version "1"}
                                               'ex/b {:fkn/version "1"}}} nil)]
            (libs->lib-ver res))))))

;; +a -> +b -> -x2 -> -y2 -> -z2
;;    -> +c -> +d -> +x3 -> +y2 -> +z2
;;    -> -x1 -> -y1 -> -z1
;; include all of x3/y3/z3
(deftest test-multi-version-discovery
  (fkn/with-libs {'ex/a {{:fkn/version "1"} [['ex/b {:fkn/version "1"}]
                                             ['ex/c {:fkn/version "1"}]
                                             ['ex/x {:fkn/version "1"}]]}
                  'ex/b {{:fkn/version "1"} [['ex/x {:fkn/version "2"}]]}
                  'ex/c {{:fkn/version "1"} [['ex/d {:fkn/version "1"}]]}
                  'ex/d {{:fkn/version "1"} [['ex/x {:fkn/version "3"}]]}
                  'ex/x {{:fkn/version "1"} [['ex/y {:fkn/version "1"}]]
                         {:fkn/version "2"} [['ex/y {:fkn/version "2"}]]
                         {:fkn/version "3"} [['ex/y {:fkn/version "2"}]]}
                  'ex/y {{:fkn/version "1"} [['ex/z {:fkn/version "1"}]]
                         {:fkn/version "2"} [['ex/z {:fkn/version "2"}]]}
                  'ex/z {{:fkn/version "1"} nil
                         {:fkn/version "2"} nil}}
    (is (= {:a "1", :b "1", :c "1", :d "1", :x "3", :y "2", :z "2"}
          (let [res (deps/resolve-deps {:deps {'ex/a {:fkn/version "1"}}} nil)]
            (libs->lib-ver res))))))

(def ^:dynamic ^File *test-dir* nil)

(defmacro with-test-dir
  [& body]
  `(let [name# (-> clojure.test/*testing-vars* last symbol str)
         dir# (jio/file "test-out" name#)]
     (.delete dir#)
     (.mkdirs dir#)
     (binding [*test-dir* dir#]
       ~@body)))

(deftest test-local-root
  (with-test-dir
    (let [base (.getCanonicalFile *test-dir*)]
      (jio/make-parents *test-dir* "b/deps.edn")
      (testing "a relative local root canonicalizes relative to parent dep"
        (binding [dir/*the-dir* base]
          (is (= ['ex/b {:local/root (.getPath (.getCanonicalFile (File. base "b")))}]
                 (ext/canonicalize 'ex/b {:local/root "b"} {})))))
      (testing "an absolute local root canonicalizes to itself"
        (binding [dir/*the-dir* base]
          (let [abs-b (.getAbsolutePath (jio/file *test-dir* "b"))]
            (is (= ['ex/b {:local/root abs-b}]
                   (ext/canonicalize 'ex/b {:local/root abs-b} {})))))))))

(deftest test-local-root-relative-to-project-deps
  (with-test-dir
    (let [base (.getCanonicalFile *test-dir*)
          adeps (.getPath (jio/file base "a/deps.edn"))
          bdeps (.getPath (jio/file base "b/deps.edn"))
          adir (.getAbsolutePath (jio/file base "a"))
          bdir (.getAbsolutePath (jio/file base "b"))]
      (jio/make-parents *test-dir* "a/deps.edn")
      (jio/make-parents *test-dir* "b/deps.edn")
      (spit adeps "{:deps {b/b {:local/root \"../b\"}}}")
      (spit bdeps "{:paths [\"src\"]}")
      (let [abasis (deps/create-basis {:user nil :dir adir})]
        (is (contains? (:classpath abasis) (.getAbsolutePath (jio/file bdir "src"))))))))

;; simple check that pom resolution is working - load tda itself as pom dep
(deftest test-local-pom
  (is (seq (deps/resolve-deps
             {:deps {'c/tda {:local/root "." :deps/manifest :pom}}
              :mvn/repos mvn/standard-repos}
             nil))))

(def install-data
  {:paths ["src"]
   :deps {'org.clojure/clojure {:mvn/version "1.10.1"}}
   :aliases {:test {:extra-paths ["test"]}}
   :mvn/repos {"central" {:url "https://repo1.maven.org/maven2/"}
               "clojars" {:url "https://repo.clojars.org/"}}})

(deftest calc-basis
  (let [{:keys [libs classpath] :as basis} (deps/calc-basis install-data)]
    ;; basis is superset of merged deps
    (is (= install-data (select-keys basis (keys install-data))))

    ;; lib map contains transitive deps
    (is (= (set (keys libs)) #{'org.clojure/clojure 'org.clojure/spec.alpha 'org.clojure/core.specs.alpha}))

    ;; classpath has all deps and src path
    (is (= (set (vals classpath)) #{{:lib-name 'org.clojure/clojure}
                                    {:lib-name 'org.clojure/spec.alpha}
                                    {:lib-name 'org.clojure/core.specs.alpha}
                                    {:path-key :paths}}))
    (is (contains? (set (keys classpath)) "src"))))

(defn select-cp
  [classpath key]
  (->> classpath (filter #(contains? (val %) key)) (apply conj {})))

(deftest calc-basis-extra-deps
  (let [ra {:extra-deps {'org.clojure/tools.deps.alpha {:mvn/version "0.8.677"}}}
        {:keys [libs]} (deps/calc-basis install-data {:resolve-args ra})
        expanded-deps (-> libs keys set)]
    ;; libs has extra deps and transitive deps
    (is (< 4 (count expanded-deps)))
    (is (contains? expanded-deps 'org.clojure/tools.deps.alpha))))

(deftest calc-basis-override-deps
  (let [ra {:extra-deps {'org.clojure/clojure {:mvn/version "1.6.0"}}}
        {:keys [libs]} (deps/calc-basis install-data {:resolve-args ra})]
    ;; libs has extra deps and transitive deps
    (is (= (get-in libs ['org.clojure/clojure :mvn/version]) "1.6.0"))))

(deftest calc-basis-extra-paths
  (let [cpa {:extra-paths ["x" "y"]}
        {:keys [classpath]} (deps/calc-basis install-data {:classpath-args cpa})]
    ;; classpath has extra paths
    (is (= {"src" {:path-key :paths}, "x" {:path-key :extra-paths}, "y" {:path-key :extra-paths}}
          (select-cp classpath :path-key)))))

(deftest calc-basis-classpath-overrides
  (let [cpa {:classpath-overrides {'org.clojure/clojure "foo"}}
        {:keys [classpath]} (deps/calc-basis install-data {:classpath-args cpa})]
    ;; classpath has replaced path
    (is (= (get classpath "foo") {:lib-name 'org.clojure/clojure}))))

(deftest optional-deps-included
  (let [master-edn (merge install-data
                     '{:deps {org.clojure/clojure {:mvn/version "1.10.1"}
                              org.clojure/core.async {:mvn/version "1.1.587" :optional true}}})
        {:keys [libs]} (deps/calc-basis master-edn)]

    ;; libs contains optional dep
    (is (= (get-in libs ['org.clojure/core.async :mvn/version]) "1.1.587"))))

;(update-excl [lib use-coord coord-id use-path include reason exclusions cut])
(deftest test-update-excl
  ;; new lib/version, no exclusions
  (let [ret (#'deps/update-excl 'a {:mvn/version "1"} {:mvn/version "1"} '[b a] true :new-version nil nil)]
    (is (= {:exclusions' nil, :cut' nil}
          (select-keys ret [:exclusions' :cut'])))
    (is (not (nil? (:child-pred ret)))))

  ;; new lib/version, with exclusions
  (let [ret (#'deps/update-excl 'a {:mvn/version "1" :exclusions ['c]} {:mvn/version "1"} '[b a] true :new-version nil nil)]
    (is (= {:exclusions' '{[b a] #{c}}, :cut' '{[a {:mvn/version "1"}] #{c}}}
          (select-keys ret [:exclusions' :cut'])))
    (is (not (nil? (:child-pred ret)))))

  ;; same lib/version, fewer excludes
  ;; a (excl c)
  ;; b -> a -> c1
  (let [excl '{[a] #{c}}
        ret (#'deps/update-excl 'a {:mvn/version "1"} {:mvn/version "1"} '[b a] false :same-version
              excl '{[a {:mvn/version "1"}] #{c}})]
    (is (= {:exclusions' excl, :cut' '{[a #:mvn{:version "1"}] nil}} (select-keys ret [:exclusions' :cut']))) ;; remove cut
    (let [pred (:child-pred ret)]
      (is (true? (boolean (pred 'c))))))

  ;; same lib/version, subset excludes
  ;; a (excl c d)
  ;; b -> a (excl c)
  (let [excl '{[a] #{c d}}
        cut '{[a {:mvn/version "1"}] #{c d}}
        ret (#'deps/update-excl 'a '{:mvn/version "1" :exclusions [c]} {:mvn/version "1"} '[b a] false
              :same-version excl cut)]
    (is (= {:exclusions' '{[a] #{c d}, [b a] #{c}}, :cut' '{[a {:mvn/version "1"}] #{c}}}
          (select-keys ret [:exclusions' :cut'])))
    (let [pred (:child-pred ret)]
      (is (false? (boolean (pred 'c)))) ;; already enqueued
      (is (true? (boolean (pred 'd)))))) ;; newly enqueueable due to smaller exclusion set

  ;; same lib/version, same excludes
  ;; a (excl c)
  ;; b -> a (excl c)
  (let [excl '{[a] #{c}}
        cut '{[a {:mvn/version "1"}] #{c}}
        ret (#'deps/update-excl 'a {:mvn/version "1" :exclusions ['c]} {:mvn/version "1"} '[b a] false :same-version excl cut)]
    (is (= {:exclusions' (assoc excl '[b a] '#{c}), :cut' cut} (select-keys ret [:exclusions' :cut']))) ;; no change in cut
    (let [pred (:child-pred ret)]
      (is (false? (boolean (pred 'c))))))

  ;; same lib/version, more excludes
  ;; a (excl c)
  ;; b -> a (excl c, d)
  (let [excl '{[a] #{c}}
        cut '{[a {:mvn/version "1"}] #{c}}
        ret (#'deps/update-excl 'a '{:mvn/version "1" :exclusions [c d]} {:mvn/version "1"} '[b a] false :same-version excl cut)]
    (is (= {:exclusions' '{[a] #{c}, [b a] #{c d}}, :cut' cut} (select-keys ret [:exclusions' :cut']))) ;; no change in cut
    (let [pred (:child-pred ret)] ;; c excluded in both, but re-enqueue d - always intersection
      (is (false? (boolean (pred 'c))))
      ;;(is (true? (boolean (pred 'd))))
      )))

;; +x1 -> -a1 -> +b2
;; +z1 -> +y1 -> +a2 -> -b1 (or +b1, but at least a consistent result)
;; TDEPS-58
(deftest test-dep-ordering
  (fkn/with-libs
    {'ex/a {{:fkn/version "1"} [['ex/b {:fkn/version "2"}]]
            {:fkn/version "2"} [['ex/b {:fkn/version "1"}]]}
     'ex/b {{:fkn/version "1"} nil
            {:fkn/version "2"} nil}
     'ex/x {{:fkn/version "1"} [['ex/a {:fkn/version "1"}]]}
     'ex/y {{:fkn/version "1"} [['ex/a {:fkn/version "2"}]]}
     'ex/z {{:fkn/version "1"} [['ex/y {:fkn/version "1"}]]}}
    (is (= (let [res (deps/resolve-deps {:deps {'ex/x {:fkn/version "1"}, 'ex/z {:fkn/version "1"}}} nil)]
             (reduce-kv #(assoc %1 (-> %2 name keyword) (:fkn/version %3)) {} res))
           (let [res (deps/resolve-deps {:deps {'ex/z {:fkn/version "1"}, 'ex/x {:fkn/version "1"}}} nil)]
             (reduce-kv #(assoc %1 (-> %2 name keyword) (:fkn/version %3)) {} res))))))

(deftest test-create-basis
  (fkn/with-libs
    {'ex/a {{:fkn/version "1"} nil}
     'ex/b {{:fkn/version "1"} nil}
     'ex/c {{:fkn/version "1"} nil}}
    (are [libs params] (= (set libs) (set (keys (:libs (deps/create-basis params)))))
      ;; check pulling project sources
      [] {:root nil :user nil :project nil}
      ['ex/a] {:root {:deps {'ex/a {:fkn/version "1"}}} :user nil :project nil}
      ['ex/a] {:root nil :user {:deps {'ex/a {:fkn/version "1"}}} :project nil}
      ['ex/a] {:root nil :user nil :project {:deps {'ex/a {:fkn/version "1"}}}}
      ['ex/a] {:root nil :user nil :project nil :extra {:deps {'ex/a {:fkn/version "1"}}}}

      ;; check aliases
      ['ex/a 'ex/b] {:root nil
                     :project {:deps {'ex/a {:fkn/version "1"}}
                               :aliases {:b {:extra-deps {'ex/b {:fkn/version "1"}}}}}
                     :aliases [:b]})))

;; focus not on merging but on verifying output from single deps.edn + aliases
(deftest test-create-basis-full
  (fkn/with-libs
    {'ex/a {{:fkn/version "1"} nil}
     'ex/b {{:fkn/version "1"} nil}
     'ex/c {{:fkn/version "1"} nil}}

    (are [project aliases expected]
         (let [basis (deps/create-basis {:root nil :user nil :project project :aliases aliases})]
           ;;(println "\nbasis:")
           ;;(clojure.pprint/pprint basis)
           (and (util/submap? project basis)  ;; basis is superset of deps.edn
                (util/submap? expected basis)))

      ;; no aliases, basis deps case
      {:deps {'ex/a {:fkn/version "1"}}}
      []
      {:libs {'ex/a {:fkn/version "1", :deps/manifest :fkn, :paths ["REPO/ex/a/1/a-1.jar"]}}
       :classpath {"REPO/ex/a/1/a-1.jar" {:lib-name 'ex/a}}
       :classpath-roots ["REPO/ex/a/1/a-1.jar"]}

      ;; aliases to add both resolve-args and classpath-args
      {:deps {'ex/a {:fkn/version "1"}}
       :aliases {:a1 {:extra-deps {'ex/b {:fkn/version "2"}} ;; mixture of resolve-deps/make-cp args
                      :extra-paths ["p"]}
                 :a2 {:override-deps {'ex/b {:fkn/version "1"}}
                      :default-deps {'ex/c {:fkn/version "1"}}
                      :classpath-overrides {'ex/a "override.jar"}}}}
      [:a1 :a2]
      {:libs {'ex/a {:fkn/version "1", :deps/manifest :fkn, :paths ["REPO/ex/a/1/a-1.jar"]}
              'ex/b {:fkn/version "1", :deps/manifest :fkn, :paths ["REPO/ex/b/1/b-1.jar"]}}
       :classpath {"override.jar" {:lib-name 'ex/a}  ;; see override
                   "REPO/ex/b/1/b-1.jar" {:lib-name 'ex/b}
                   "p" {:path-key :extra-paths}}  ;; see extra path
       :classpath-roots ["p" "override.jar" "REPO/ex/b/1/b-1.jar"]  ;; ditto above
       :basis-config {:root nil, :user nil, :aliases [:a1 :a2]} ;; aliases remembered
       })))

(deftest test-project-deps-doesnt-exist
  (is (= "BOGUS.edn" (-> (deps/create-basis {:user nil :project "BOGUS.edn"})
                       :basis-config :project)))
  (is (= "BOGUS.edn" (-> (deps/create-basis {:user nil :dir "foo" :project "BOGUS.edn"})
                           :basis-config :project)))
  (is (= "foo/BOGUS.edn" (-> (deps/create-basis {:user nil :project "foo/BOGUS.edn"})
                       :basis-config :project))))

(deftest test-resolved-added-libs
  (let [basis (deps/create-basis {:user nil :project nil})
        libs (:libs basis)]
    (are [add] (contains? (:added (deps/resolve-added-libs {:existing libs, :procurer basis, :add add}))
                          (key (first add)))
      '{org.clojure/tools.cli {:mvn/version "1.0.214"}}
      '{io.github.clojure/tools.deps.graph {:git/tag "v1.1.76" :git/sha "6c58e98"}}
      '{org.clojure/tools.deps {:local/root "."}})))

(deftest test-find-all-versions
  (is (seq (ext/find-all-versions 'ragtime nil {:mvn/repos mvn/standard-repos}))))

(comment
  (test-resolved-added-libs)
  (test-find-all-versions)
  )

(ns clojure.tools.deps.test-edn
  (:require
    [clojure.test :refer :all]
    [clojure.tools.deps.edn :as depsedn])
  (:import
    [java.io File]))

(deftest test-read-deps-on-nonexistent-file
  (is (nil? (depsedn/read-deps (File. "NONEXISTENT_FILE")))))

(deftest test-merge-or-replace
  (are [vals ret]
    (= ret (apply #'depsedn/merge-or-replace vals))

    [nil nil] nil
    [nil {:a 1}] {:a 1}
    [{:a 1} nil] {:a 1}
    [{:a 1 :b 1} {:a 2 :c 3} {:c 4 :d 5}] {:a 2 :b 1 :c 4 :d 5}
    [nil 1] 1
    [1 nil] 1
    [1 2] 2))

(deftest test-merge-edns
  (is (= (depsedn/merge-edns
           [{:deps {'a {:v 1}, 'b {:v 1}}
             :a/x {:a 1}
             :a/y "abc"}
            {:deps {'b {:v 2}, 'c {:v 3}}
             :a/x {:b 1}
             :a/y "def"}
            nil])
        {:deps {'a {:v 1}, 'b {:v 2}, 'c {:v 3}}
         :a/x {:a 1, :b 1}
         :a/y "def"})))

(deftest merge-alias-maps
  (are [m1 m2 out]
    (= out (#'depsedn/merge-alias-maps m1 m2))

    {} {} {}
    {} {:extra-deps {:a 1}} {:extra-deps {:a 1}}
    {:extra-deps {:a 1 :b 1}} {:extra-deps {:b 2}} {:extra-deps {:a 1 :b 2}}
    {} {:default-deps {:a 1}} {:default-deps {:a 1}}
    {:default-deps {:a 1 :b 1}} {:default-deps {:b 2}} {:default-deps {:a 1 :b 2}}
    {} {:override-deps {:a 1}} {:override-deps {:a 1}}
    {:override-deps {:a 1 :b 1}} {:override-deps {:b 2}} {:override-deps {:a 1 :b 2}}
    {} {:extra-paths ["a" "b"]} {:extra-paths ["a" "b"]}
    {:extra-paths ["a" "b"]} {:extra-paths ["c" "d"]} {:extra-paths ["a" "b" "c" "d"]}
    {} {:jvm-opts ["-Xms100m" "-Xmx200m"]} {:jvm-opts ["-Xms100m" "-Xmx200m"]}
    {:jvm-opts ["-Xms100m" "-Xmx200m"]} {:jvm-opts ["-Dfoo=bar"]} {:jvm-opts ["-Xms100m" "-Xmx200m" "-Dfoo=bar"]}
    {} {:main-opts ["foo.bar" "1"]} {:main-opts ["foo.bar" "1"]}
    {:main-opts ["foo.bar" "1"]} {:main-opts ["foo.baz" "2"]} {:main-opts ["foo.baz" "2"]}))

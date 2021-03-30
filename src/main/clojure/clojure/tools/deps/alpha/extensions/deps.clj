;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{:skip-wiki true}
  clojure.tools.deps.alpha.extensions.deps
  (:require
    [clojure.java.io :as jio]
    [clojure.tools.deps.alpha :as deps]
    [clojure.tools.deps.alpha.extensions :as ext]
    [clojure.tools.deps.alpha.util.dir :as dir]
    [clojure.tools.deps.alpha.util.io :as io]))

(defn- deps-map
  [config dir]
  (let [f (jio/file dir "deps.edn")]
    (if (.exists f)
      (deps/merge-edns [(deps/root-deps) (deps/slurp-deps f)])
      (deps/root-deps))))

(defmethod ext/coord-deps :deps
  [_lib {:keys [deps/root] :as _coord} _mf config]
  (dir/with-dir (jio/file root)
    (seq (:deps (deps-map config root)))))

(defmethod ext/coord-paths :deps
  [_lib {:keys [deps/root] :as _coord} _mf config]
  (dir/with-dir (jio/file root)
    (->> (:paths (deps-map config root))
      (map #(dir/canonicalize (jio/file %)))
      (map #(do
              (when (not (dir/sub-path? %))
                (io/printerrln "WARNING: Specified path" % "is external to project" root))
              %))
      (map #(.getCanonicalPath %))
      vec)))


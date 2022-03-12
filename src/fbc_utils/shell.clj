(ns fbc-utils.shell
  (:require [clojure.java.shell :as sh]
            [clojure.core.async :as as]
            [fbc-utils.core :as ut]))

(def shell-definitions {})

(defn apply-shell-defs [s]
  (reduce (fn [acc [k v :as item]]
            (clojure.string/replace acc (re-pattern (str "\\{" (name k) "\\}")) v))
          s
          shell-definitions))

(defn ! [s]
  (let [s                                 (apply-shell-defs s)
        {:keys [out exit err] :as result} (sh/sh "bash" "-c" s)]
    (when-not (zero? exit)
      (ut/throw (str "exit failed: " err)))
    out))

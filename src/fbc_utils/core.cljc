(ns fbc-utils.core
  (:require [clojure.spec.alpha :as s]
            [clojure.set]))

(defmacro defmethod-group [fname keys & body]
  `(do ~@(for [key keys]
           `(defmethod ~fname ~key
              ~@body))))

(defmacro closed-map
  ([req-keys opt-keys]
   `(clojure.spec.alpha/merge (clojure.spec.alpha/keys :req ~(vec req-keys)) (clojure.spec.alpha/map-of ~(clojure.set/union req-keys opt-keys) any?)))
  ([req-keys]
   `(closed-map ~req-keys #{})))

(defmacro for-async [[item items] & body]
  `(loop [items# ~items
          acc#   []]
     (if (seq items#)
       (recur (rest items#)
              (conj acc#
                    (let [~item (first items#)]
                      ~@body)))
       acc#)))

(defn extract-opts [args]
  (if (and (seq args) (map? (first args)))
    [(first args) (rest args)]
    [{} args]))

(defn date->unix-time [date]
  (int (/ (.getTime date) 1000)))

(defn validate-atom [spec state-atom]
  (set-validator! state-atom
                  (fn [k]
                    (if (s/valid? spec k)
                      true
                      (throw (ex-info (str (s/explain-str spec k) "\n" (pr-str k)) {}))))))

#?(:cljs (do (defn get-tick-count []
               (.getTime (js/Date.)))

             (defn log [& args]
               (.apply (.-log js/console)
                       js/console
                       (apply array
                              args)))))

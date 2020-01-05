(ns fbc-utils.cupdate
  (:require [fbc-utils.debug :refer [??]])
  (:refer-clojure :exclude [type]))

(defn type [k]
  (cond (map? k)  :map
        (coll? k) :coll
        (fn? k)   :fn
        :else     :default))

(defmulti cupdate
  (fn ([x y & xs] 
       (mapv type (into [x y] xs)))))
 
(defmethod cupdate [:map :map]
  [coll upd]
  (reduce (fn [acc [k v :as item]]
            (cond (= k ::every)  acc
                  (= v ::delete) (dissoc acc k)
                  :else          (update acc
                                         k
                                         (fn [g]
                                           (cupdate g v)))))
          (if-let [g (upd ::every)]
            (into {}
                  (for [[k v] coll]
                    [k (cupdate v g)]))
            coll)
          upd))

(defmethod cupdate [:map :coll]
  [coll upd]
  (cond (= (count upd) 1) (into {}
                                (map #(cupdate % (first upd)) coll))
        (= (count upd) 2) (into {}
                                (for [[k v] coll]
                                  [(cupdate k (first upd)) (cupdate v (second upd))]))))

(defmethod cupdate [:coll :map]
  [coll upd]
  (reduce (fn [acc [k v :as item]]
            (update acc
                    k
                    (fn [g]
                      (cupdate g v))))
          coll
          upd))

(defmethod cupdate [:default :fn]
  [coll upd]
  (upd coll))

(defmethod cupdate [:coll :coll]
  [coll upd]
  (cond-> (cond (= (count upd) 1)            (map #(cupdate % (first upd)) coll)
                (= (count coll) (count upd)) (map cupdate coll upd)
                :else                        upd)
    (vector? coll) vec))

(defmethod cupdate [:default :default]
  [coll upd]
  upd)

(defmethod cupdate [:coll :fn]
  [coll upd]
  (upd coll))

(defmethod cupdate [:map :fn]
  [coll upd]
  (upd coll))

(defmethod cupdate [:default :coll]
  [coll upd]
  upd)

(defmethod cupdate [:default :map]
  [coll upd]
  (if coll
    (throw (ex-info (str "Can't cupdate map " upd " to atomic value " coll) {}))
    (cupdate {} upd)))

(defmethod cupdate [:map :default]
  [coll upd]
  (if (keyword? upd)
    (coll upd)
    (throw (ex-info (str "Can't cupdate arbitrary value " upd " against map " coll) {}))))

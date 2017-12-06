(ns fbc-utils.core)

(defmacro defmethod-group [fname keys & body]
  `(do ~@(for [key keys]
           `(defmethod ~fname ~key
              ~@body))))

(defmacro closed-map
  ([req-keys opt-keys]
   `(clojure.spec/merge (clojure.spec/keys :req ~(vec req-keys)) (clojure.spec/map-of ~(st/union req-keys opt-keys) any?)))
  ([req-keys]
   `(closed-map ~req-keys {})))

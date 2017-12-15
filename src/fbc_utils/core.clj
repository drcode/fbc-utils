(ns fbc-utils.core)

(defmacro defmethod-group [fname keys & body]
  `(do ~@(for [key keys]
           `(defmethod ~fname ~key
              ~@body))))

(defmacro closed-map
  ([req-keys opt-keys]
   `(clojure.spec.alpha/merge (clojure.spec.alpha/keys :req ~(vec req-keys)) (clojure.spec.alpha/map-of ~(clojure.set/union req-keys opt-keys) any?)))
  ([req-keys]
   `(closed-map ~req-keys {})))

(defn extract-opts [args]
  (if (and (seq args) (map? (first args)))
    [(first args) (rest args)]
    [{} args]))


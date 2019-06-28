(ns fbc-utils.test
  (:refer-clojure :exclude [test]))

(defmacro test-helper [& body]
  (if-let [[a & more] (seq body)]
    (let [start (seq (drop-while (fn [k]
                                   (not= k 'start))
                                 more))]
      (cond start       `(test-helper ~@(rest start))
            (= a 'stop) nil
            (= a '-)    `(test-helper ~@(rest (rest more)))
            (seq more)  `(do (clojure.test/testing '~a (clojure.test/is (= ~(first more) ~a)))
                             (test-helper ~@(rest more)))
            :else       `(clojure.test/testing '~a (clojure.test/is (= :no-result ~a)))))))

(defmacro test [& body]
  `(clojure.test/deftest ~'tests
     (test-helper ~@body)))

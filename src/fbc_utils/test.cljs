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
            (seq more)  `(do (cljs.test/testing '~a (cljs.test/is (= ~(first more) ~a)))
                             (test-helper ~@(rest more)))
            :else       `(cljs.test/testing '~a (cljs.test/is (= :no-result ~a)))))))

(defmacro test [& body]
  `(cljs.test/deftest ~'tests
     (test-helper ~@body)))

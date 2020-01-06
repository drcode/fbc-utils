(ns fbc-utils.test
  (:require #?(:clj  [clojure.test]
               :cljs [cljs.test :include-macros true]))
  (:refer-clojure :exclude [test]))

(defn- cljs-env?
  "Take the &env from a macro, and tell whether we are expanding into cljs."
  [env]
  (boolean (:ns env)))

(defmacro if-cljs
  "Return then if we are generating cljs code and else for Clojure code.
   https://groups.google.com/d/msg/clojurescript/iBY5HaQda4A/w1lAQi9_AwsJ"
  [then else]
  (if (cljs-env? &env) then else))

(defmacro is [& args]
  `(if-cljs
       (cljs.test/is ~@args)
     (clojure.test/is ~@args)))

(defmacro testing [& args]
  `(if-cljs
       (cljs.test/testing ~@args)
     (clojure.test/testing ~@args)))

(defmacro deftest [& args]
  `(if-cljs
       (cljs.test/deftest ~@args)
     (clojure.test/deftest ~@args)))

(defmacro test-helper [& body]
  (if-let [[a & more] (seq body)]
    (let [start (seq (drop-while (fn [k]
                                   (not= k 'start))
                                 more))]
      (cond start       `(test-helper ~@(rest start))
            (= a 'stop) nil
            (= a '-)    `(test-helper ~@(rest (rest more)))
            (seq more)  `(do (testing '~a (is (= ~(first more) ~a)))
                             (test-helper ~@(rest more)))
            :else       `(testing '~a (is (= :no-result ~a)))))))

(defmacro test [& body]
  `(deftest ~'tests
     (test-helper ~@body)))




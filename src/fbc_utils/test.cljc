(ns fbc-utils.test
  #?(:clj  (:require [clojure.test] [clojure.string :as st])
     :cljs (:require [cljs.test :include-macros true]))
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

(defn test-helper [body desc]
  (if-let [[a & more] (seq body)]
    (cond (= a 'stop)                                                 nil
          (string? a)                                                 (test-helper more a)
          (and (coll? a) (seq a) (= (first a) 'clojure.core/unquote)) `(do ~(second a)
                                                                           ~(test-helper more desc)) 
          (and (seq more) (not= (first more) 'stop))                  `(do (testing '~(or desc a) (is (= ~(first more) ~a)))
                                                                           ~(test-helper (rest more) nil))
          :else                                                       `(testing '~(or desc a) (is (= :no-result ~a))))))

(defmacro test [& body]
  (let [start (seq (drop-while (fn [k]
                                 (not= k 'start))
                               body))]
    `(deftest ~'tests
       ~(test-helper (or (when start
                           (rest start))
                         body) nil))))

;; (defmacro test-file [& body]
;;   (let [start (seq (drop-while (fn [k]
;;                                  (not= k 'start))
;;                                body))]
;;     `(deftest ~'tests
;;        (spit "test_output.edn" "loading...")
;;        (let [err# (atom nil)]
;;          (do (spit "test_output.edn" (with-out-str (try ~(test-helper (or (when start
;;                                                                             (rest start))
;;                                                                           body) nil)
;;                                                         (catch Exception e#
;;                                                           (reset! err# e#)
;;                                                           nil))))
;;              (when @err#
;;                (throw @err#)))))))

(def abort-id "sadiiiiiew53")

(def was-aborted (atom nil))

(defn abort-output []
  (println abort-id)
  (reset! was-aborted true))

(defmacro test-file [& body]
  (let [start (seq (drop-while (fn [k]
                                 (not= k 'start))
                               body))]
    `(deftest ~'tests
       (spit "test_output.edn" "loading...")
       (reset! was-aborted false)
       (let [err# (atom nil)
             s#    (with-out-str (try ~(test-helper (or (when start
                                                          (rest start))
                                                        body)
                                                    nil)
                                      (catch Exception e#
                                        (reset! err# e#))))]
         (spit "test_output.edn"
               (if @was-aborted
                 (first (st/split s# (re-pattern abort-id)))
                 s#))
         (when @err#
           (throw @err#))))))

(defmacro error-name [& body]
  `(try ~@body
        (catch Exception e#
          (str e#))))




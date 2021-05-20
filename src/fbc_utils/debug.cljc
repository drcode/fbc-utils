(ns fbc-utils.debug
  #?(:cljs (:require [cljs.reader :refer [read-string]])
     :clj  (:require [clojure.pprint :refer [pprint]])))

(defmacro loop-dbg [vars & body]
  "prints out the value of all parameters into the loop for debugging purposes."
  `(loop ~vars
     (do ~@(map (fn [[var val]]
                  `(dbg ~var '~var))
                (partition 2 vars))
         ~@body)))

(defn strip-ampersand [coll]
  (if (coll? coll)
    (vec (remove #{'&} coll))
    coll))

(defmacro let-dbg [vars & body]
  "prints out the value of all declarations of a let"
  `(let ~(vec (mapcat (fn [[var val]]
                        `(~var ~val
                          ~'_  (dbg ~(strip-ampersand var) '~var)))
                      (partition 2 vars)))
     ~@body))

(defmacro for-dbg [bindings & body]
  `(doall (for ~bindings
            (do ~@(map (fn [[var val]]
                         `(dbg ~var '~var))
                       (partition 2 bindings))
                (dbg (do ~@body) '~'result)))))

(defmacro map-dbg [fun & colls]
  (let [[k params & body] fun]
    (assert (list? fun))
    (assert (= k 'fn))
    `(doall (map (fn ~params
                   ~@(map (fn [param]
                            `(dbg ~param '~param))
                          params)
                   ~@body)
                 ~@colls))))

(defmacro defn-dbg [nam args & body]
  `(defn ~nam ~args
     ~@(map (fn [arg]
              `(dbg ~arg '~arg))
            args)
     (dbg (do ~@body) '~'result)))

(defmacro fn-dbg [args & body]
  `(fn ~args
     ~@(map (fn [arg]
              `(dbg ~arg '~arg))
            args)
     (dbg (do ~@body) '~'result)))

(defmacro ?? [var & args]
  (if-let [[arg] (seq args)]
    `(dbg (~arg ~var) '~var)
    `(dbg ~var '~var)))

(def max-label-length 60)
(def max-raw-val-length 1000)

(def ^:dynamic dbg-enabled true)

(defn dbg
"Simple debug function useful for getting intermediates in -> piping."
([val s]
 (when dbg-enabled
   (try (let [key    (let [s (pr-str s)]
                       (if (> (count s) max-label-length)
                         (str (subs s 0 (- max-label-length 3)) "...")
                         s))
              result (try (pr-str val)
                          #?(:cljs (catch js/Error e
                                     e)))]
          #?(:clj (if (= s val)
                    (println "###" s "###")
                    (if (> (count result) max-raw-val-length)
                      (do (println key "=")
                          (pprint val))
                      (println key
                               "="
                               result)))
             :cljs (let [k (exists? js/window)]
                     (if k
                       (if (= s val)
                         (.log js/console (str "### " s " ###"))
                         (.log js/console (str key "=") val))
                       (if (= s val)
                         (println "###" s "###")
                         (println key "=" result))))))))
 val)
([val]
 (dbg val "dbg")))

(defmacro dbg-switch [condition & body]
  `(binding [dbg-enabled (and dbg-enabled ~condition)]
     (try ~@body
          (catch Exception e#
            (when-not dbg-enabled
              (println "Error in disabled debug switch"))
            (throw e#)))))

(defmacro dbg-switch-force [condition & body]
  `(binding [dbg-enabled ~condition]
     (try ~@body
          (catch Exception e#
            (when-not dbg-enabled
              (println "Error in disabled debug switch"))
            (throw e#)))))

(defmacro !! [& body]
  `(do ~@(butlast body)))

(defn dbg* [form]
`(dbg ~form '~form))

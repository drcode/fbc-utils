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
                        `(~var (dbg ~val '~var)))
                      (partition 2 vars)))
     ~@body))

(defmacro let-dbg [vars & body] ;prints out the value of all declarations of a let
  (let [bindings   (partition 2 vars)
        dbg-macro? (fn [[nam :as binding]]
                     (and (coll? nam) (seq nam) (= (first nam) 'fbc-utils.debug/dbg)))]
    (if (some dbg-macro? bindings)
      `(let ~(vec (mapcat (fn [[nam val :as binding]]
                            (if (dbg-macro? binding)
                              (let [nam (second nam)]
                                [nam val (gensym) `(fbc-utils.debug/dbg ~nam '~nam)])
                              binding))
                          bindings))
         ~@body)
      `(let ~(vec (mapcat (fn [[var val]]
                            `(~var (dbg ~val '~var)))
                          bindings))
         ~@body))))

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

(defmacro ?? [var]
  `(dbg ~var '~var))

(def max-label-length 60)
(def max-raw-val-length 1000)

(def ^:dynamic dbg-enabled true)
(def active-key (atom false))
(def ^:dynamic indent 0)

(defn reset-debug-indent []
  (reset! active-key false)
  )

(defn dbg-key [s]
  (when dbg-enabled
    (try (let [key (let [s (pr-str s)]
                     (if (> (count s) max-label-length)
                       (str (subs s 0 (- max-label-length 3)) "...")
                       s))]
           (when @active-key
             (println))
           (dotimes [_ indent]
             (print " | "))
           #?(:clj (if (keyword? s)
                     (println "###" s "###")
                     (do (reset! active-key true)
                         (print key)))
              :cljs (let [k (exists? js/window)]
                      (if k
                        (if (keyword? s)
                          (.log js/console (str "### " s " ###"))
                          (.log js/console (str key "= ")))
                        (if (keyword? s)
                          (println "###" s "###")
                          (do (reset! active-key true)
                              (print key "= ")))))))))
  val)

;; Simple debug function useful for getting intermediates in -> piping.
(defn dbg-val [val]
  (when dbg-enabled
    (try (let [result (pr-str val)]
           (if @active-key
             (print " = ")
             (dotimes [n# indent]
               (if (= n# (dec indent))
                 (print " └-")
                 (print " | "))))
           
           #?(:clj (if (> (count result) max-raw-val-length)
                     (pprint val)
                     (println result))
              :cljs (let [k (exists? js/window)]
                      (if k
                        (.log js/console val)
                        (println result)))))))
  (reset! active-key false)
  val)

(defmacro dbg [exp s]
  `(do (let [s# ~s]
         (dbg-key s#)
         (when-not (keyword? s#)
           (binding [indent (inc indent)]
             (do
               (dbg-val ~exp)))))))

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

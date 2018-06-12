(ns fbc-utils.debug)

(defmacro loop-dbg [vars & body]
          "prints out the value of all parameters into the loop for debugging purposes."
          `(loop ~vars
                 (println ~@(for [[var] (partition 2 vars)]
                                 `(str '~var "=" (pr-str ~var) " ")))
                 ~@body))

(defmacro let-dbg [vars & body]
          "prints out the value of all declarations of a let"
          `(let ~vars
                (println ~@(for [[var] (partition 2 vars)]
                                `(str '~var "=" (pr-str ~var) " ")))
                ~@body))

(defn format-filename [{:keys [file line]}]
  (str (re-find #"[^/]*$" file) ":" line))

(defmacro mdbg [var]
  `(dbg ~var '~var ~#?(:clj  (re-find #"[^/]*$" *file*)
                       :cljs (format-filename (meta &form)))))

(defmacro ?? [var]
  `(dbg ~var '~var ~#?(:clj  (re-find #"[^/]*$" *file*)
                       :cljs (format-filename (meta &form)))))

(def max-label-length 60)

(defn dbg
  "Simple debug function useful for getting intermediates in -> piping."
  ([val s fname]
   (try (let [key    (let [s (pr-str s)]
                       (if (> (count s) max-label-length)
                         (str (subs s 0 (- max-label-length 3)) "...")
                         s))
              result (try (pr-str val)
                          #?(:cljs (catch js/Error e
                                     e)))]
          #?(:clj (println (when fname
                     fname)
                   key
                   "="
                   result)
             :cljs (if (exists? js/window)
                     (.log js/console
                           (str #_(when fname
                                    fname)
                                s
                                "=")
                           val)
                     (println (when fname
                                fname)
                              key
                              "="
                              result)))))
   val)
  ([val]
   (dbg val "dbg" nil)))

(defmacro mdbg-sample [n var]
  `(if (zero? (rand-int ~n))
     (mdbg ~var)
     ~var))

(ns fbc-utils.core
  #?(:clj (:refer-clojure :rename {read-string core-read-string}))
  (:require [clojure.spec.alpha :as s]
            [clojure.math.combinatorics :as cb]            
            [clojure.set]
            #?(:clj [clojure.java.io :as io])
            #?(:clj [clojure.edn :as ed])
            #?(:clj [clojure.java.shell :as sh])
            #?(:cljs [cljs.reader :as rd])))

(def pi 3.14159265)

(defn degrees->rad [degrees]
  (* (/ (or degrees 0) 360) 2 pi))

(defn sqrt [x]
  #?(:clj (Math/sqrt x)
     :cljs (js/Math.sqrt x)))

(defn abs [x]
  #?(:clj (Math/abs x)
     :cljs (js/Math.abs x)))

(defn pow [x y]
  #?(:clj (Math/pow x y)
     :cljs (js/Math.pow x y)))

(defn atan2 [y x]
  #?(:clj  (Math/atan2 y x)
     :cljs (js/Math.atan2 y x)))

(defn cos [x]
  #?(:clj (Math/cos x)
     :cljs (js/Math.cos x)))

(defn sin [x]
  #?(:clj (Math/sin x)
     :cljs (js/Math.sin x)))

(defn acos [x]
  #?(:clj (Math/acos x)
     :cljs (js/Math.acos x)))

(defn asin [x]
  #?(:clj (Math/asin x)
     :cljs (js/Math.asin x)))

(defn parse-int [s]
  #?(:clj  (Integer. s)
     :cljs (js/parseInt s)))

(defn xor [a b]
  (or (and a (not b)) (and (not a) b)))

(def max-int #?(:clj Integer/MAX_VALUE
                :cljs js/Number.MAX_SAFE_INTEGER))

(defn sign [n]
  (cond (zero? n) 0
        (pos? n) 1
        (neg? n) -1))

(defn ceil [n]
  #?(:clj  (int (Math/ceil n))
     :cljs (js/Math.ceil n)))

(defn round [n]
  #?(:cljs (int (js/Math.round n))
     :clj  (int (Math/round (float n)))))

(defn interpolate [v1 v2 frac]
  (+ v1 (* (- v2 v1) frac)))

(defn flip-map [coll]
  (into {}
        (for [[k v] coll]
          [v k])))

(defn multivec
  ([sizes val]
   (if-let [[cur & more] (seq sizes)]
     (vec (repeat cur (multivec more val)))
     val))
  ([sizes]
   (multivec sizes nil)))

(defn cartesians [& args]
  (map vec (apply cb/cartesian-product (map range args))))

(defmacro defmethod-group [fname keys & body]
  `(do ~@(for [key keys]
           `(defmethod ~fname ~key
              ~@body))))

(defmacro closed-map
  ([req-keys opt-keys]
   `(clojure.spec.alpha/merge (clojure.spec.alpha/keys :req ~(vec req-keys)) (clojure.spec.alpha/map-of ~(clojure.set/union req-keys opt-keys) any?)))
  ([req-keys]
   `(closed-map ~req-keys #{})))

(defmacro closed-map-un
  ([req-keys opt-keys]
   `(clojure.spec.alpha/merge (clojure.spec.alpha/keys :req-un ~(vec req-keys) :opt-un ~(vec opt-keys)) (clojure.spec.alpha/map-of ~(clojure.set/union req-keys (set (map (comp keyword name) opt-keys))) any?)))
  ([req-keys]
   `(closed-map-un ~req-keys {})))

(defmacro forv [[item items] & body]
  `(loop [items# ~items
          acc#   []]
     (if (seq items#)
       (recur (rest items#)
              (conj acc#
                    (let [~item (first items#)]
                      ~@body)))
       acc#)))

(defmacro for-reduce [[acc acc-init item items] & body]
  `(loop [items# ~items
          ~acc   ~acc-init]
     (if (seq items#)
       (recur (rest items#)
              (let [~item (first items#)]
                ~@body))
       ~acc)))

(defmacro lazy [& body]
  `(memoize (fn []
              ~@body)))

(defmacro fn-> [var & statements]
  `(fn [x#]
     (let [~var x#]
       (-> x#
           ~@statements))))

(defn extract-opts [args]
  (if (and (seq args) (map? (first args)))
    [(first args) (rest args)]
    [{} args]))

(defn date->unix-time [date]
  (int (/ (.getTime date) 1000)))

#_(defn validate-atom [spec state-atom]
    (set-validator! state-atom
                    (fn [k]
                      (if (s/valid? spec k)
                        true
                        (throw (ex-info (str (s/explain-str spec k) "\n" (pr-str k)) {}))))))

(defn square [x]
  (* x x))

(defn avg [& args]
  (/ (apply + args)
     (count args)))

(defn between ;interval closed on left, open on right
  ([a b c]
   (and (<= a b) (< b c)))
  ([a b]
   (<= a b)))

(defn bracket
  "the return value is bracketed between min-val and max-val, closed on both sides"
  [min-val val max-val]
  (cond (>= min-val val) min-val
        (< val max-val)  val
        :else            max-val))

(defn dissoc-in [coll path]
  (update-in coll (butlast path) dissoc (last path)))

(defn consv [item coll]
  (vec (cons item coll)))

(defn looped-forward [length index]
  (mod (inc index) length))

(defn looped-backward [length index]
  (mod (+ index length -1) length))

(defn looped-increment [length index direction]
  "Increments the index is direction is 'true' otherwise decrements. Correctly wraps around the ends based on the length parameter"
  (if direction
    (mod (inc index) length)
    (mod (+ index length -1) length)))

(defn sqr-dist [[x1 y1] [x2 y2]]
  (+ (square (- x1 x2)) (square (- y1 y2))))

(defn rem-exclamation [kw]
  "Removes a postfixed exclamation mark from a keyword- Useful for situations where ':foo!' is used as a shorthand for 'set :foo'"
  (keyword (namespace kw) (apply str (butlast (name kw)))))

(defn keyed-by-id [coll]
  "Takes a collection of items with a :id key, and returns the collection as a map against those keys."
  (into {}
        (for [{:keys [:id] :as item} coll]
          [id item])))

(defn key-max [db]
  (apply max (keys db)))

(defn push-with-id [db obj]
  (let [id (if (seq db)
             (inc (key-max db))
             0)]
    (assoc db id (assoc obj :id id))))

(defn index-of [coll val]
  "Returns index of first instance of val in coll"
  (loop [coll  coll
         index 0]
    (when-let [[item & more] (seq coll)]
      (if (= item val)
        index
        (recur more (inc index))))))

(defn find-first [coll pred]
  (when-let [[k] (seq (filter pred coll))]
    k))

(defn partition-pred [pred coll]
  "Same as [(filter pred coll) (remove pred coll)]"
  [(filter pred coll) (remove pred coll)])

(defn ord [c]
  "converts char to ascii code"
  #?(:cljs (.charCodeAt c 0)
     :clj  (int c)))

(defn throw [s]
  (throw (ex-info (if (keyword? s)
                    (name s)
                    s)
                  {})))

(defn minimal-angle-difference [& args]
  "For two radian angles, find the minimal angle between them."
  (let [[ang1 ang2] (sort args)
        ang         (mod (- ang2 ang1) (* pi 2))]
    (if (< ang pi)
      ang
      (- (* pi 2) ang))))

(defn lsr-rand
  "A crude lsr-based rand implementation that is deterministic"
  ([]
   0xACE1)
  ([prev]
   (let [bit (bit-and (bit-xor prev (bit-shift-right prev 2) (bit-shift-right prev 3) (bit-shift-right prev 5)) 1)]
     (bit-or (bit-shift-right prev 1) (bit-shift-left bit 15)))))

(defmacro ^{:private true} assert-args
  [& pairs]
  `(do (when-not ~(first pairs)
         (throw (IllegalArgumentException.
                  (str (first ~'&form) " requires " ~(second pairs) " in " ~'*ns* ":" (:line (meta ~'&form))))))
     ~(let [more (nnext pairs)]
        (when more
          (list* `assert-args more)))))

(defmacro if-let*
  "Multiple binding version of if-let"
  ([bindings then]
   `(if-let ~bindings ~then nil))
  ([bindings then else]
   (assert-args
     (vector? bindings) "a vector for its binding"
     (even? (count bindings)) "exactly even forms in binding vector")
   (if (== 2 (count bindings))
     `(let [temp# ~(second bindings)]
        (if temp#
          (let [~(first bindings) temp#]
            ~then)
          ~else))
     (let [if-let-else (keyword (name (gensym "if_let_else__")))
           inner (fn inner [bindings]
                   (if (seq bindings)
                     `(if-let [~(first bindings) ~(second bindings)]
                        ~(inner (drop 2 bindings))
                        ~if-let-else)
                     then))]
       `(let [temp# ~(inner bindings)]
          (if (= temp# ~if-let-else) ~else temp#))))))

(defn dist [pt-a pt-b]
  #?(:cljs (js/Math.sqrt (sqr-dist pt-a pt-b))
     :clj (Math/sqrt (sqr-dist pt-a pt-b))))

#?(:clj (do (def read-string ed/read-string)
            (defmacro static-slurp [file]
              (clojure.core/slurp file))
            (defn get-tick-count []
              (System/currentTimeMillis))
            (defn exists [nam]
              (.exists (io/file nam)))))

#?(:cljs (do (defn get-tick-count []
               (js/performance.now))
             (def read-string rd/read-string)
             (defn client-coords [e]
               (let [br (.getBoundingClientRect (.-currentTarget e))]
                 [(- (.-clientX (.-nativeEvent e)) (.-x br)) (- (.-clientY (.-nativeEvent e)) (.-y br))]))
             (defonce night-mode-enabled (atom false))
             (defn devtools-night-mode []
               (when (and (not @night-mode-enabled) (get js/window "devtools") js/devtools.core.get_prefs)
                 (doseq [[k v] (js/devtools.core.get_prefs)]
                   (when (and (re-find #"-style" (name k)) v)
                     (when-let [[_ front r g b back] (re-find #"(^.*rgba\()(\d+),(\d+),(\d+)(,1\).*$)" v)]
                       (let [fun (fn [k]
                                   (min 255 (+ k 140)))
                             rr (fun (read-string r))
                             gg (fun (read-string g))
                             bb (fun (read-string b))]
                         (if (< (+ (read-string r) (read-string g) (read-string b)) 40)
                           (js/devtools.core.set_pref_BANG_ k (str front 0 "," 0 "," 0 back))
                           (js/devtools.core.set_pref_BANG_ k (str front rr "," gg "," bb back)))))))
                 (reset! night-mode-enabled true)))))

(ns fbc-utils.core
  (:require [clojure.spec.alpha :as s]
            [clojure.set]))

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

(defn interpolate [v1 v2 frac]
  (+ v1 (* (- v2 v1) frac)))

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
  "Like for, but (1) is eager, allowing it to work in go blocks (2) returns a vector (3) permits multiple body expressions"
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

(defn validate-atom [spec state-atom]
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

(defn keep-indexed [pred coll]
  (keep (fn [[index val]]
          (pred index val))
        (map-indexed vector coll)))

(defn consv [item coll]
  (vec (cons item coll)))

;; removed because same as split-with
#_(defn split-while
  "Same as [(take-while pred lst) (drop-while pred lst)]"
  [pred lst]
  [(take-while pred lst) (drop-while pred lst)])

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
  "Takes a collection of items with a :db/id key, and returns the collection as a map against those keys."
  (into {}
        (for [{:keys [:db/id] :as item} coll]
          [id item])))

(defn index-of [coll val]
  "Returns index of first instance of val in coll"
  (loop [coll  coll
         index 0]
    (when-let [[item & more] (seq coll)]
      (if (= item val)
        index
        (recur more (inc index))))))

(defn partition-pred [pred coll]
  "Same as [(filter pred coll) (remove pred coll)]"
  [(filter pred coll) (remove pred coll)])

(defn ord [c]
  "converts char to ascii code"
  #?(:cljs (.charCodeAt c 0)
     :clj  (int c)))

(defn dist [pt-a pt-b]
  #?(:cljs (js/Math.sqrt (sqr-dist pt-a pt-b))
     :clj (Math/sqrt (sqr-dist pt-a pt-b))))

#?(:clj (do (defmacro static-slurp [file]
              (clojure.core/slurp file))
            
            (defn get-tick-count []
              (System/currentTimeMillis))))

#?(:cljs (do (defn get-tick-count []
               (js/performance.now))
             (defn client-coords [e]
               (let [br (.getBoundingClientRect (.-currentTarget e))]
                 [(- (.-clientX (.-nativeEvent e)) (.-x br)) (- (.-clientY (.-nativeEvent e)) (.-y br))]))))


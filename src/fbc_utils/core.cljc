(ns fbc-utils.core
  #?(:clj (:refer-clojure :rename {read-string core-read-string}))
  (:require [clojure.spec.alpha :as s]
            [clojure.math.combinatorics :as cb]            
            [clojure.set :as se]
            [clojure.string :as st]
            [clojure.pprint :as pp]
            #?(:clj [clojail.core])
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

#_(defmacro <!timeout [c tim]
    `(let [k#        ~c
           [val# c#] (clojure.core.async/alts! [k# (clojure.core.async/timeout ~tim)])]
       (when (= c# k#)
         val#)))

(defn sign [n]
  (cond (zero? n) 0
        (pos? n) 1
        (neg? n) -1))

(defn ceil [n]
  #?(:clj  (int (Math/ceil n))
     :cljs (js/Math.ceil n)))

(defn round [n]
  #?(:cljs (int (js/Math.round n))
     :clj  (int (Math/round (double n)))))

(defn int->hex [n]
  (let [hex-chars "0123456789ABCDEF"]
    (str "0x"
         (loop [n       n
                hex-str ""]
           (if (zero? n)
             hex-str
             (recur (quot n 16) (str (nth hex-chars (mod n 16)) hex-str)))))))

(defn interpolate [v1 v2 frac]
  (+ v1 (* (- v2 v1) frac)))

(defmacro unwrap [var & body]
  `(let [~var (deref ~var)]
     ~@body))

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

(defn sane-number [n]
  (if (= (float (int n)) n)
    (int n)
    (float n)))

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

(defmacro let-> [val var & body]
  `(let [~var ~val]
     ~@body))

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

(defn trues [coll]
  (filter identity coll))

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

(defn looped-range [length index direction];returns the looped indexes as an infinite list
  (iterate (fn [index]
             (looped-increment length index direction))
           index))

;;(take 10 (looped-range 10 3 true))

(defn split-with-count [pred coll];like "split-with" but adds a count argument to the predicate
  (loop [acc  []
         coll coll
         n    0]
    (if-let [[cur & more] (seq coll)]
      (if (pred cur n)
        (recur (conj acc cur) more (inc n))
        [acc coll])
      [acc nil])))

;;(split-with-count <= [0 1 2 3 5 5 6 7])

(defn removev [coll index]
  (vec (concat (subvec coll 0 index) (subvec coll (inc index)))))

;;(removev [1 2 3 4 5] 2)

(defn take-in-order [& args];like "take-while" but accepts multiple predicates, which will be fullfilled in order
  (when (> (count args) 1)
    (let [[a b] (split-with-count (first args) (last args))]
      (concat a (apply take-in-order (conj (vec (butlast (rest args))) b))))))

;;(take-in-order (fn [k n] (odd? k)) (fn [k n] (even? k)) (fn [k n] (odd? k)) [1 7 6 4 3 9 9 8])

(defn sqr-dist [[x1 y1] [x2 y2]]
  (+ (square (- x1 x2)) (square (- y1 y2))))

(defn rem-exclamation [kw]
  "Removes a postfixed exclamation mark from a keyword- Useful for situations where ':foo!' is used as a shorthand for 'set :foo'"
  (keyword (namespace kw) (apply str (butlast (name kw)))))

(defn namespacify [namespace obj]
  (into {}
        (for [[k v] obj]
          [(keyword (name namespace) (name k)) v])))

(defn keyed-by-id [coll]
  "Takes a collection of items with a :id key, and returns the collection as a map against those keys."
  (into {}
        (for [{:keys [:id] :as item} coll]
          [id item])))

(defn key-max [db]
  (apply max (keys db)))

(defn next-key [db]
  (if (seq db)
    (inc (key-max db))
    0))

(defn push-with-id [db obj]
  (let [id (next-key db)]
    (assoc db id (assoc obj :id id))))

(defn index-of [coll val]
  "Returns index of first instance of val in coll"
  (loop [coll  coll
         index 0]
    (when-let [[item & more] (seq coll)]
      (if (= item val)
        index
        (recur more (inc index))))))

(defn find-index [pred coll]
  "Returns index of first match of pred in coll"
  (loop [coll  coll
         index 0]
    (when-let [[item & more] (seq coll)]
      (if (pred item)
        index
        (recur more (inc index))))))

(defn find-first [pred coll]
  (when-let [[k] (seq (filter pred coll))]
    k))

(defn partition-pred [pred coll]
  "Same as [(filter pred coll) (remove pred coll)]"
  [(filter pred coll) (remove pred coll)])

(defn partition-eager [n coll]
  (lazy-seq (when (seq coll)
              (cons (take n coll) (partition-eager n (drop n coll))))))

(defn partition-slop [min-size coll] ;;takes any remaining items after the partition and disperses it among the existing partitions
  (let [coll-size           (count coll)
        num-partitions      (max 1 (Math/ceil (/ coll-size min-size)))
        base-partition-size (quot coll-size num-partitions)
        slop                (rem coll-size num-partitions)
        take-n              (fn [i]
                              (+ base-partition-size
                                 (if (< i slop) 1 0)))]
    (first (reduce (fn [acc block]
                     (let [n (take-n block)
                           new-items (split-at n (second acc))]
                       [(conj (first acc) (first new-items)) (second new-items)]))
                   [[] coll]
                   (range num-partitions)))))

(defn ord [c]
  "converts char to ascii code"
  #?(:cljs (.charCodeAt c 0)
     :clj  (int c)))

(defn throw [& args]
  (throw (ex-info (if (seq args)
                    (apply str args)
                    "UNNAMED ERROR")
                  {})))

(defn maplist [fun coll]
  (lazy-seq (when (seq coll)
              (cons (fun coll) (maplist fun (rest coll))))))

;;(maplist #(apply + %) [1 2 3 4 5])

(defmacro catch-to-debug [& body]
  `(try ~@body
        (catch Exception e#
          (fbc-utils.debug/dbg (.getMessage e#) "exception")
          (.getMessage e#))))

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


(defn interleave-all
  "Returns a lazy seq of the first item in each coll, then the second, etc.
  Unlike `clojure.core/interleave`, the returned seq contains all items in the
  supplied collections, even if the collections are different sizes."
  {:arglists '([& colls])}
  ([] ())
  ([c1] (lazy-seq c1))
  ([c1 c2]
   (lazy-seq
    (let [s1 (seq c1), s2 (seq c2)]
      (if (and s1 s2)
        (cons (first s1) (cons (first s2) (interleave-all (rest s1) (rest s2))))
        (or s1 s2)))))
  ([c1 c2 & colls]
   (lazy-seq
    (let [ss (remove nil? (map seq (conj colls c2 c1)))]
      (if (seq ss)
        (concat (map first ss) (apply interleave-all (map rest ss))))))))


(defn furthest-away [start end]
  (when (not= start end)
    (let [mid (bit-shift-right (+ start end) 1)]
      (cons mid (interleave-all (furthest-away start mid) (furthest-away (inc mid) end))))))

(defn standarddev [a] 
  (sqrt (/ (apply + (map square (map - a (repeat (apply avg a))))) (dec (count a)))))

;;(standarddev [1 2 4])

;;(take 10 (furthest-away 1 1000))

(defn distinct-by [f coll]
  (letfn [(step [xs seen]
            (lazy-seq (when-let [[x & more] (seq xs)]
                        (let [k (f x)]
                          (if (seen k)
                            (step more seen)
                            (cons x (step more (conj seen k))))))))]
    (step coll #{})))

(def distinguishable-colors
  ((fn fun [visited level]
     (let [vals      (conj (vec (range 0 256 (bit-shift-right 256 level))) 255)
           cols      (vec (for [r vals
                                g vals
                                b vals]
                            [r g b]))
           col-count (count cols)
           cols      (map cols (concat [0 (dec col-count)] (furthest-away 1 (dec col-count))))
           cols      (remove visited cols)]
       (lazy-cat cols (fun (se/union visited (set cols)) (inc level)))))
   #{}
   0))

(defn print-in-columns-helper [& strs]
  (let [split-strs (map clojure.string/split-lines strs)
        rows-num   (apply max (map count split-strs)) 
        col-widths (for [s split-strs]
                     (apply max (map count s)))
        split-strs (for [col split-strs]
                     (take rows-num (concat col (repeat ""))))]
    (doall (apply map
                  (fn [& str-row]
                    (doall (map (fn [s wid]
                                  (print (apply str (take (inc wid) (concat s (repeat " "))))))
                                str-row
                                col-widths))
                    (println))
                  split-strs))))

(defmacro print-in-columns [& args]
  `(print-in-columns-helper ~@(for [arg args]
                                `(with-out-str ~arg))))

;;(print-in-columns (dotimes [n 12] (println n)) (dotimes [n 15] (println "foo")))

(defn nested-indexes [coll depth]
  (if (pos? depth)
    (mapcat (fn [[index item]]
              (map cons (repeat index) (nested-indexes item (dec depth))))
            (map-indexed vector coll))
    [[]]))

;;(nested-indexes [[:a :b] [:c] [:d :e]] 2)

(defn not-too-deep ;throws error if more than "num" sequence items are consumed
  ([coll num]
   (lazy-seq (when (seq coll)
               (if (zero? num)
                 (throw (ex-info "sequence consumption too deep" {}))
                 (cons (first coll) (not-too-deep (rest coll) (dec num))))))) 
  ([coll]
   (not-too-deep coll 1000)))

;;(not-too-deep (range 10000))

(defn or-fun [a b]
  (or a b))

(defn spit-edn [fname edn]
  (spit fname (with-out-str (pp/pprint edn))))

(declare get-tick-count)

(let [last-time (atom 0)]
  (defn time-gated-println [s]
    (let [n (get-tick-count)]
      (swap! last-time
             (fn [last-time]
               (cond-> last-time
                 (> (- n last-time) 200) (do (println s) n)))))))

(defn group-while [pred coll] ;breaks coll sequentially into pairs of coll, where the first in pair is true and the second of pair is false
  (lazy-seq (when (seq coll)
              (let [[a more] (split-with pred coll)
                    [b more] (split-with (complement pred) more)]
                (cons [a b] (group-while pred more))))))

#_(group-while even? [4 6 3 4 3 3 3 4])

(defn previous-consecutive-cycle [coll]
  (map concat (reductions conj () (cycle (reverse coll))) (repeat (cycle coll))))

(defn eat [val] ;val is really big, show first 100 chars of print, plus total num of character
  (let [s (with-out-str (pp/pprint val))]
    (str (apply str (take 100 s)) "...(" (count s) " chars)")))

(defn matches [regex s]
  (when-let [k (re-matches regex s)]
    (rest k)))

;;(matches #"Ask Task (.*): (.*)" "Ask Task #1: Foo bar")

(defn match [regex s]
  (when-let [k (re-matches regex s)]
    (second k)))

;;(match #"Ask Task (.*)" "Ask Task #1")

(defn serialize [fname val] ;passes through the value but also serializez it as edn
  (spit fname (with-out-str (pp/pprint val)))
  val)

#?(:clj (do (def read-string ed/read-string)
            (defmacro static-slurp [file]
              (clojure.core/slurp file))
            (defn get-tick-count []
              (System/currentTimeMillis))
            (defn exists [nam]
              (.exists (io/file nam)))
            (defmacro with-timeout [time & body]
              `(let [out# *out*]
                 (clojail.core/thunk-timeout (fn []
                                               (binding [*out* out#]
                                                 ~@body))
                                             ~time)))
            (defn log-slurp [fname]
              (if (exists fname)
                (map read-string (st/split (slurp fname) #"\n"))
                []))

            (defn log-append [fname item]
              (spit fname (str (pr-str item) "\n") :append true))))

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

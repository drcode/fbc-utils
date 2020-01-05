(ns fbc-utils.style
  (:require [clojure.string :as st]))

(defn strip-nils [coll]
  (into {}
        (for [[k v] coll]
          (when v
            [k v]))))

(defn style 
  ([flex atts]
   (let [{:keys [atts]} (reduce (fn [{:keys [front atts] :as acc} item]
                                  {:front false
                                   :atts  (merge atts
                                                 (case item
                                                   "row"            {:flex-direction :row}
                                                   "column"         {:flex-direction :column}
                                                   "row-reverse"    {:flex-direction :row-reverse}
                                                   "column-reverse" {:flex-direction :column-reverse}
                                                   "wrap"           {:flex-wrap :wrap}
                                                   "start"          (if front
                                                                      {:justify-content :flex-start}
                                                                      {:align-items :flex-start})
                                                   "end"            (if front
                                                                      {:justify-content :flex-end}
                                                                      {:align-items :flex-end})
                                                   "center"         (if front
                                                                      {:justify-content :center}
                                                                      {:align-items :center})
                                                   "between"        {:justify-content :space-between}
                                                   "around"         {:justify-content :space-around}
                                                   "evenly"         {:justify-content :space-evenly}
                                                   "stretch"        {:align-items :stretch}
                                                   "baseline"       {:align-items :baseline}
                                                   (if-let [[_ n] (re-matches #"^max([0-9]+)$" item)]
                                                     {:max-width (str n "rem")}
                                                     (let [[cur & more] item
                                                           more         (apply str more)
                                                           rem          (fn [s]
                                                                          (case s
                                                                            nil nil
                                                                            "0" nil
                                                                            "1" "1rem"
                                                                            "2" "2rem"
                                                                            "3" "3rem"
                                                                            "4" "4rem"
                                                                            "5" "0.5rem"))]
                                                       (case (first item)
                                                         "m" (if (= 1 (count more))
                                                               {:margin (rem (first more))}
                                                               (let [[l t r b] more]
                                                                 (strip-nils {:margin-left   (rem l)
                                                                              :margin-top    (rem t)
                                                                              :margin-right  (rem r)
                                                                              :margin-bottom (rem b)})))
                                                         "p" (if (= 1 (count more))
                                                               {:padding (rem (first more))}
                                                               (let [[l t r b] more]
                                                                 (strip-nils {:padding-left   (rem l)
                                                                              :padding-top    (rem t)
                                                                              :padding-right  (rem r)
                                                                              :padding-bottom (rem b)})))
                                                         "b" {:background-color (if (re-matches #"^[0-9a-f][0-9a-f][0-9a-f]$" more)
                                                                                  (str "#" more)
                                                                                  more)}
                                                         "c" {:color (if (re-matches #"^[0-9a-f][0-9a-f][0-9a-f]$" more)
                                                                       (str "#" more)
                                                                       more)}
                                                         "o" (let [[_ col wid] (re-matches #"^([a-z]+)([0-9]+)?$" more)]
                                                               (strip-nils {:box-sizing   :border-box
                                                                            :border-style :solid
                                                                            :border-width  wid
                                                                            :border-color col}))
                                                         (throw (ex-info (str "Can't parse style clause " item) {})))))))})
                                {:front true
                                 :atts  atts}
                                (st/split (name flex) #":"))]
     (cond-> atts
       (seq (select-keys atts [:flex-direction :flex-wrap :justify-content :align-items])) (assoc :display :flex))))
  ([flex]
   (style flex {})))



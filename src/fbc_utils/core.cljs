(ns fbc-utils.core)

(defn get-tick-count []
  (.getTime (js/Date.)))

(defn log [& args]
  (.apply (.-log js/console)
          js/console
          (apply array
                 args)))

(defn extract-opts [args]
  (if (and (seq args) (map? (first args)))
    [(first args) (rest args)]
    [{} args]))


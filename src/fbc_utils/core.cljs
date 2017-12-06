(ns fbc-utils.core)

(defn get-tick-count []
  (.getTime (js/Date.)))


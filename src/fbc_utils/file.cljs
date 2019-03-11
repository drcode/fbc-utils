(ns animui.file
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [fbc-utils.debug :refer [??]]
            [cljs-node-io.core :as io]
            [cljs-node-io.file :as fi]
            [cljs.core.async :as as]))

(def last-err (atom nil))

(defn aslurp [fname]
  (go (let [[err data] (<! (io/aslurp fname))]
        (if err
          (do (println "Error slurping file" fname err)
              (reset! last-err err)
              nil)
          data))))

(defn aspit [fname s]
  (go (let [[err] (as/<! (io/aspit fname s))]
        (when err
          (println "Error spitting file" fname err)
          (reset! last-err err)))))

(defn empty-out-directory [fname]
  (let [f (fi/File. fname)]
    (doseq [f (.listFiles f)]
      (.delete f))))




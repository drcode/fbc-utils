(ns fbc-utils.cliui
  (:refer-clojure :rename {defn      core-defn
                           defmethod core-defmethod})
  (:require [snek.core :as sn :refer [defn defmethod defsnek]]
            [clojure.string :as st]
            [fbc-utils.core :as ut :refer [defmethod-group]]))

(def command-snek {"" {:desc "" :shortcut "" :cmd :_ :args [{:label :_ :snek  [nil]}]}})

(def indexes-snek [{:id 0 :type :_}])

(defsnek)

(defsnek -> [""])
(defn historical-actions []
  (if (ut/exists "actions.edn")
    (st/split (slurp "actions.edn") #"\n")
    []))

(defn recover-state [state action-fun]
  (reduce (fn [acc [cmd param :as item]]
            (action-fun acc cmd param))
          state
          (map read-string (historical-actions))))

(defmulti parse-core
  (fn [cmd arg-label args]
    [cmd arg-label]))

(defmethod-group parse-core [[:help :noargs] [:quit :noargs]]
  [cmd arg-label args]
  [cmd nil])

(defmethod parse-core :default
  [cmd arg-label args])

(defmulti action-core
  (fn [_ cmd param]
    cmd))

(defmethod action-core :help
  [{:keys [commands] :as env} cmd param]
  (mapv println (sort (fn [& args]
                        (apply compare
                               (for [arg args]
                                 (st/replace arg #"\[|\]" ""))))
                      (map :desc (vals commands))))
  env)

(defmethod action-core :quit
  [env cmd param]
  env)

(defmethod action-core :default
  [env cmd param])

(defsnek command-snek nil indexes-snek "" -> [:_ nil])
(defn parse-input [commands parse-fun indexes s]
  (let [[_ cmd param] (re-matches #"([a-z]+)( +.+)?$" s)
        {:keys [cmd
                args]
         :as   command} (commands cmd)]
    (if-let [k (some (fn [{:keys [label
                                  snek]
                           :as   arg}]
                       (when-let [g (sn/parse (conj snek :__end) (str param " :__end"))]
                         [label (butlast g)]))
                     args)]
      (or (apply parse-core cmd k) (apply parse-fun cmd k))
      [:nop nil])))

(defsnek "" -> {:desc "" :shortcut "" :cmd :_})
(defn parse-command [desc]
  (let [k (re-seq #"( |[a-z]+|\[([a-z]+)\])" desc)]
    {:desc     desc
     :shortcut (apply str
                      (keep (fn [[_ _ x :as item]]
                              x)
                            k))
     :cmd      (keyword (apply str
                               (for [[_ a b :as item] k]
                                 (cond b         b
                                       (= a " ") "-"
                                       :else     a))))}))

(defsnek [nil] -> {:strings [""] :indexes [{}]})
(defn annotated-strings [coll]
  (reduce (fn [{:keys [strings
                       indexes]
                :as   acc} item]
            (if (string? item)
              (update acc :strings conj item)
              {:strings (conj strings (:text item))
               :indexes (conj indexes (dissoc item :text))}))
          {:strings []
           :indexes []}
          coll))

(defsnek [{:desc ""}] -> command-snek)
(defn massage-commands [commands]
  (let [parsed (for [{:keys [desc]
                      :as  command} commands]
                 (assoc (parse-command desc)
                        :args
                        (ut/forv [[k v] (dissoc command :desc)]
                                 {:label k
                                  :snek  v})))]
    (if (not= (count (distinct (map :shortcut parsed))) (count parsed))
      (throw "overlap in command shortcuts!")
      (reduce (fn [acc
                   {:keys [shortcut]
                    :as   item}]
                (assoc acc shortcut item))
              {}
              parsed))))

(defn command-helper [render commands action-fun parse-fun s state]
  (let [{:keys [indexes]
         :as   rendering}              (render state)
        [cmd param :as action-literal] (parse-input commands parse-fun indexes s)
        new-state                      (if (action-core {:state state
                                                         :commands commands}
                                                        cmd
                                                        param)
                                         state
                                         (action-fun state cmd param))
        {:keys [strings]
         :as   rendering}              (render new-state)]
    (mapv println strings)
    (if (= state new-state)
      (println "###############NO ACTION#############")
      (spit "actions.edn" (str (pr-str action-literal) "\n") :append true))
    [cmd new-state]))

(defn command-fun [{:keys [state
                           commands
                           parse
                           action
                           render]
                    :as   args}]
  (swap! state recover-state action)
  (let [commands (massage-commands (concat [{:desc   "[h]elp"
                                             :noargs []}
                                            {:desc   "[q]uit"
                                             :noargs []}]
                                           commands))]
    (fn [s]
      (let [[cmd state-new] (command-helper render commands action parse s @state)]
        (reset! state state-new)
        cmd))))

(defn repl [command]
  (command "h")
  (loop []
    (print "> ")
    (flush)
    (let [cmd (command (read-line))]
      (when-not (= cmd :quit)
        (recur)))))

(defn user-command [command]
  (binding [*ns* (create-ns 'user)]
    (eval `(def user/command ~command))))

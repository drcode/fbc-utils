(ns fbc-utils.cliui
  (:refer-clojure :rename {defn      core-defn
                           defmethod core-defmethod})
  (:require [snek.core :as sn :refer [defn defmethod defsnek]]
            [clojure.string :as st]
            [clojure.pprint :as pp]
            [fbc-utils.core :as ut :refer [defmethod-group]]))

(def command-snek {"" {:desc     ""
                       :shortcut ""
                       :cmd      :_
                       :args     [{:label nil
                                   :snek  [nil]}]}})

(def indexes-snek [{:id nil :type :_}])

(defsnek)

(defsnek -> [""])
(defn historical-actions []
  (if (ut/exists "actions.edn")
    (st/split (slurp "actions.edn") #"\n")
    []))

(defmulti parse-core
  (fn [cmd arg-label args]
    [cmd arg-label]))

(defmethod-group parse-core [[:help :noargs] [:quit :noargs] [:dump nil]]
  [cmd arg-label args]
  [cmd nil])

(defmethod parse-core [:help :command]
  [cmd arg-label args]
  [cmd (first args)])

(defmethod parse-core :default
  [cmd arg-label args])

;; (parse-core :help :noargs [])

(defmulti action-core
  (fn [_ cmd param]
    cmd))

(defn print-commands [commands]
  (mapv println
        (sort (fn [& args]
                (apply compare
                       (for [arg args]
                         (st/replace arg #"\[|\]" ""))))
              (map :desc (vals commands)))))

(defmethod action-core :help
  [{:keys [commands] :as env} cmd param]
  (if param
    (let [{:keys [args
                  desc]} (commands (name param))]
      (println desc)
      (doseq [{:keys [label
                      snek]
               :as   arg}
              (:args (commands (name param)))]
        (println (apply str
                        (when label
                          (str (name label) ": "))
                        (interpose " " (map pr-str snek))))))
    (print-commands commands))
  env)

(defmethod action-core :dump
  [{:keys [state]
    :as   env}
   cmd
   param]
  (pp/pprint state)
  env)

(defmethod action-core :quit
  [env cmd param]
  env)

(defmethod action-core :nop
  [env cmd param]
  env)

(defmethod action-core :default
  [env cmd param]
  env)

(defsnek command-snek nil indexes-snek "" -> [:_ nil])
(defn parse-input [commands parse-fun indexes s]
  (let [[_ cmd param] (re-matches #"([a-z]+)( +.+)?$" s)
        {:keys [cmd
                args]
         :as   command} (commands cmd)]
    (if-let [k (some (fn [{:keys [label
                                  snek]
                           :as   arg}]
                       (when-let [g (sn/parse (conj (ut/forv [arg snek]
                                                             (if (= arg ::ui-index)
                                                               0
                                                               arg))
                                                    :__end)
                                              (str param " :__end"))]
                         [label (map (fn [arg val]
                                       (if (= arg ::ui-index)
                                         (indexes (dec val))
                                         val))
                                     snek
                                     (butlast g))]))
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
              {:strings (conj strings (str (inc (count indexes)) ". " (:text item)))
               :indexes (conj indexes (dissoc item :text))}))
          {:strings []
           :indexes []}
          coll))

(defn dups [seq]
  (for [[id freq] (frequencies seq)
        :when (> freq 1)]
    id))

(defsnek [{:desc ""}] -> command-snek)
(defn massage-commands [commands]
  (let [parsed (for [{:keys [desc]
                      :as  command} commands]
                 (assoc (parse-command desc)
                        :args
                        (ut/forv [[k v] (dissoc command :desc)]
                                 {:label k
                                  :snek  v})))]
    (if-let [[k] (seq (dups (map :shortcut parsed)))]
      (ut/throw (str "command shortcut " k " is duplicated"))
      (reduce (fn [acc
                   {:keys [shortcut]
                    :as   item}]
                (assoc acc shortcut item))
              {}
              parsed))))

(def core-commands [{:desc   "[h]elp"
                     :noargs []
                     :command ['_]}
                    {:desc "[d][u]mp"
                     nil   []}
                    {:desc   "[q]uit"
                     :noargs []}])

(def alternate-commands (atom nil))

(defn command-helper [render commands action-fun parse-fun s state]
  (let [{:keys [indexes]
         :as   rendering}              (render state)
        [cmd param :as action-literal] (parse-input commands parse-fun indexes s)]
    (when (and @alternate-commands (not (#{:help :dump :quit :nop} cmd)))
      (reset! alternate-commands nil))
    (let [new-state         (if (action-core {:state    state
                                              :commands commands}
                                             cmd
                                             param)
                              state
                              (action-fun state cmd param))
          {:keys [strings
                  indexes]
           :as   rendering} (render new-state)]
      (mapv println strings)
      (if (= state new-state)
        (println "##NO ACTION##")
        (spit "actions.edn" (str (pr-str action-literal) "\n") :append true))
      [cmd new-state])))

(defn temp-commands [commands]
  (reset! alternate-commands (massage-commands (concat core-commands commands))))

(defn recover-state [state action-fun]
  (reduce (fn [acc [cmd param :as item]]
            (reset! alternate-commands nil)
            (action-fun acc cmd param))
          state
          (map read-string (remove #{""} (historical-actions)))))

(defn command-fun [{:keys [state
                           commands
                           parse
                           action
                           render]
                    :as   args}]
  (reset! alternate-commands nil)
  (swap! state recover-state action)
  (let [commands (massage-commands (concat core-commands commands))]
    (fn [s]
      (let [[cmd state-new] (command-helper render (or @alternate-commands commands) action parse s @state)]
        (when @alternate-commands
          (print-commands @alternate-commands))
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

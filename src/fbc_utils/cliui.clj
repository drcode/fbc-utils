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

(defsnek 0 -> [""])
(defn historical-actions [backsteps]
  (if (ut/exists "actions.edn")
    (reverse (drop backsteps (reverse (st/split (slurp "actions.edn") #"\n"))))
    []))

(defmulti parse-core
  (fn [cmd arg-label args]
    [cmd arg-label]))

(defmethod-group parse-core [[:help :noargs] [:quit :noargs] [:dump nil] [:meta nil] [:undo nil]]
  [cmd arg-label args]
  [cmd nil])

(defmethod parse-core [:help :command]
  [cmd arg-label args]
  [cmd (first args)])

(defmethod parse-core :default
  [cmd arg-label args])

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
  (pp/pprint @state)
  env)

(defmethod action-core :quit
  [env cmd param]
  env)

(defmethod action-core :nop
  [env cmd param]
  env)

(defmethod action-core :default
  [env cmd param]
  nil)

(def meta-state (atom nil))
(def starting-state (atom nil))

(defmethod action-core :meta
  [env cmd param]
  (reset! meta-state 0)
  env)

(defmethod action-core :undo
  [{:keys [starting-state
           action-fun
           state]
    :as   env}
   cmd
   param]
  (reset! state (recover-state starting-state action-fun 1))
  (spit "actions.edn" (str (apply str (interpose "\n" (historical-actions 1))) "\n"))
  env)

(declare recover-state)

(defmethod action-core :meta-up
  [{:keys [starting-state
           action-fun
           state]
    :as   env}
   cmd
   param]
  (swap! meta-state inc)
  (reset! state (recover-state starting-state action-fun @meta-state))
  env)

(defmethod action-core :meta-down
  [{:keys [starting-state
           action-fun
           state]
    :as   env}
   cmd
   param]
  [env cmd param]
  (swap! meta-state dec)
  (reset! state (recover-state starting-state action-fun @meta-state))
  env)

(defmethod action-core :meta-save
  [env cmd param]
  (spit "actions.edn" (str (apply str (interpose "\n" (historical-actions @meta-state))) "\n"))
  (reset! meta-state nil)
  env)

(defsnek command-snek nil indexes-snek "" -> [:_ nil])
(defn parse-input [commands parse-fun indexes s]
  (let [[_ cmd param] (re-matches #"([a-z]+)( +.+)?$" s)
        {:keys [cmd
                args]
         :as   command} (commands cmd)]
    (if @meta-state
      [({"u" :meta-up
         "d" :meta-down
         "s" :meta-save} s)
       nil]
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
        (or (apply parse-core cmd k) (apply parse-fun cmd k) [:nop nil])
        [:nop nil]))))

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
                     :noargs []}
                    {:desc   "[m]eta"
                     nil     []}
                    {:desc   "[un]do"
                     nil     []}])

(def alternate-commands (atom nil))

(def ^:dynamic inside-repl false)

(defn repl-println [& args]
  (when inside-repl
    (apply println args)))

(defn command-helper [render commands action-fun parse-fun s state auto-command starting-state]
  (let [{:keys [indexes]
         :as   rendering}              (render @state)
        [cmd param :as action-literal] (parse-input commands parse-fun indexes s)]
    (when (and @alternate-commands (not (#{:help :dump :quit :nop :meta :meta-up :meta-down :meta-save} cmd)))
      (reset! alternate-commands nil))
    (let [new-state (if (action-core {:state          state
                                      :action-fun     action-fun
                                      :starting-state starting-state
                                      :commands       commands}
                                     cmd
                                     param)
                      @state
                      (action-fun @state cmd param))]
      (if (= @state new-state)
        (println "##NO ACTION##")
        (spit "actions.edn" (str (pr-str action-literal) "\n") :append true))
      (let [{:keys [strings
                    indexes]
             :as   rendering} (render new-state)]
        (mapv println strings))
      (when @meta-state
        (mapv println (reverse (take 10 (map-indexed (fn [index item]
                                                       (str item
                                                            (when (= index @meta-state)
                                                              " *")))
                                                     (reverse (historical-actions 0))))))
        (println "[u]p\n[d]own\n[s]ave"))
      (if (and auto-command (not @meta-state))
        (let [action-literal (parse-fun auto-command nil nil)
              new-new-state  (apply action-fun new-state action-literal)]
          (when-not (= new-state new-new-state)
            (spit "actions.edn" (str (pr-str action-literal) "\n") :append true))
          [cmd new-new-state])
        [cmd new-state]))))

(defn temp-commands [commands]
  (reset! alternate-commands (massage-commands (concat core-commands commands))))

(defn recover-state [state action-fun backsteps]
  (reduce (fn [acc [cmd param :as item]]
            (reset! alternate-commands nil)
            (action-fun acc cmd param))
          state
          (map read-string
               (remove (fn [s]
                         (or (= s "") (re-matches #"#_.+" s)))
                       (historical-actions backsteps)))))

(defn command-fun [{:keys [state
                           commands
                           parse
                           action
                           render
                           auto-command]
                    :as   args}]
  (reset! alternate-commands nil)
  (reset! meta-state nil)
  (reset! starting-state @state)
  (swap! state recover-state action 0)
  (let [commands (massage-commands (concat core-commands commands))]
    (fn [s]
      (let [[cmd state-new] (command-helper render (or @alternate-commands commands) action parse s state auto-command @starting-state)]
        (when @alternate-commands
          (print-commands @alternate-commands))
        (reset! state state-new)
        cmd))))

(defn repl [command]
  (binding [inside-repl true]
    (command "h")
    (loop []
      (print "> ")
      (flush)
      (let [cmd (command (read-line))]
        (when-not (= cmd :quit)
          (recur))))))

(defn user-command [command]
  (binding [*ns* (create-ns 'user)]
    (eval `(def user/command ~command))))

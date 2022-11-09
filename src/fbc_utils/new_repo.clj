(ns fbc-utils.new-repo
  (:require [fbc-utils.debug]
            [fbc-utils.core :as ut]
            [fbc-utils.shell :as sh]))

(defn new-repo [nam]
  (println "Enter 'y' if you have created the repo" nam "at github.com")
  (when (= (read-line) "y")
    (println "creating repo...")
    (let [user-name (sh/! "whoami")
          user-name (apply str (butlast user-name))
          dir       (str "/home/" user-name "/" nam)]
      (when-not (ut/exists dir)
        (sh/! (str "mkdir " dir)))
      (println (sh/! (str "cd " dir ";git init;touch README.md;git add .;git commit -a -m \"wip\";git remote add origin git@github.com:" user-name "/" nam ".git;git push -u origin master"))))))

(new-repo (first *command-line-args*))
(shutdown-agents)

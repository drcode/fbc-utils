(ns tomo.cupdate-test
  (:require-macros [fbc-utils.debug :refer [??]])
  (:require [cljs.test :refer [deftest is]]
            [fbc-utils.debug :as db]
            [fbc-utils.core :as ut]
            [tomo.cupdate :as cu :refer [cupdate]]
            [tomo.test :refer-macros [test]]))

(test (cupdate {:foo 1} {:foo inc})
      {:foo 2}

      (cupdate {:foo {:bar 1}} {:foo {:bar inc}})
      {:foo {:bar 2}}

      (cupdate [1 2 3] [inc])
      [2 3 4]

      (cupdate {:foo 1} {:bar 2})
      {:foo 1
       :bar 2}

      (cupdate {:foo 1}
               {:bar (fn [k]
                       (if k
                         (inc k)
                         3))
                :foo inc})
      {:foo 2
       :bar 3}

      (cupdate {:foo [1 2]}
               {:foo [inc]})
      {:foo [2 3]}

      (cupdate {:foo 2
                :bar 5}
               {::cu/every inc})
      {:foo 3
       :bar 6}

      (cupdate {:foo 2
                :bar 5}
               {:bar ::cu/delete})
      {:foo 2}

      (cupdate {:foo 1}
               {:foo inc})
      {:foo 2}

      (cupdate {}
               {:foo [1 1 1]})
      {:foo [1 1 1]}

      (cupdate {:foo [0 0 0]}
               {:foo [1 1 1]})
      {:foo [1 1 1]}

      (cupdate {:foo [[1 2 3] [5 6 7]]}
               {:foo [[inc]]})
      {:foo [[2 3 4] [6 7 8]]}

      (cupdate [{:foo 3} {:foo 5}]
               [:foo])
      [3 5]
      
      (cupdate {:foo 3} :foo)
      3

      (cupdate [:a :b :c] {1 :derp})
      [:a :derp :c]

      (cupdate {:db/id [0 0 0]}
               {:db/id [1 0 0]})
      {:db/id [1 0 0]})

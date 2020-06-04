(ns fbc-utils.cliui-test
  (:refer-clojure :rename {test core-test})
  (:require [fbc-utils.debug :refer [??]]
            [fbc-utils.debug :as db]
            [fbc-utils.core :as ut]
            [fbc-utils.cliui :refer :all]
            [fbc-utils.test :refer [test]]))

(test (massage-commands [{:desc "play [t]est note"
                          :noargs []}])
      {"t" {:desc     "play [t]est note"
            :shortcut "t"
            :cmd      :play-test-note
            :args     [{:label :noargs
                        :snek  []}]}}
      (annotated-strings ["foo" {:text "bar" :derp 42} "baz" {:text "qux" :zup 77}])
      {:strings ["foo" "1. bar" "baz" "2. qux"]
       :indexes [{:derp 42} {:zup 77}]})

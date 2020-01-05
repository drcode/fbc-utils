(ns fbc-utils.style-test
  (:require [cljs.test :refer [deftest is]]
            [fbc-utils.test :refer-macros [test]]
            [fbc-utils.style :refer [style]]))

(test (style :m1010)
      {:margin-left  "1rem"
       :margin-right "1rem"}

      (style :m1)
      {:margin "1rem"}

      (style :bred)
      {:background-color "red"}

      (style :baaf)
      {:background-color "#aaf"}

      (style :m1:bred)
      {:background-color "red"
       :margin           "1rem"}

      (style :wrap)
      {:flex-wrap :wrap
       :display   :flex}

      (style :end:bblue)
      {:justify-content  :flex-end
       :background-color "blue"
       :display          :flex}

      (style :bblue:end
             {:max-width "25rem"})
      {:max-width        "25rem"
       :background-color "blue"
       :align-items      :flex-end
       :display          :flex})


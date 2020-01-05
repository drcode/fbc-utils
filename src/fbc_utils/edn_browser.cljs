(ns fbc-utils.edn-browser
  (:require [qlkit-renderer.core :as qr :refer-macros [defcomponent]]))

(def ellipse-threshold 30)

(defn ellipsize [item]
  (let [s (subs (pr-str item) 0 ellipse-threshold)]
    (cond-> s
      (= (count s) ellipse-threshold) (str "..."))))

(defcomponent Browser
  (render [{:keys [edn] :as atts} {:keys [visible path dirty] :as state}]
          (let [edn (if (= (type edn) (type (atom nil)))
                              @edn
                              edn)]
            (if visible
              (let [leaf (get-in edn path)]
                [:div {:display          :flex
                       :cursor           :default
                       :font-family      "Lucida Console"
                       :background-color :white
                       :on-click         (fn [e]
                                           (if (.-shiftKey e)
                                             (qr/update-state! dissoc :visible)
                                             (qr/update-state! update :dirty (fn [dirty]
                                                                               (inc (or dirty 0))))))
                       :justify-content  :center
                       :width            "100%"
                       :padding          "1rem"
                       :max-height       "40rem"
                       :min-height       "20rem"
                       :overflow         :auto}
                 [:div {:display :flex
                        :flex-direction :column
                        :margin-right "1rem"}
                  (map-indexed (fn [index key]
                                 [:div {:on-click (fn [e]
                                                    (qr/update-state! update
                                                                      :path
                                                                      (fn [path]
                                                                        (vec (take index path)))))}
                                  (str key)])
                               path)]
                 [:div {:display :flex
                        :flex-direction :column}
                  (cond (map? leaf) (map-indexed (fn [index key]
                                                   [:div {:on-click (fn [e]
                                                                      (qr/update-state! assoc :path (conj (or path []) key)))}
                                                    [:span (if (zero? index)
                                                           "{"
                                                           "\u00a0")
                                                     [:span {:color :blue} (pr-str key)]
                                                     " "
                                                     (ellipsize (leaf key))
                                                     (when (= index (dec (count leaf)))
                                                       "}")]])
                                                 (sort (keys leaf)))
                        (vector? leaf) (map-indexed (fn [index item]
                                                      [:div {:on-click (fn [e]
                                                                         (qr/update-state! assoc :path (conj (or path []) index)))}
                                                       (str (if (zero? index)
                                                              "["
                                                              "\u00a0")
                                                            (ellipsize (leaf index))
                                                            (when (= index (dec (count leaf)))
                                                              "]"))])
                                                    leaf)
                        :else (pr-str leaf))]])
              [:span {:on-click (fn [e]
                                  (when (.-shiftKey e)
                                    (qr/update-state! assoc :visible true)))}
               "\u00a0\u00a0"]))))

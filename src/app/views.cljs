(ns app.views
  (:require [reagent.core :refer [atom]]
            [app.state :refer [app-state]]
            [app.events :refer [increment decrement]]))

(def w (atom 10))
(def h (atom 10))
(def state (atom (vec (repeat @w (vec (repeat @h "red"))))))

(defn pixel-state [x y] (get-in @state [x y]))
(defn swap-pixel [x y val] (swap! state assoc-in [x y] val))

(defn pixel
  [x y options]
  [:div
   {:on-click #(swap-pixel x y "blue")
    :style {:width "20px"
            :height "20px"
            :background (pixel-state x y)
            :outline (when (:outline options) "2px solid black")}}])

(defn pixel-grid2
  [w h options]
  (into [:div]
        (for [x (range w)]
          [:span {:style {:float "left"}}
           (for [y (range h)]
             [pixel x y options])])))

(defn pixel-grid
  [w h options]
  (into [:div]
        (repeat w
                (into [:span {:style {:float "left"}}]
                      (repeat h [pixel options])))))

(defn app []
  [:div
   [pixel-grid2 10 10 {:outline true}]])


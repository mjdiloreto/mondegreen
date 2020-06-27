(ns app.events
  (:require [app.state :refer [app-state]]))

(def mockevent (clj->js {"preventDefault" #()}))

;(increment mockevent)

(defn increment
  [event]
  (.preventDefault event)
  (swap! app-state update-in [:count] inc))

(defn decrement
  [event]
  (.preventDefault event)
  (swap! app-state update-in [:count] dec))


(ns drawing.dom
  (:require [cljs.core.async :refer [chan put!]]))

(defn events
  ([target type] (events target type (chan)))
  ([target type out]
    (.addEventListener target type (fn [e] (put! out e)))
    out))

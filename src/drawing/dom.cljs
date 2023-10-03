(ns drawing.dom
  (:require [cljs.core.async :refer [chan put!]]))

(defn events [target type]
  (let [out (chan)]
    (.addEventListener target type (fn [e] (put! out e)))
    out))

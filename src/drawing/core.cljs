(ns drawing.core
  (:require [goog.dom :as dom]))

(defn ^:dev/after-load init []
  (let [id "drawing"
        width 608
        height 1080]
    (when-not (dom/getElement id)
      (dom/append js/document.body (dom/createDom "canvas" #js {:id id})))
    (let [canvas (dom/getElement id)
          ctx (.getContext canvas "2d")]
      (dom/setProperties canvas #js {:width width :height height})
      (.fillRect ctx 0 0 width height))))

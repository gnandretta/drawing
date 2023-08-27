(ns drawing.canvas
  (:require [goog.dom :as dom]))

(defn draw [drawing-var]
  (let [{:keys [width height] :as mt} (meta drawing-var)
        id (name (:name mt))]
    (when-not (dom/getElement id)
      (dom/append js/document.body (dom/createDom "canvas" #js {:id id})))
    (let [canvas (dom/getElement id)]
      (dom/setProperties canvas #js {:width width :height height})
      (@drawing-var {:ctx (.getContext canvas "2d")
                     :width width
                     :height height}))))

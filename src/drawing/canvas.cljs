(ns drawing.canvas
  (:require [goog.dom :as dom]))

(defn draw*
  ([f] (draw* {} f))
  ([mt f] (let [{:keys [width height]} mt
                id (name (:name mt))]
            (when-not (dom/getElement id)
              (dom/append js/document.body (dom/createDom "canvas" #js {:id id})))
            (let [canvas (dom/getElement id)]
              (dom/setProperties canvas #js {:width width :height height})
              (f {:ctx    (.getContext canvas "2d")
                  :width  width
                  :height height})))))

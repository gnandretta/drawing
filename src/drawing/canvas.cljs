(ns drawing.canvas
  (:require [goog.dom :as dom]))

(def default-drawing-mt {:name   "drawing"
                         :width  600
                         :height 600})

(defn draw*
  ([f] (draw* {} f))
  ([mt f] (let [mt (merge default-drawing-mt mt)
                {:keys [width height]} mt
                id (name (:name mt))]
            (when-not (dom/getElement id)
              (dom/append js/document.body (dom/createDom "canvas" #js {:id id})))
            (let [canvas (dom/getElement id)]
              (dom/setProperties canvas #js {:width width :height height})
              (f {:ctx    (.getContext canvas "2d")
                  :width  width
                  :height height})))))

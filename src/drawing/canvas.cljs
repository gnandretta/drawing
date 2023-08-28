(ns drawing.canvas
  (:require [goog.object :as object]
            [goog.dom :as dom]))

(def ^:dynamic *ctx* nil)

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
            (let [canvas (dom/getElement id)
                  ctx (.getContext canvas "2d")]
              (dom/setProperties canvas #js {:width width :height height})
              (binding [*ctx* ctx]
                (f {:ctx    ctx
                    :width  width
                    :height height}))))))

(defn set-fill-style
  "Sets the CSS color, gradient, or pattern to use inside shapes. \"#000\" by
   default."
  [value]
  (object/set *ctx* "fillStyle" value))

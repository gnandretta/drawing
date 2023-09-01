(ns drawing.canvas
  (:require [goog.object :as object]
            [goog.dom :as dom]))

(def ^:dynamic *ctx* nil)

(def default-drawing-mt {:name   "drawing"
                         :dimensions [600 600]})

(defn draw*
  ([f] (draw* {} f))
  ([mt f] (let [mt (merge default-drawing-mt mt)
                {:keys [dimensions]} mt
                [width height] dimensions
                id (name (:name mt))]
            (when-not (dom/getElement id)
              (dom/append js/document.body (dom/createDom "canvas" #js {:id id})))
            (let [canvas (dom/getElement id)
                  ctx (.getContext canvas "2d")]
              (dom/setProperties canvas #js {:width width :height height})
              (binding [*ctx* ctx]
                (f {:ctx    ctx
                    :dimensions dimensions
                    :width  (partial * width)
                    :height height}))))))

(defn- sp [nm value]                                        ; short for set property
  (object/set *ctx* nm value))

(defn- im [nm & args]                                       ; short for invoke method
  (.apply (object/get *ctx* nm) *ctx* (into-array args)))

(defn set-fill-style
  "Sets the CSS color, gradient, or pattern to use inside shapes. \"#000\" by
   default."
  [value]
  (sp "fillStyle" value))

(defn fill-rect
  "Draws a rectangle filled according fill-style without modifying the current
   path. Positive values of w and h are to the right and down, respectively.
   Negative values to the left and up."
  [[x y] [w h]]
  (im "fillRect" x y w h))

(defn translate
  "Adds a translation transformation to the current matrix by moving the canvas
   origin the given units."
  [[x y]]
  (im "translate" x y))

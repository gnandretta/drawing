(ns drawing.canvas
  (:require [goog.dom :as dom]
            [goog.object :as object]))

(def ^:dynamic *ctx* nil)
(def ^:dynamic *dimensions* nil)
(def ^:dynamic *dpi* nil)

(def default-drawing-mt {:name   "drawing"
                         :size   [600 600]
                         :margin [0]
                         :dpi    300})

(def paper-mms {:a3 [297 420]})

(defn d
  "Multiplies the drawing's dimensions by the given numbers, returning
   proportional dimensions."
  [& n]
  (map (apply partial * n) (:content *dimensions*)))

(defn w
  "Multiples the drawing's width by the given numbers, returning a proportional
   width."
  [& n]
  (apply * (first (:content *dimensions*)) n))

(defn h
  "Multiples the drawing's height by the given numbers, returning a proportional
   height."
  [& n]
  (apply * (second (:content *dimensions*)) n))

(defn mm [x]
  (let [f #(js/Math.round (/ (* % *dpi*) 25.4))]            ; 1 inch = 25.4 mm
    (if (number? x)
      (f x)
      (mapv f x))))

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

(defn reset-transform
  "Resets the current transformation matrix to the identity matrix."
  []
  (im "resetTransform"))

(defn translate
  "Adds a translation transformation to the current matrix by moving the canvas
   origin the given units."
  [[x y]]
  (im "translate" x y))

(defn- expand-margin [v]
  (case (count v)
    1 (vec (repeat 4 (first v)))
    2 (let [[tb lr] v] [tb lr tb lr])
    3 (let [[t lr b] v] [t lr b lr])
    v))

(defn- compute-layout [size paper margin]
  (let [[mt mr mb ml :as margin] (cond-> (expand-margin margin) paper mm)
        [w h :as canvas] (if paper
                           (mm (cond-> paper symbol? paper-mms))
                           size)]
    {:dimensions {:canvas  canvas
                  :content [(- w ml mr) (- h mt mb)]}
     :margin     margin}))

(defn- draw-margin [[t r b l]]                              ; this isn't drawing anything when there's no margin
  (let [[cw ch] (:canvas *dimensions*)]
    (reset-transform)
    (set-fill-style "white")
    (fill-rect [0 0] [cw t])
    (fill-rect [(- cw r) t] [r (- ch t b)])
    (fill-rect [0 (- ch b)] [cw b])
    (fill-rect [0 t] [l (- ch t b)])))

(defn draw*
  [mt f & args]
  (let [{:keys [size paper dpi margin] :as mt} (merge default-drawing-mt mt)
        id (name (:name mt))]
    (when-not (dom/getElement id)
      (dom/append js/document.body (dom/createDom "canvas" #js {:id id})))
    (binding [*dpi* dpi]
      (let [canvas (dom/getElement id)
            {:keys [dimensions margin]} (compute-layout size paper margin)]
        (dom/setProperties canvas (clj->js (zipmap [:width :height] (:canvas dimensions))))
        (binding [*ctx* (.getContext canvas "2d")
                  *dimensions* dimensions]
          (let [[mt _ _ ml] margin] (translate [mt ml]))
          (apply f args)
          (draw-margin margin))))))

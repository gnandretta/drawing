(ns drawing.canvas
  (:refer-clojure :exclude [print])
  (:require [goog.dom :as dom]
            [goog.object :as object]))

(def ^:dynamic *dimensions* nil)
(def ^:dynamic *dpi* nil)

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

(defn- sp [ctx nm value]                                    ; short for set property
  (object/set ctx nm value)
  ctx)

(defn- im [ctx nm & args]                                   ; short for invoke method
  (.apply (object/get ctx nm) ctx (into-array args))
  ctx)

(defn set-fill-style
  "Sets the CSS color, gradient, or pattern to use inside shapes. \"#000\" by
   default."
  [ctx value]
  (sp ctx "fillStyle" value))

(defn set-line-width
  "Sets the thickness of lines, in coordinate space units. 1.0 by default,
   zero, negative, Infinity, and NaN values are ignored."   ; see scale() to know more about "coordinate space units"
  [ctx value]
  (sp ctx "lineWidth" value))

(defn set-stroke-style
  "Sets the CSS color, gradient, or pattern to use for shapes outlines. \"#000\"
   by default."
  [ctx value]
  (sp ctx "strokeStyle" value))

(defn begin-path
  "Creates a new path by emptying the list of sub-paths."   ; TODO what's the difference between path and sub-path?
  [ctx]
  (im ctx "beginPath"))

(defn fill                                                  ; TODO implement other arities and doc rules from https://en.wikipedia.org/wiki/Nonzero-rule and https://en.wikipedia.org/wiki/Even–odd_rule (see stroke docs).
  "Fills the current or given path with the current fill-style. fill-rule
  specifies the algorithm that determines if a point is inside or outside the
  filling region. \"nonzero\" by default and the other possible value is
  \"evenodd\"."
  [ctx]
  (im ctx "fill"))

(defn fill-rect
  "Draws a rectangle filled according to fill-style without modifying the
   current path. Positive values of w and h are to the right and down,
   respectively. Negative values to the left and up."
  [ctx [x y] [w h]]
  (im ctx "fillRect" x y w h))

(defn rect
  "Adds a rectangle to the current path."                   ; TODO doc all values can be negative like in fill-rect, or remove from fill-rect's doc if it's a general pattern
  [ctx [x y] [w h]]
  (im ctx "rect" x y w h))

(defn reset-transform
  "Resets the current transformation matrix to the identity matrix."
  [ctx]
  (im ctx "resetTransform"))

(defn stroke                                                ; TODO implement other arities
  "Outlines the current or given path with the current stroke-style. Strokes
   are aligned to the center of a path (half of the stroke is drawn on the
   inner side, and half on the outer side) and drawn using the non-zero winding
   rule (path intersections will still get filled)."
  [ctx]
  (im ctx "stroke"))

(defn stroke-rect
  "Draws an outlined rectangle according to stroke-style without modifying the
   current path."                                           ; TODO same as rect
  [ctx [x y] [w h]]
  (im ctx "strokeRect" x y w h))

(defn translate
  "Adds a translation transformation to the current matrix by moving the canvas
   origin the given units."
  [ctx [x y]]
  (im ctx "translate" x y))

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

(defn- draw-margin [ctx [t r b l]]                          ; this isn't drawing anything when there's no margin
  (let [[cw ch] (:canvas *dimensions*)]
    (-> ctx
        (reset-transform)
        (set-fill-style "white")
        (fill-rect [0 0] [cw t])
        (fill-rect [(- cw r) t] [r (- ch t b)])
        (fill-rect [0 (- ch b)] [cw b])
        (fill-rect [0 t] [l (- ch t b)]))))

(defn resize [canvas size]
  (dom/setProperties canvas (clj->js (zipmap [:width :height] size))))

(defn create [id size]
  (when-not (dom/getElement id)
    (dom/append js/document.body (dom/createDom "canvas" #js {:id id})))
  (let [canvas (dom/getElement id)]
    (resize canvas size)                                    ; TODO not sure about resizing during creation
    canvas))

(defn ctx [canvas]
  (.getContext canvas "2d"))

(defn print                                                 ; TODO not sure about the name
  [f & {:keys [id size paper dpi margin]
        :or   {id     (-> (meta f) (get :name "drawing") name)
               size   [600 600]
               margin [0]
               dpi    300}
        :as   kwargs}]
  (binding [*dpi* dpi]
    (let [{:keys [dimensions margin]} (compute-layout size paper margin)
          canvas (create id (:canvas dimensions))
          ctx (.getContext canvas "2d")]
      (binding [*dimensions* dimensions]
        (let [[mt _ _ ml] margin] (translate ctx [mt ml]))
        ((if (var? f) @f f) (assoc kwargs :ctx ctx))
        (draw-margin ctx margin)))))

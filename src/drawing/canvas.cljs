(ns drawing.canvas
  (:require [goog.dom :as dom]
            [goog.object :as object]))

(def paper-mms {:a3 [297 420]})

(defn d
  "Multiplies the drawing's dimensions by each the given numbers or dimensions,
   returning proportional dimensions."
  [size & xs]
  (reduce (fn [acc x]
            (mapv * acc (if (vector? x) x [x x])))
          size
          xs))

(defn w
  "Multiples the drawing's width by the given numbers, returning a proportional
   width."
  [width & ns]
  (apply * width ns))

(defn h
  "Multiples the drawing's height by the given numbers, returning a proportional
   height."
  [height & ns]
  (apply * height ns))

(defn mm [dpi x]
  (let [f #(js/Math.round (/ (* % dpi) 25.4))]              ; 1 inch = 25.4 mm
    (if (number? x) (f x) (mapv f x))))

(defn set-fill-style
  "Sets the CSS color, gradient, or pattern to use inside shapes. \"#000\" by
   default."
  [ctx value]
  (object/set ctx "fillStyle" value)
  ctx)

(defn set-font
  "Sets the CSS font to use when drawing text."
  [ctx value]
  (object/set ctx "font" value)
  ctx)

(defn set-line-width
  "Sets the thickness of lines, in coordinate space units. 1.0 by default,
   zero, negative, Infinity, and NaN values are ignored."   ; see scale() to know more about "coordinate space units"
  [ctx value]
  (object/set "lineWidth" value)
  ctx)

(defn set-stroke-style
  "Sets the CSS color, gradient, or pattern to use for shapes outlines. \"#000\"
   by default."
  [ctx value]
  (object/set ctx "strokeStyle" value)
  ctx)

(defn set-text-align
  "Sets the text alignment. \"start\" by default, other possible values are
   \"end\", \"left\", \"right\", and \"center\"."
  [ctx value]
  (object/set ctx "textAlign" value)
  ctx)

(defn set-text-baseline
  "Sets the text baseline. \"alphabetic\" by default, other possible values are
   \"top\", \"hanging\", \"middle\", \"ideographic\", and \"bottom\"."
  [ctx value]
  (object/set ctx "textBaseline" value)
  ctx)

(defn arc
  "Adds a circular arc centered at (x,y) to the current sub-path, clockwise by
   default."                                                ; TODO doc somewhere how angles are measured (radians from teh positive x-axis) and letter meaning
  ([ctx [x y] r a-start a-end]
   (.arc ctx x y r a-start a-end)
   ctx)
  ([ctx [x y] r a-start a-end counter-clockwise]
   (.arc ctx x y r a-start a-end counter-clockwise)
   ctx))

(defn begin-path
  "Creates a new path by emptying the list of sub-paths."   ; TODO what's the difference between path and sub-path?
  [ctx]
  (.beginPath ctx)
  ctx)

(defn fill                                                  ; TODO implement other arities and doc rules from https://en.wikipedia.org/wiki/Nonzero-rule and https://en.wikipedia.org/wiki/Even–odd_rule (see stroke docs).
  "Fills the current or given path with the current fill-style. fill-rule
  specifies the algorithm that determines if a point is inside or outside the
  filling region. \"nonzero\" by default and the other possible value is
  \"evenodd\"."
  [ctx]
  (.fill ctx)
  ctx)

(defn fill-rect
  "Draws a rectangle filled according to fill-style without modifying the
   current path. Positive values of w and h are to the right and down,
   respectively. Negative values to the left and up."
  [ctx [x y] [w h]]
  (.fillRect ctx x y w h)
  ctx)

(defn fill-text                                             ; TODO implement arity with max-width
  "Draws text at (x,y) using the font and text layout defined by font,
   text-align, text-baseline and direction, and filling characters according to
   fill-style."
  [ctx s [x y]]
  (.fillText ctx s x y)
  ctx)

(defn line-to
  "Adds a straight line to the current sub-path from its last point to (x,y)."
  [ctx [x y]]
  (.lineTo ctx x y)
  ctx)

(defn measure-text
  "Returns an object (that implements TextMetrics) with the dimensions of s."
  [ctx s]
  (.measureText ctx s))

(defn move-to
  "Creates a new sub-path that beings at (x,y)."
  [ctx [x y]]
  (.moveTo ctx x y)
  ctx)

(defn rect
  "Adds a rectangle to the current path."                   ; TODO doc all values can be negative like in fill-rect, or remove from fill-rect's doc if it's a general pattern
  [ctx [x y] [w h]]
  (.rect ctx x y w h)
  ctx)

(defn reset-transform
  "Resets the current transformation matrix to the identity matrix."
  [ctx]
  (.resetTransform ctx)
  ctx)

(defn restore
  "Pops from the drawing state stack and makes it the current drawing state.
   Does nothing if the stack is empty."
  [ctx]
  (.restore ctx)
  ctx)

(defn rotate
  "Adds a rotation (around the canvas origin) to the transformation matrix."
  [ctx a]
  (.rotate ctx a)
  ctx)

(defn save
  "Pushes the current drawing state onto a stack."          ; TODO doc what makes the drawing state
  [ctx]
  (.save ctx)
  ctx)

(defn stroke                                                ; TODO implement other arities
  "Outlines the current or given path with the current stroke-style. Strokes
   are aligned to the center of a path (half of the stroke is drawn on the
   inner side, and half on the outer side) and drawn using the non-zero winding
   rule (path intersections will still get filled)."
  [ctx]
  (.stroke ctx)
  ctx)

(defn stroke-rect
  "Draws an outlined rectangle according to stroke-style without modifying the
   current path."                                           ; TODO same as rect
  [ctx [x y] [w h]]
  (.strokeRect ctx x y w h)
  ctx)

(defn translate
  "Adds a translation transformation to the current matrix by moving the canvas
   origin the given units."
  [ctx [x y]]
  (.translate ctx x y)
  ctx)

(defn- expand-margin [v]
  (case (count v)
    1 (vec (repeat 4 (first v)))
    2 (let [[tb lr] v] [tb lr tb lr])
    3 (let [[t lr b] v] [t lr b lr])
    v))

(defn- compute-layout [size paper dpi margin]
  (let [[mt mr mb ml :as margin] (cond->> (expand-margin margin) paper (mm dpi))
        [w h :as canvas] (if paper
                           (mm dpi (cond-> paper symbol? paper-mms))
                           size)]
    {:dimensions {:canvas  canvas
                  :content [(- w ml mr) (- h mt mb)]}
     :margin     margin}))

(defn- draw-margin                                          ; this isn't drawing anything when there's no margin
  ([ctx size margin] (draw-margin ctx size margin "white"))
  ([ctx [width height] [t r b l] color]
   (-> ctx
       (reset-transform)
       (set-fill-style color)
       (fill-rect [0 0] [width t])
       (fill-rect [(- width r) t] [r (- height t b)])
       (fill-rect [0 (- height b)] [width b])
       (fill-rect [0 t] [l (- height t b)]))))

(defn- size->properties [size]
  (clj->js (zipmap [:width :height] size)))

(def default-ctx-type "2d")

(defn create
  ([size] (create size default-ctx-type))
  ([size ctx-type]
   (.getContext (dom/createDom "canvas" (size->properties size)) ctx-type)))

(defn append
  ([id size] (append id size default-ctx-type))
  ([id size ctx-type]
   (let [id (-> id symbol str)]                             ; ns-qualified name of a keyword (without :) or same string
     (when-not (dom/getElement id)
       (dom/append js/document.body (dom/createDom "canvas" #js {:id id})))
     (let [canvas (dom/getElement id)]
       (dom/setProperties canvas (size->properties size))
       (.getContext canvas ctx-type)))))

(defn layout [& {:keys [dpi margin paper size]
                 :or   {size   [600 600]
                        margin [0]
                        dpi    300}}]
  (let [[mt mr mb ml :as margin] (cond->> (expand-margin margin) paper (mm dpi)) ; TODO don't assume paper margin in mm
        [width height :as canvas] (if paper
                                    (mm dpi (cond-> paper symbol? paper-mms))
                                    size)
        content [(- width ml mr) (- height mt mb)]]
    {:dimensions  {:canvas canvas :content content}
     :margin      margin
     :top-left    [mt mr]                                   ; TODO maybe add a corners/edges map with each of them?
     :d           (partial d content)                       ; TODO are the letter fns worth it? i.e., (w 0.5) vs (* w 0.5)
     :w           (partial w width)
     :h           (partial h height)
     :draw-margin (fn                                       ; TODO is this fn worth it?
                    ([ctx] (draw-margin ctx canvas margin))
                    ([ctx color] (draw-margin ctx canvas margin color)))}))

#_(defn print                                               ; TODO not sure about the name
    [f & {:keys [id size paper dpi margin]
          :or   {id     (-> (meta f) (get :name "drawing") name)
                 size   [600 600]
                 margin [0]
                 dpi    300}
          :as   kwargs}]
    (binding [*dpi* dpi]
      (let [{:keys [dimensions margin]} (compute-layout size paper dpi margin)
            canvas (append id (:canvas dimensions))
            ctx (.getContext canvas "2d")]
        (binding [*dimensions* dimensions]
          (let [[mt _ _ ml] margin] (translate ctx [mt ml]))
          ((if (var? f) @f f) (assoc kwargs :ctx ctx))
          (draw-margin ctx margin)))))

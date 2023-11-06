(ns drawing.canvas
  (:refer-clojure :exclude [apply get set])
  (:require [drawing.math :as m]
            [goog.dom :as dom]
            [goog.object :as object]))

(def paper-mms {:a3 [297 420]})

(defn mm [dpi x]
  (let [f #(js/Math.round (/ (* % dpi) 25.4))]              ; 1 inch = 25.4 mm
    (if (number? x) (f x) (mapv f x))))

(defn get
  ([ctx] (get ctx "canvas"))
  ([ctx nm] (object/get ctx nm)))                           ; TODO not sure about this arity, maybe take keywords?

(defn set
  "Sets a property on a drawing context and returns the context."
  [ctx nm value]
  (object/set ctx nm value)
  ctx)

(defn call
  "Calls a method on a drawing context and returns the context."
  [ctx nm & args]
  (.apply (object/get ctx nm) ctx (into-array args))
  ctx)

(defn apply
  "Like call except args are a seq, but when a fn is provided instead of a
   method name calls clojure.core/apply on the fn—which comes in handy in a
   thread-first macro (->)."
  [ctx nm-or-f args]
  (if (fn? nm-or-f)
    (clojure.core/apply nm-or-f ctx args)
    (clojure.core/apply call ctx nm-or-f args)))

(defn set-fill-style
  "Sets the CSS color, gradient, or pattern to use inside shapes. \"#000\" by
   default."
  [ctx value]
  (object/set ctx "fillStyle" (name value))
  ctx)

(defn set-font
  "Sets the CSS font to use when drawing text."
  [ctx value]
  (object/set ctx "font" value)
  ctx)

(defn set-global-composite-operation
  "Sets the compositing or blending mode operations when drawing shapes."
  [ctx value]
  (object/set ctx "globalCompositeOperation" (name value))
  ctx)

(defn set-line-cap
  "Sets the shape used to draw the end of lines. :butt by default, other
   possible values are :round and :square, and add half of the line's width." ; TODO review doc
  [ctx value]
  (object/set ctx "lineCap" (name value))
  ctx)

(defn set-line-width
  "Sets the thickness of lines, in coordinate space units. 1.0 by default,
   zero, negative, Infinity, and NaN values are ignored."   ; see scale() to know more about "coordinate space units"
  [ctx value]
  (object/set ctx "lineWidth" value)
  ctx)

(defn set-shadow-color
  "Sets the shadow color. rgba(0,0,0,0) by default—fully transparent black, not
   rendered as any fully transparent shadow."
  [ctx value]
  (object/set ctx "shadowColor" value)
  ctx)

(defn set-shadow-offset
  "Sets the distance shadows will be offset, vertically and horizontally. [0,0]
   by default, ignores Infinity and NaN values."
  [ctx [x y]]
  (object/set ctx "shadowOffsetX" x)
  (object/set ctx "shadowOffsetY" y)
  ctx)

(defn set-stroke-style
  "Sets the CSS color, gradient, or pattern to use for shapes outlines. \"#000\"
   by default."
  [ctx value]
  (object/set ctx "strokeStyle" (name value))
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
  "Adds a circular arc centered at (0,0) by default to the current sub-path,
   clockwise."                                              ; TODO doc somewhere how angles are measured (radians from teh positive x-axis) and letter meaning
  ([ctx r a-start a-end] (arc ctx [0 0] r a-start a-end))   ; TODO simplify a-start = 0
  ([ctx [x y] r a-start a-end]
   (.arc ctx x y r a-start a-end)
   ctx))

(defn arc-to
  "Adds a circular arc to the current sub-path connected to the last point with
   a straight line."
  [ctx [ax ay] [bx by] r]
  (.arcTo ctx ax ay bx by r)
  ctx)

(defn arcc
  "Adds a circular arc centered at (0,0) by default to the current sub-path,
   counterclockwise."
  ([ctx r a-start a-end] (arcc ctx [0 0] r a-start a-end))
  ([ctx [x y] r a-start a-end]
   (.arc ctx x y r a-start a-end true)
   ctx))

(defn begin-path
  "Creates a new path by emptying the list of sub-paths."   ; TODO what's the difference between path and sub-path?
  [ctx]
  (.beginPath ctx)
  ctx)

(defn bezier-curve-to
  "Adds a cubic Bézier curve to the current sub-path from its last point to
   (x,y) with the control points (ax,ay) amd (bx,by)."
  [ctx [ax ay] [bx by] [x y]]
  (.bezierCurveTo ctx ax ay bx by x y)
  ctx)

(defn clear-rect
  "Sets pixels in the given rectangle to transparent black without affecting
   the current path."
  ([ctx d] (clear-rect ctx [0 0] d))
  ([ctx [x y] [w h]]
   (.clearRect ctx x y w h)
   ctx))

(defn clip
  "Makes the current path the current clipping region, if there isn't any.
   Otherwise, it makes the intersection of the clipping region and path the
   new current clipping region."
  [ctx]                                                     ; TODO implement other arities
  (.clip ctx)
  ctx)

(defn close-path
  "Adds a straight line to the current sub-path from its last point to its
   first point unless they are the same point—the shape is closed or has a
   single point."
  [ctx]
  (.closePath ctx)
  ctx)

(defn fill                                                  ; TODO implement other arities and doc rules from https://en.wikipedia.org/wiki/Nonzero-rule and https://en.wikipedia.org/wiki/Even–odd_rule (see stroke docs).
  "Fills the current or given path with the current fill-style. fill-rule
  specifies the algorithm that determines if a point is inside or outside the
  filling region. \"nonzero\" by default and the other possible value is
  \"evenodd\"."
  ([ctx]
   (.fill ctx)
   ctx)
  ([ctx path]
   (.fill ctx path)
   ctx))

(defn fill-rect
  "Draws a rectangle from filled according to fill-style without modifying the
   current path, starting at (0,0) by default—which vertex is drawn at (x,y)
   depends on whether w and h are positive or negative."
  ([ctx d] (fill-rect ctx [0 0] d))
  ([ctx [x y] [w h]]
   (.fillRect ctx x y w h)
   ctx))

(defn fill-text                                             ; TODO implement arity with max-width
  "Draws text at (0,0) by default using the font and text layout defined by
   font, text-align, text-baseline and direction, and filling characters
   according to fill-style."
  ([ctx s] (fill-text ctx s [0 0]))
  ([ctx s [x y]]
   (.fillText ctx s x y)
   ctx))

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
  "Creates a new sub-path that begins at (x,y)."
  [ctx [x y]]
  (.moveTo ctx x y)
  ctx)

(defn move-to-origin                                        ; TODO do i really want this?
  "Creates a new sub-path that begins at (0,0)."
  [ctx]
  (move-to ctx [0 0]))

(defn quadratic-curve-to
  "Adds a quadratic Bézier curve to the current sub-path from its last point to
   (x,y) with the control point (cx,cy)."
  [ctx [cx cy] [x y]]
  (.quadraticCurveTo ctx cx cy x y)
  ctx)

(defn rect
  "Adds a rectangle to the current path."                   ; TODO doc all values can be negative like in fill-rect, or remove from fill-rect's doc if it's a general pattern
  ([ctx d] (rect ctx [0 0] d))
  ([ctx [x y] [w h]]
   (.rect ctx x y w h)
   ctx))

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

(defn round-rect
  "Adds a rounded rectangle to the current path with the given radii—which works
   pretty much like CSS border-radius property."
  ([ctx d radii] (round-rect ctx [0 0] d radii))            ; radii is the plural of radius, also radiuses
  ([^js/CanvasRenderingContext2D ctx [x y] [w h] radii]
   (.roundRect ctx x y w h radii)
   ctx))

(defn save
  "Pushes the current drawing state onto a stack."          ; TODO doc what makes the drawing state
  [ctx]
  (.save ctx)
  ctx)

(defn scale
  "Adds a scaling transformation to the current matrix by changing how many
   pixels correspond to a coordinate space unit, 1 unit = 1 pixel by default.
   Negative values flip pixels across the corresponding axis."
  [ctx [x y]]
  (.scale ctx x y)
  ctx)

(defn stroke
  "Outlines the current or given path with the current stroke-style. Strokes
   are aligned to the center of a path (half of the stroke is drawn on the
   inner side, and half on the outer side) and drawn using the non-zero winding
   rule (path intersections will still get filled)."
  ([ctx]
   (.stroke ctx)
   ctx)
  ([ctx path]
   (.stroke ctx path)
   ctx))

(defn stroke-rect
  "Draws an outlined rectangle according to stroke-style without modifying the
   current path."                                           ; TODO same as rect
  ([ctx d] (stroke-rect ctx [0 0] d))
  ([ctx [x y] [w h]]
   (.strokeRect ctx x y w h)
   ctx))

(defn transform                                             ; TODO add skew fn?
  "Multiplies the current transformation matrix by the given matrix, which are
   described by a c e
                b d f
                0 0 1, and transform coordinates into (ax+cy+e,bx+dy+f). When
   b = c = 0, a and d control horizontal and vertical scaling. When a = d = 1,
   b and c control horizontal and vertical skewing. e and f control horizontal
   and vertical translation."
  [ctx a b c d e f]
  (.transform ctx a b c d e f)
  ctx)

(defn translate
  "Adds one or more translation transformations to the current matrix by moving
   the canvas origin the given units."
  [ctx & xys]
  (doseq [[x y] xys] (.translate ctx x y))
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

; TODO take size from ctx, stop setting fill style, make public, and rename to fill-margin
(defn- draw-margin                                          ; this isn't drawing anything when there's no margin
  ([ctx size margin] (draw-margin ctx size margin "white"))
  ([ctx [width height] [t r b l] color]
   (-> ctx
       (save)
       (reset-transform)
       (set-fill-style color)
       (fill-rect [0 0] [width t])
       (fill-rect [(- width r) t] [r (- height t b)])
       (fill-rect [0 (- height b)] [width b])
       (fill-rect [0 t] [l (- height t b)])
       (restore))))

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
     :d           (partial m/v* content)                    ; TODO are the letter fns worth it? i.e., (w 0.5) vs (* w 0.5)
     :w           (partial * width)
     :h           (partial * height)
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

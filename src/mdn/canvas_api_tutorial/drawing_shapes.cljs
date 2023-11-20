(ns mdn.canvas-api-tutorial.drawing-shapes
  (:require [drawing.canvas :as c :refer [j>]]
            [drawing.math :as m]))

(defn drawing-shapes []                                     ; https://developer.mozilla.org/en-US/docs/Web/API/Canvas_API/Tutorial/Drawing_shapes#rectangular_shape_example
  (-> (c/append ::drawing-shapes [150 150])
      (c/fill-rect [25 25] [100 100])
      (c/clear-rect [45 45] [60 60])
      (c/stroke-rect [50 50] [50 50])))

(defn drawing-a-triangle []                                 ; https://developer.mozilla.org/en-US/docs/Web/API/Canvas_API/Tutorial/Drawing_shapes#drawing_a_triangle
  (-> (c/append ::drawing-a-triangle [150 150])
      (c/begin-path)
      (c/move-to [75 50])
      (c/line-to [100 75])
      (c/line-to [100 25])
      (c/fill)))

(defn moving-the-pen []                                     ; https://developer.mozilla.org/en-US/docs/Web/API/Canvas_API/Tutorial/Drawing_shapes#moving_the_pen
  (-> (c/append ::moving-the-pen [150 150])
      (c/begin-path)
      (c/circle [75 75] 50)
      (c/move-to [110 75])                                  ; remove c/move-to calls to see connecting lines
      (c/arc [75 75] 35 (m/pii))
      (c/move-to [65 65])
      (c/circle [60 65] 5)
      (c/move-to [95 65])
      (c/circle [90 65] 5)
      (c/stroke)))

(defn lines []                                              ; https://developer.mozilla.org/en-US/docs/Web/API/Canvas_API/Tutorial/Drawing_shapes#lines
  (-> (c/append ::lines [150 150])
      (c/begin-path)
      (c/move-to [25 25])
      (c/line-to [105 25])
      (c/line-to [25 105])
      (c/fill)                                              ; threats sub-path as closed
      (c/begin-path)
      (c/move-to [125 125])
      (c/line-to [125 45])
      (c/line-to [45 125])
      (c/close-path)                                        ; must explicitly close the sub-path
      (c/stroke)))

(defn arcs []                                               ; https://developer.mozilla.org/en-US/docs/Web/API/Canvas_API/Tutorial/Drawing_shapes#arcs
  (let [ctx (c/append ::arcs [150 200])]
    (doseq [i (range 4) j (range 3) :let [xy (map #(+ 25 (* 50 %)) [j i])
                                          r 20
                                          a-start 0
                                          a-end (m/pi (+ 1 (/ j 2)))]]
      (-> ctx
          (c/begin-path)
          ((if (even? i) c/arc c/arcc) xy r [a-start a-end])
          ((if (> i 1) c/fill c/stroke))))))

(defn quadratic-bezier-curves []                            ; https://developer.mozilla.org/en-US/docs/Web/API/Canvas_API/Tutorial/Drawing_shapes#quadratic_bezier_curves
  (-> (c/append ::quadratic-bezier-curves [150 150])
      (c/begin-path)
      (c/move-to [75 25])
      (c/quadratic-curve-to [25 25] [25 62.5])
      (c/quadratic-curve-to [25 100] [50 100])
      (c/quadratic-curve-to [50 120] [30 125])
      (c/quadratic-curve-to [60 120] [65 100])
      (c/quadratic-curve-to [125 100] [125 62.5])
      (c/quadratic-curve-to [125 25] [75 25])
      (c/stroke)))

(defn cubic-bezier-curves []                                ; https://developer.mozilla.org/en-US/docs/Web/API/Canvas_API/Tutorial/Drawing_shapes#cubic_bezier_curves
  (-> (c/append ::cubic-bezier-curves [150 150])
      (c/begin-path)
      (c/move-to [75 40])
      (c/bezier-curve-to [75 37] [70 25] [50 25])
      (c/bezier-curve-to [20 25] [20 62.5] [20 62.5])
      (c/bezier-curve-to [20 80] [40 102] [75 120])
      (c/bezier-curve-to [110 102] [130 80] [130 62.5])
      (c/bezier-curve-to [130 62.5] [130 25] [100 25])
      (c/bezier-curve-to [85 25] [75 37] [75 40])
      (c/fill)))

(defn making-combinations []                                ; https://developer.mozilla.org/en-US/docs/Web/API/Canvas_API/Tutorial/Drawing_shapes#making_combinations
  (let [ctx (c/append ::making-combinations [150 150])]
    (-> ctx
        (c/round-rect [12 12] [150 150] 15)                 ; bg
        (c/round-rect [19 19] [150 150] 9)
        (c/round-rect [53 53] [49 33] 10)
        (c/round-rect [53 119] [49 16] 6)
        (c/round-rect [135 53] [49 33] 10)
        (c/round-rect [135 119] [25 49] 10)
        (c/stroke)

        (c/begin-path)                                      ; pacman
        (c/arc [37 37] 13 (m/pii (/ 1 7) (/ 1 -7)))
        (c/line-to [31 37])
        (c/fill)

        (j>                                                 ; fruits
          (doseq [x (m/steps 8 51 6)] (c/fill-rect ctx [x 35] [4 4]))
          (doseq [y (m/steps 6 51 6)] (c/fill-rect ctx [115 y] [4 4]))
          (doseq [x (m/steps 8 51 6)] (c/fill-rect ctx [x 99] [4 4])))

        (c/begin-path)                                      ; ghost
        (c/move-to [83 116])
        (c/line-to [83 102])
        (c/bezier-curve-to [83 94] [89 88] [97 88])
        (c/bezier-curve-to [105 88] [111 94] [111 102])
        (c/line-to [111 116])
        (c/line-to [106.333 111.333])
        (c/line-to [101.666 116])
        (c/line-to [97 111.333])
        (c/line-to [92.333 116])
        (c/line-to [87.666 111.333])
        (c/line-to [87 116])
        (c/fill)

        (c/set-fill-style :white)                           ; eyes
        (c/begin-path)
        (c/move-to [91 96])
        (c/bezier-curve-to [88 96] [87 99] [87 101])
        (c/bezier-curve-to [87 103] [88 106] [91 106])
        (c/bezier-curve-to [94 106] [95 103] [95 101])
        (c/bezier-curve-to [95 99] [94 96] [91 96])
        (c/move-to [103 96])
        (c/bezier-curve-to [100 96] [99 99] [99 101])
        (c/bezier-curve-to [99 103] [100 106] [103 106])
        (c/bezier-curve-to [106 106] [107 103] [107 101])
        (c/bezier-curve-to [107 99] [106 96] [103 96])
        (c/fill)

        (c/set-fill-style :black)                           ; eye dots
        (c/begin-path)
        (c/circle [101 102] 2)
        #_(c/move-to [89 102])                              ; to avoid straight line between arcs, which isn't filled
        (c/circle [89 102] 2)
        (c/fill))))

(comment
  (defn rounded-rect [ctx [x y] [w h] r]                    ; prefer roundRect() from canvas api, helper from https://developer.mozilla.org/en-US/docs/Web/API/Canvas_API/Tutorial/Drawing_shapes#making_combinations
    (-> ctx
        (c/begin-path)
        (c/move-to [x (+ y r)])
        (c/arc-to [x (+ y h)] [(+ x r) (+ y h)] r)
        (c/arc-to [(+ x w) (+ y h)] [(+ x w) (- (+ y h) r)] r)
        (c/arc-to [(+ x w) y] [(- (+ x w) r) y] r)
        (c/arc-to [x y] [x (+ y r)] r)
        (c/stroke))))

(defn path-2d-example []                                    ; https://developer.mozilla.org/en-US/docs/Web/API/Canvas_API/Tutorial/Drawing_shapes#path2d_example
  (let [rectangle (c/rect (c/path) [10 10] [50 50])
        circle (c/circle (c/path) [100 35] 25)]
    (-> (c/append ::path-2d-example [150 150])
        (c/stroke rectangle)
        (c/fill circle))))

(defn using-svg-paths []                                    ; https://developer.mozilla.org/en-US/docs/Web/API/Canvas_API/Tutorial/Drawing_shapes#using_svg_paths
  (-> (c/append ::using-svg-paths [150 150])
      (c/fill (c/svg->path "M10 10 h 80 v 80 h -80 Z"))))

(comment
  (drawing-shapes)
  (drawing-a-triangle)
  (moving-the-pen)
  (lines)
  (arcs)
  (quadratic-bezier-curves)
  (cubic-bezier-curves)
  (making-combinations)
  (path-2d-example)
  (using-svg-paths))

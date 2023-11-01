(ns mdn.canvas-api-tutorial.drawing-shapes
  (:require [drawing.canvas :as c]
            [drawing.math :as m]))

(defn drawing-shapes []                                     ; https://developer.mozilla.org/en-US/docs/Web/API/Canvas_API/Tutorial/Drawing_shapes#rectangular_shape_example
  (-> (c/append ::drawing-shapes [150 150])
      (c/fill-rect [25 25] [100 100])
      (c/call "clearRect" 45 45 60 60)                      ; TODO implement clear-rect
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
      (c/arc [75 75] 50 0 (m/pi 2))
      (c/move-to [110 75])                                  ; remove c/move-to calls to see connecting lines
      (c/arc [75 75] 35 0 (m/pi))
      (c/move-to [65 65])
      (c/arc [60 65] 5 0 (m/pi 2))
      (c/move-to [95 65])
      (c/arc [90 65] 5 0 (m/pi 2))
      (c/stroke)))

(comment
  (drawing-shapes)
  (drawing-a-triangle)
  (moving-the-pen))

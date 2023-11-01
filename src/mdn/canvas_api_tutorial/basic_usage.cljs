(ns mdn.canvas-api-tutorial.basic-usage
  (:require [drawing.canvas :as c]))

(defn a-simple-example []                                   ; https://developer.mozilla.org/en-US/docs/Web/API/Canvas_API/Tutorial/Basic_usage#a_simple_example
  (-> (c/append ::a-simple-example [150 150])
      (c/set-fill-style "rgb(200, 0, 0)")
      (c/fill-rect [10 10] [50 50])
      (c/set-fill-style "rgba(0, 0, 200, 0.5)")
      (c/fill-rect [30 30] [50 50])))

(comment
  (a-simple-example))

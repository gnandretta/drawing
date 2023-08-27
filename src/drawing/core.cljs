(ns drawing.core
  (:require [drawing.canvas :as c :include-macros true]
            [goog.object :as object]))

(defn drawing
  {:width 608
   :height 1080}
  [{:keys [ctx width height]}]
  (let [circle-radius (* width 0.3)
        probability-inside (rand)
        probability-outside (rand)]
    (object/set ctx "fillStyle" "rgb(229,228,228)")
    (.fillRect ctx 0 0 width height)
    (.translate ctx (/ width 2) (/ height 2))
    (doseq [_ (range 45000)]
      (let [angle (rand (* 2 js/Math.PI))
            hypotenuse-inside (* (- 1 (rand (rand))) circle-radius)
            hypotenuse-outside (* (js/Math.cosh (rand js/Math.PI)) circle-radius)]
        (object/set ctx "fillStyle" (rand-nth (vec (concat (repeat 4 "rgb(109,79,246)")
                                                           (repeat 2 "rgb(64,0,131)")
                                                           (repeat 2 "rgb(57,0,3)")
                                                           (repeat 2 "rgb(255,195,190)")
                                                           (repeat 1 "rgb(255,255,255)")))))
        (when (> (rand) probability-inside)
          (.fillRect ctx
                     (* hypotenuse-inside (js/Math.cos angle))
                     (* hypotenuse-inside (js/Math.sin angle))
                     1
                     1))
        (when (> (rand) probability-outside)
          (.fillRect ctx
                     (* hypotenuse-outside (js/Math.cos angle))
                     (* hypotenuse-outside (js/Math.sin angle))
                     1
                     1))))))

(defn ^:dev/after-load init []
  (c/draw drawing))

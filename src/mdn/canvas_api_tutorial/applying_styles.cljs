(ns mdn.canvas-api-tutorial.applying-styles
  (:require [cljs.math :as cm]
            [drawing.canvas :as c]
            [drawing.cljs :refer [jump->]]
            [drawing.math :as m]))

(defn a-fill-style-example []                               ; https://developer.mozilla.org/en-US/docs/Web/API/Canvas_API/Tutorial/Applying_styles_and_colors#a_fillstyle_example
  (let [ctx (c/append ::a-fill-style-example [150 150])]
    (doseq [i (range 6) j (range 6) :let [[r g] (map #(js/Math.floor (- 255 (* 42.5 %))) [i j])]]
      (-> ctx
          (c/set-fill-style (str "rgb(" r "," g ",0)"))     ; TODO implement rgb fn
          (c/fill-rect [(* 25 j) (* 25 i)] [25 25])))))

(defn a-stroke-style-example []                               ; https://developer.mozilla.org/en-US/docs/Web/API/Canvas_API/Tutorial/Applying_styles_and_colors#a_strokestyle_example
  (let [ctx (c/append ::a-stroke-style-example [150 150])]
    (doseq [i (range 6) j (range 6) :let [[g b] (map #(js/Math.floor (- 255 (* 42.5 %))) [i j])]]
      (-> ctx
          (c/set-stroke-style (str "rgb(0," g "," b ")"))     ; TODO when implemented, use rgb fn
          (c/begin-path)
          (c/arc (map #(+ 12.5 (* 25 %)) [j i]) 10 0 (m/pi 2))
          (c/stroke)))))

(defn a-global-alpha-example []                               ; https://developer.mozilla.org/en-US/docs/Web/API/Canvas_API/Tutorial/Applying_styles_and_colors#a_globalalpha_example
  (let [ctx (c/append ::a-global-alpha-example [150 150])]
    (-> ctx
        (c/set-fill-style "#fd0")
        (c/fill-rect [75 75])
        (c/set-fill-style "#6c0")
        (c/fill-rect [75 0] [75 75])
        (c/set-fill-style "#09f")
        (c/fill-rect [0 75] [75 75])
        (c/set-fill-style "#f30")
        (c/fill-rect [75 75] [75 75])
        (c/set-fill-style "#fff"))
    (set! (.-globalAlpha ctx) 0.2)                          ; TODO implement set-global-alpha
    (doseq [i (range 7)]
          (-> ctx
              (c/begin-path)
              (c/arc [75 75] (+ 10 (* 10 i)) 0 (m/pi 2))
              (c/fill)))))

(defn an-example-using-rgba []                               ; https://developer.mozilla.org/en-US/docs/Web/API/Canvas_API/Tutorial/Applying_styles_and_colors#an_example_using_rgba
 (let [ctx (c/append ::an-example-using-rgba [150 150])]
    (-> ctx
        (c/set-fill-style "rgb(255, 221, 0)")
        (c/fill-rect [150 37.5])
        (c/set-fill-style "rgb(102, 204, 0)")
        (c/fill-rect [0 37.5] [150 37.5])
        (c/set-fill-style "rgb(0, 153, 255)")
        (c/fill-rect [0 75] [150 37.5])
        (c/set-fill-style "rgb(255, 51, 0)")
        (c/fill-rect [0 112.5] [150 37.5]))
    (doseq [i (range 10) j (range 4)]
      (c/set-fill-style ctx (str "rgba(255,255,255," (/ (inc i) 10)))
      (c/fill-rect ctx [(+ 5 (* 14 i)) (+ 5 (* 37.5 j))] [14 27.5]))))

(defn a-line-width-example []                               ; https://developer.mozilla.org/en-US/docs/Web/API/Canvas_API/Tutorial/Applying_styles_and_colors#a_linewidth_example
  (let [ctx (c/append ::a-line-width-example [150 150])]
    (doseq [i (range 10)]
      (-> ctx
          (c/set-line-width (inc i))                        ; odd-widths produce crisp lines, can void using .5px coords
          (c/begin-path)
          (c/move-to [(+ 5 (* 14 i)) 5])
          (c/line-to [(+ 5 (* 14 i)) 140])
          (c/stroke)))))

(defn a-line-cap-example []                                 ; https://developer.mozilla.org/en-US/docs/Web/API/Canvas_API/Tutorial/Applying_styles_and_colors#a_linecap_example
  (let [ctx (c/append ::a-line-cap-example [150 150])]
    (-> ctx
        (c/set-stroke-style "#09f")                         ; guides
        (c/begin-path)
        (c/move-to [10 10])
        (c/line-to [140 10])
        (c/move-to [10 140])
        (c/line-to [140 140])
        (c/stroke)

        (c/set-stroke-style :black)                         ; lines
        (c/set-line-width 15))
    (doseq [[i line-cap] (map-indexed list [:butt :round :square]) :let [x (+ 25 (* 50 i))]]
      (-> ctx
          (c/set-line-cap line-cap)
          (c/begin-path)
          (c/move-to [x 10])
          (c/line-to [x 140])
          (c/stroke)))))

(defn a-line-join-example []                                ; https://developer.mozilla.org/en-US/docs/Web/API/Canvas_API/Tutorial/Applying_styles_and_colors#a_linejoin_example
  (let [ctx (c/append ::a-line-join-example [150 150])]
    (c/set-line-width ctx 10)
    (doseq [[i line-join] (map-indexed list ["round" "bevel" "miter"])]
      (set! (.-lineJoin ctx) line-join)                     ; TODO implement set-line-join
      (-> ctx
          (c/begin-path)
          (c/move-to [-5 (+ 5 (* 40 i))])
          (c/line-to [35 (+ 45 (* 40 i))])
          (c/line-to [75 (+ 5 (* 40 i))])
          (c/line-to [115 (+ 45 (* 40 i))])
          (c/line-to [155 (+ 5 (* 40 i))])
          (c/stroke)))))

(defn a-demo-of-the-miter-limit-property []                 ; https://developer.mozilla.org/en-US/docs/Web/API/Canvas_API/Tutorial/Applying_styles_and_colors#a_demo_of_the_miterlimit_property
  (let [miter-limit 10
        ctx (c/append ::a-demo-of-the-miter-limit-property [150 150])]
    (set! (.-miterLimit ctx) miter-limit)                   ; TODO implement set-miter-limit
    (-> ctx
        (c/set-stroke-style "#09f")                         ; guides
        (c/set-line-width 2)
        (c/stroke-rect [-5 50] [160 50])
        (c/set-stroke-style :black)                         ; lines
        (c/set-line-width 10)
        (c/begin-path)
        (c/move-to [0 100])
        (jump-> (doseq [i (range 24)]
                  (c/line-to ctx [(* 2 (cm/pow i 1.5))
                                  (+ 75 (* 25 (if (even? i) 1 -1)))]))) ; TODO implement +-/set-sign fn?
        (c/stroke))))

(comment
  (a-fill-style-example)
  (a-stroke-style-example)
  (a-global-alpha-example)
  (an-example-using-rgba)
  (a-line-width-example)
  (a-line-cap-example)
  (a-line-join-example)
  (a-demo-of-the-miter-limit-property))

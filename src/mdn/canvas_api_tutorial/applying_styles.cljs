(ns mdn.canvas-api-tutorial.applying-styles
  (:require [cljs.core.async :refer [<! >! chan put! timeout] :refer-macros [go]]
            [cljs.math :as cm]
            [drawing.animation :as a]
            [drawing.canvas :as c]
            [drawing.cljs :refer [jump->]]
            [drawing.math :as m]
            [goog.dom :as dom]))

(defn a-fill-style-example []                               ; https://developer.mozilla.org/en-US/docs/Web/API/Canvas_API/Tutorial/Applying_styles_and_colors#a_fillstyle_example
  (let [ctx (c/append ::a-fill-style-example [150 150])]
    (doseq [i (range 6) j (range 6) :let [[r g] (map #(js/Math.floor (- 255 (* 42.5 %))) [i j])]]
      (-> ctx
          (c/set-fill-style (c/rgb [r g 0]))
          (c/fill-rect [(* 25 j) (* 25 i)] [25 25])))))

(defn a-stroke-style-example []                             ; https://developer.mozilla.org/en-US/docs/Web/API/Canvas_API/Tutorial/Applying_styles_and_colors#a_strokestyle_example
  (let [ctx (c/append ::a-stroke-style-example [150 150])]
    (doseq [i (range 6) j (range 6) :let [[g b] (map #(js/Math.floor (- 255 (* 42.5 %))) [i j])]]
      (-> ctx
          (c/set-stroke-style (c/rgb [0 g b]))
          (c/begin-path)
          (c/arc (map #(+ 12.5 (* 25 %)) [j i]) 10 (m/pii 2))
          (c/stroke)))))

(defn a-global-alpha-example []                             ; https://developer.mozilla.org/en-US/docs/Web/API/Canvas_API/Tutorial/Applying_styles_and_colors#a_globalalpha_example
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
        (c/set-fill-style "#fff")
        (c/set-global-alpha 0.2))
    (doseq [i (range 7)]
      (-> ctx
          (c/begin-path)
          (c/arc [75 75] (+ 10 (* 10 i)) (m/pii 2))
          (c/fill)))))

(defn an-example-using-rgba []                              ; https://developer.mozilla.org/en-US/docs/Web/API/Canvas_API/Tutorial/Applying_styles_and_colors#an_example_using_rgba
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
      (c/set-fill-style ctx (c/rgba [255 255 255 (/ (inc i) 10)]))
      (c/fill-rect ctx [(+ 5 (* 14 i)) (+ 5 (* 37.5 j))] [14 27.5]))))

(defn a-line-width-example []                               ; https://developer.mozilla.org/en-US/docs/Web/API/Canvas_API/Tutorial/Applying_styles_and_colors#a_linewidth_example
  (let [ctx (c/append ::a-line-width-example [150 150])]
    (doseq [i (range 10) :let [x (+ 5 (* 14 i))]]
      (-> ctx
          (c/set-line-width (inc i))                        ; odd-widths produce crisp lines, can void using .5px coords
          (c/begin-path)
          (c/move-to [x 5])
          (c/line-to [x 140])
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
    (doseq [[i line-join] (map-indexed list [:round :bevel :miter])]
      (-> ctx
          (c/set-line-join line-join)
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
    (-> ctx
        (c/set-miter-limit miter-limit)
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

(defn using-line-dashes []
  (let [d [150 150]
        ctx (c/append ::using-line-dashes d)
        in (chan)
        [play ctrl] (a/play)]
    (go (loop [offset 0]
          (>! in offset)
          (<! (timeout 20))
          (recur (if (= offset 5) 0 (inc offset)))))
    (c/set-line-dash ctx [4 2])
    (go (while true
          (let [offset (<! in)]
            (-> ctx
                (c/clear-rect d)
                (c/set-line-dash-offset (* -1 offset))
                (c/stroke-rect [10 10] [100 100])))
          (<! play)))
    ctrl))

(defn a-create-linear-gradient-example []                   ; https://developer.mozilla.org/en-US/docs/Web/API/Canvas_API/Tutorial/Applying_styles_and_colors#a_createlineargradient_example
  (let [ctx (c/append ::a-create-linear-gradient-example [150 150])
        gradient-bg (c/linear-gradient ctx [0 0] [0 150] [[0 "#00abeb"] ; TODO be consistent with color format?
                                                          [0.5 "#fff"]
                                                          [0.5 "#26c000"] ; same position as above for sharp transition
                                                          [1 "#fff"]])
        gradient-goal-post (c/linear-gradient ctx [0 50] [0 95] [[0.5 "rgb(0,0,0)"] ; implicit color stop at 0 of the same color
                                                                 [1 "rgba(0,0,0,0)"]])] ; same color as above to make it fade out
    (-> ctx
        (c/set-fill-style gradient-bg)
        (c/set-stroke-style gradient-goal-post)
        (c/fill-rect [10 10] [130 130])
        (c/stroke-rect [50 50] [50 50]))))

(defn a-create-radial-gradient-example []                   ; https://developer.mozilla.org/en-US/docs/Web/API/Canvas_API/Tutorial/Applying_styles_and_colors#a_createradialgradient_example
  (let [ctx (c/append ::a-create-radial-gradient-example [150 150])
        gradients [(c/radial-gradient ctx [0 150] 50 [0 140] 90
                                      [[0 "#f4f201"]
                                       [0.8 "rgb(228,199,0)"]
                                       [1 "rgba(228,199,0,0)"]])
                   (c/radial-gradient ctx [95 15] 15 [102 20] 40
                                      [[0 "#00c9ff"]
                                       [0.8 "#00b5e2"]      ; close but not the same color as below
                                       [1 "rgba(0, 201, 255, 0)"]])
                   (c/radial-gradient ctx [105 105] 20 [112 120] 50
                                      [[0 "#ff5f98"]
                                       [0.75 "rgb(255, 1, 136)"]
                                       [1 "rgba(255, 1, 136, 0)"]])
                   (c/radial-gradient ctx [45 45] 10 [52 50] 30
                                      [[0 "#a7d30c"]
                                       [0.9 "rgb(1, 159, 98)"]
                                       [1 "rgba(1, 159, 98, 0)"]])]]
    (doseq [g gradients]
      (-> ctx
          (c/set-fill-style g)
          (c/fill-rect [150 150])))))

(defn a-create-conic-gradient-example []                    ; https://developer.mozilla.org/en-US/docs/Web/API/Canvas_API/Tutorial/Applying_styles_and_colors#a_createconicgradient_example
  (let [ctx (c/append ::a-create-conic-gradient-example [250 150]) ;
        gradient-a (c/conic-gradient ctx 2 [62 75] [[0 "#a7d30c"] ; positioned at center of rectangle
                                                    [1 "#fff"]])
        gradient-b (c/conic-gradient ctx 0 [187 75] [[0 "black"] ; positioned at center of rectangle
                                                     [0.25 "black"]
                                                     [0.25 "white"]
                                                     [0.5 "white"]
                                                     [0.5 "black"]
                                                     [0.75 "black"]
                                                     [0.75 "white"]
                                                     [1 "white"]])]
    (-> ctx
        (c/set-fill-style gradient-a)
        (c/fill-rect [12 25] [100 100])
        (c/set-fill-style gradient-b)
        (c/fill-rect [137 25] [100 100]))))

(defn- load-image [url]                                     ; TODO move it to a drawing ns for reuse
  (let [c (chan)
        img (js/Image.)]
    (dom/setProperties img #js {:onload #(put! c img) :src url})
    c))

(defn a-create-pattern-example []                           ; https://developer.mozilla.org/en-US/docs/Web/API/Canvas_API/Tutorial/Applying_styles_and_colors#a_createpattern_example
  (go (let [url "https://developer.mozilla.org/en-US/docs/Web/API/Canvas_API/Tutorial/Applying_styles_and_colors/canvas_createpattern.png"
            img (<! (load-image url))
            ctx (c/append ::a-create-pattern-example [150 150])]
        (-> ctx
            (c/set-fill-style (c/pattern ctx img :repeat))
            (c/fill-rect [100 100])))))

(defn a-shadow-text-example []                              ; https://developer.mozilla.org/en-US/docs/Web/API/Canvas_API/Tutorial/Applying_styles_and_colors#a_shadowed_text_example
  (-> (c/append ::a-shadow-text-example [150 150])
      (c/set-shadow-offset [2 2])
      (c/set-shadow-color "rgba(0,0,0,0.5)")
      (c/set-font "20px Times New Roman")
      (c/fill-text "Sample String" [5 30])))

(defn canvas-fill-rules []                                  ; https://developer.mozilla.org/en-US/docs/Web/API/Canvas_API/Tutorial/Applying_styles_and_colors#canvas_fill_rules
  (-> (c/append ::canvas-fill-rules [150 150])
      (c/begin-path)
      (c/arc [50 50] 30 (m/pii 2))
      (c/arc [50 50] 15 (m/pii 2))
      (c/fill :evenodd)))

(comment
  (a-fill-style-example)
  (a-stroke-style-example)
  (a-global-alpha-example)
  (an-example-using-rgba)
  (a-line-width-example)
  (a-line-cap-example)
  (a-line-join-example)
  (a-demo-of-the-miter-limit-property)
  (def a (using-line-dashes)) (go (>! a :toggle))
  (a-create-linear-gradient-example)
  (a-create-radial-gradient-example)
  (a-create-conic-gradient-example)
  (a-create-pattern-example)
  (a-shadow-text-example)
  (canvas-fill-rules))

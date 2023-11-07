(ns mdn.canvas-api-tutorial.transformations
  (:require [drawing.canvas :as c]
            [drawing.math :as m]))

(defn a-save-and-restore-canvas-state-example []            ; https://developer.mozilla.org/en-US/docs/Web/API/Canvas_API/Tutorial/Transformations#a_save_and_restore_canvas_state_example
  (-> (c/append ::a-save-and-restore-canvas-state-example [150 150])
      (c/fill-rect [150 150])
      (c/save)
      (c/set-fill-style "#09f")
      (c/fill-rect [15 15] [120 120])
      (c/save)
      (c/set-fill-style :white)
      (c/set-global-alpha 0.5)
      (c/fill-rect [30 30] [90 90])
      (c/restore)
      (c/fill-rect [45 45] [60 60])
      (c/restore)
      (c/fill-rect [60 60] [30 30])))

(defn a-translate-example []                                ; https://developer.mozilla.org/en-US/docs/Web/API/Canvas_API/Tutorial/Transformations#a_translate_example
  (let [ctx (c/append ::a-translate-example [150 150])]
    (doseq [i (range 3) j (range 3) :let [x (+ 10 (* 50 j))
                                          y (+ 10 (* 50 i))
                                          r (* 51 i)
                                          g (- 255 (* 51 i))]]
      (-> ctx
          (c/save)
          (c/set-fill-style (c/rgb [r g 255]))
          (c/translate [x y])
          (c/fill-rect [25 25])
          (c/restore)))))

(defn a-rotate-example []                                   ; https://developer.mozilla.org/en-US/docs/Web/API/Canvas_API/Tutorial/Transformations#a_rotate_example
  (let [ctx (c/append ::a-rotate-example [300 200])
        blue "#0095dd"
        grey "#4d4e53"]
    (-> ctx
        (c/save)
        (c/set-fill-style blue)                             ; rotated from top-left corner of canvas
        (c/fill-rect [30 30] [100 100])
        (c/rotate (m/pi (/ 25 180)))
        (c/set-fill-style grey)
        (c/fill-rect [30 30] [100 100])
        (c/restore)
        #_(c/translate [120 0])                             ; uncomment and set x=x-120 below to make things easier
        (c/set-fill-style blue)                             ; rotated from center of grey rectangle
        (c/fill-rect [150 30] [100 100])
        (c/translate [200 80])
        (c/rotate (m/pi (/ 25 180)))
        (c/translate [-200 -80])
        (c/set-fill-style grey)
        (c/fill-rect [150 30] [100 100]))))

(defn a-scale-example []                                    ; https://developer.mozilla.org/en-US/docs/Web/API/Canvas_API/Tutorial/Transformations#a_scale_example
  (-> (c/append ::a-scale-example [150 150])
      (c/save)
      (c/scale [10 3])                                      ; scale that doesn't preserve aspect ratio
      (c/fill-rect [1 10] [10 10])
      (c/restore)
      (c/scale [-1 1])                                      ; mirror horizontally
      (c/set-font "48px serif")
      (c/fill-text "MDN" [-135 120]) ) )

(defn example-for-transform-and-set-transform []             ; https://developer.mozilla.org/en-US/docs/Web/API/Canvas_API/Tutorial/Transformations#example_for_transform_and_settransform
  (let [ctx (c/append ::example-for-transform-and-set-transform [200 250])
        sin (js/Math.sin (m/pif 6))
        cos (js/Math.cos (m/pif 6))]
    (c/translate ctx [100 100])
    (doseq [i (range 13) :let [c (js/Math.floor (* (/ 255 12) i))]]
      (-> ctx
          (c/set-fill-style (c/rgb [c c c]))
          (c/fill-rect [100 10])
          (c/transform cos sin (* -1 sin) cos 0 0)))
    (-> ctx
        (c/set-transform -1 0 0 1 100 100)
        (c/set-fill-style "rgba(255,128,255,0.5)")
        (c/fill-rect [0 50] [100 100]))))

(comment
  (a-save-and-restore-canvas-state-example)
  (a-translate-example)
  (a-rotate-example)
  (a-scale-example)
  (example-for-transform-and-set-transform))

(ns drawing.canvas)

(defmacro draw [drawing]
  (if (symbol? drawing)
    `(draw* (meta (var ~drawing)) ~drawing)
    `(draw* ~drawing)))

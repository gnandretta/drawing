(ns drawing.canvas)

(defmacro draw [drawing & args]
  (if (symbol? drawing)
    `(draw* (meta (var ~drawing)) ~drawing ~@args)
    `(draw* {} ~drawing ~@args)))

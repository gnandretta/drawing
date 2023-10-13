(ns drawing.math
  (:require [clojure.walk :as w]))

(defmacro v [form]
  (let [m '{+ drawing.math/v+
            - drawing.math/v-
            * drawing.math/v*
            / drawing.math/v-div}]
    (w/postwalk-replace m form)))

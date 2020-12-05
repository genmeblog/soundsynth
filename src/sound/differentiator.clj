(ns sound.differentiator
  (:require [fastmath.core :as m]
            [sound.dsp :as dsp]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defrecord Differentiator [^double previous ^double lp])

(defn init
  (^Differentiator [] (Differentiator. 0.0 0.0))
  (^Differentiator [^double prev] (Differentiator. prev 0.0)))

(defn process
  ^Differentiator [^Differentiator d ^double coeff ^double s]
  (Differentiator. s (dsp/ONE-POLE (.lp d) (- s (.previous d)) coeff)))

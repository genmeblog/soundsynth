(ns sound.waveshapers
  (:require [fastmath.core :as m]
            [fastmath.stats :as stats]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def ^:const ^int WAVESHAPER_SIZE 1024)

(def smooth-bipolar-fold
  (let [x (map (fn [^long i]
                 (let [i (double (if (= i WAVESHAPER_SIZE) (dec WAVESHAPER_SIZE) i))]
                   (dec (/ i (/ WAVESHAPER_SIZE 2))))) (range (inc WAVESHAPER_SIZE)))
        
        sine (map (fn [^double x]
                    (m/sin (* m/TWO_PI x))) x)
        window (map (fn [^double x]
                      (m/sq (m/exp (* (- x) x 1.5)))) x)]
    (mapv (fn [^double x ^double s ^double w]
            (+ (* s w)
               (* (m/atan (* 2.0 x))
                  (- 1.0 w)))) x sine window)))

(def bipolar-fold
  (let [x (map (fn [^long i]
                 (let [i (double (if (= i WAVESHAPER_SIZE) (dec WAVESHAPER_SIZE) i))]
                   (dec (/ i (/ WAVESHAPER_SIZE 2))))) (range (inc WAVESHAPER_SIZE)))
        
        sine (map (fn [^double x]
                    (m/sin (* 8.0 m/TWO_PI x))) x)
        window (map (fn [^double x]
                      (m/sq (m/exp (* (- x) x 4.0)))) x)]
    (mapv (fn [^double x ^double s ^double w]
            (+ (* s w)
               (* (m/atan (* 3.0 x))
                  (- 1.0 w)))) x sine window)))

(def unipolar-fold
  (let [x (map (fn [^long i]
                 (let [i (double (if (= i WAVESHAPER_SIZE) (dec WAVESHAPER_SIZE) i))]
                   (/ i WAVESHAPER_SIZE))) (range (inc WAVESHAPER_SIZE)))
        
        sine (map (fn [^double x]
                    (m/sin (* 8.0 m/TWO_PI x))) x)
        window (map (fn [^double x]
                      (m/sq (m/exp (* (- x) x 4.0)))) x)]
    (mapv (fn [^double x ^double s ^double w]
            (+ (* (+ (* 0.5 s) x x) w)
               (* (m/atan (* 4.0 x))
                  (- 1.0 w)))) x sine window)))

(def x (map (fn [^double i]
              (/ i WAVESHAPER_SIZE)) (range (inc WAVESHAPER_SIZE))))

(defn audio-rate-flip
  [x]
  (vec (concat (map (fn [^double v] (- v)) (reverse (rest x))) x)))

(def linear-audio (audio-rate-flip x))

(def sin-audio
  (audio-rate-flip (map (fn [^double v]
                          (* 0.5 (- 1.0 (m/cos (* m/PI v))))) x)))

(def tan (map (fn [^double v]
                (m/atan (* 8.0 (m/cos (* m/PI v))))) x))

(def mxtan (stats/maximum tan))

(def tan-audio
  (audio-rate-flip (map (fn [^double v]
                          (* 0.5 (- 1.0 (/ v mxtan)))) tan)))

(def inverse-sin-audio
  (audio-rate-flip (map (fn [^double v]
                          (/ (m/acos (- 1.0 (* 2.0 v))) m/PI)) x)))

(def inverse-tan-audio
  (audio-rate-flip (map (fn [^double v]
                          (/ (m/acos (/ (m/tan (* mxtan (- 1.0 (* 2.0 v)))) 8.0)) m/PI)) x)))

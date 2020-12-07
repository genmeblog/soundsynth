(ns sound.waveshaper
  (:require [fastmath.core :as m]
            [fastmath.stats :as stats]
            [sound.waveforms :as wv]
            [sound.differentiator :as diff]
            [sound.dsp :as dsp])
  (:import [sound.differentiator Differentiator]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def ^:const ^long WAVESHAPER-SIZE 1024)
(def ^:const ^double WAVESHAPER-SCALE (/ 32767.0))

(defn generate-shaper
  [^long wv1 ^long wv2 ^double mix]
  (let [steps (map (fn [^double i]
                     (/ i WAVESHAPER-SIZE)) (range (+ 12 2 (* 3 WAVESHAPER-SIZE))))
        differentiator (let [a (wv/read-wave wv1 0 0.0)
                             b (wv/read-wave wv2 0 0.0)]
                         (diff/init (m/mlerp a b mix)))]
    (->> steps
         (reduce (fn [[d buff] phase]
                   (let [p (* wv/table-size-f (m/frac phase))
                         p-integral (long p)
                         p-fractional (- p p-integral)
                         a (wv/read-wave wv1 p-integral p-fractional)
                         b (wv/read-wave wv2 p-integral p-fractional)
                         ^Differentiator ndiff (diff/process d 0.5 (m/mlerp a b mix))]
                     [ndiff (conj buff (.lp ndiff))])) [differentiator '()])
         (second)
         (reverse)
         (take-last (+ 2 WAVESHAPER-SIZE))
         (wv/normalize)
         (map (fn [^double v]
                (short (* v 32767.0))))
         (short-array))))

(defrecord WaveshaperParams [lut ^double amount bipolar?])

(defn init-params
  ^WaveshaperParams [waveform1 waveform2 mix amount bipolar?]
  (let [shaper (generate-shaper (* ^long waveform1 wv/table-size) (* ^long waveform2 wv/table-size) ^double mix)]
    (WaveshaperParams. shaper amount bipolar?)))

(defn process
  [^WaveshaperParams params ^double in]
  (let [in (m/constrain in -1.0 1.0)
        off (if (.bipolar? params)
              (m/norm in -1.0 1.0 0.0 WAVESHAPER-SIZE)
              (m/norm (m/abs in) 0.0 1.0 0.0 WAVESHAPER-SIZE))
        off-integral (long off)
        off-fractional (- off off-integral)
        a1 (aget ^shorts (.lut params) off-integral)
        a2 (aget ^shorts (.lut params) (inc off-integral))
        v (* WAVESHAPER-SCALE (m/mlerp a1 a2 off-fractional))]
    (if (.bipolar? params)
      (m/mlerp in v (.amount params))
      (* (m/sgn in) (m/mlerp (m/abs in) v (.amount params))))))




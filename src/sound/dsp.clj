(ns sound.dsp
  (:require [fastmath.core :as m]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def ^:const ^double RATE 44100.0)
(def ^:const ^double REV_RATE (/ RATE))

(defmacro ONE-POLE
  [out in coefficient]
  `(+ ~out (* ~coefficient (- ~in ~out))))

(defn interpolate-wave
  ^double [^doubles table ^long integral ^double fractional]
  (let [a (aget table integral)
        b (aget table (inc integral))]
    (m/mlerp a b fractional)))

(defn interpolate-wave-hermite
  ^double [^shorts table ^long integral ^double fractional]
  (let [xm1 (aget table integral)
        x0 (aget table (inc integral))
        x1 (aget table (+ 2 integral))
        x2 (aget table (+ 3 integral))
        c (* 0.5 (- x1 xm1))
        v (- x0 x1)
        w (+ c v)
        a (+ w v (* 0.5 (- x2 x0)))
        b-neg (+ w a)]
    (-> a
        (* fractional)
        (- b-neg)
        (* fractional)
        (+ c)
        (* fractional)
        (+ x0))))

;; (/ (/ 440.0 8) 44100.0)
(def ^:const ^double a0 0.0012471655328798186)

(def pitch-ratio-high
  (double-array (map (fn [^double x]
                       (m/pow 2.0 (/ (- x 128.0) 12.0))) (range 256))))

(def pitch-ratio-low
  (double-array (map (fn [^double x]
                       (m/pow 2.0 (/ (/ x 256.0) 12.0))) (range 256))))


(defn semitones->ratio
  ^double [^double semitones]
  (let [pitch (+ semitones 128.0)
        integral (int pitch)
        fract (- pitch integral)]
    (* (aget ^doubles pitch-ratio-high integral)
       (aget ^doubles pitch-ratio-low (int (* fract 256.0))))))

(defn note->frequency
  ^double [^double midi-note]
  (* a0 0.25 (semitones->ratio (m/constrain (- midi-note 9.0) -128.0 127.0))))



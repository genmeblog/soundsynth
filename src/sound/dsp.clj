(ns sound.dsp
  (:require [fastmath.core :as m]
            [fastmath.vector :as v])
  (:import [fastmath.vector Vec3]))

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

(defn interpolate
  ^double [^doubles table ^double index ^long size]
  (let [idx (* index size)
        integral (int idx)
        fractional (- idx integral)]
    (interpolate-wave table integral fractional)))

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

(def ^:const ^double a0 (/ (/ 440.0 8) RATE))

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

;;

(defn- cosine-oscillator-init
  [^double frequency]
  (let [iir-coefficient (* 2.0 (m/cos (* m/TWO_PI frequency)))]
    (Vec3. 0.5 (* 0.25 iir-coefficient) iir-coefficient)))

(defn- cosine-oscillator-init-approximate
  [^double frequency]
  (let [frequency (- frequency 0.25)
        sign (if (<= 0.0 frequency 0.5) -16.0 16.0)
        ^double frequency (cond
                            (neg? frequency) (- frequency)
                            (> frequency 0.5) (- frequency 0.5)
                            :else frequency)
        iir-coefficient (* sign frequency (- 1.0 (* 2.0 frequency)))]
    (Vec3. 0.5 (* 0.25 iir-coefficient) iir-coefficient)))

;; x - y0
;; y - y1
;; z - iir-coefficient
;; (+ y 0.5) - value

(defn cosine-oscillator
  [typ ^double frequency]
  (let [^Vec3 osc-data (if (= typ :approximate)
                         (cosine-oscillator-init-approximate frequency)
                         (cosine-oscillator-init frequency))
        next (fn [^Vec3 in]
               (Vec3. (- (* (.z in) (.x in)) (.y in))
                      (.x in)
                      (.z in)))]
    (iterate next (next osc-data))))


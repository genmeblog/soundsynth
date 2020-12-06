(ns sound.oscillator
  (:require [fastmath.core :as m]
            [sound.waveforms :as wv]
            [sound.dsp :as dsp]
            [sound.differentiator :as diff])
  (:import [sound.differentiator Differentiator]))

(def ^:const ^int table-size 260)
(def ^:const ^double table-size-f 256.0)

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defn read-wave
  ^double [^long off ^long phase-integral ^double phase-fractional]
  (dsp/interpolate-wave-hermite wv/wavetable (+ off phase-integral) phase-fractional))

(defrecord OscillatorParams [^long off1 ^long off2 ^double mix ^double pm])

(defn init-params
  (^OscillatorParams [^long waveform1 ^long waveform2 ^double mix]
   (init-params waveform1 waveform2 mix 0.0))
  (^OscillatorParams [^long waveform1 ^long waveform2 ^double mix ^double pm]
   (OscillatorParams. (* waveform1 table-size) (* waveform2 table-size) mix pm)))

(defrecord Oscillator [^double phase ^Differentiator diff-out ^double out])

(defn init
  (^Oscillator [] (Oscillator. 0.0 (diff/init 0.0) 0.0))
  (^Oscillator [^OscillatorParams p]
   (let [a (read-wave (.off1 p) 0 0.0)
         b (read-wave (.off2 p) 0 0.0)]
     (Oscillator. 0.0 (diff/init (m/mlerp a b (.mix p))) 0.0))))

(defn render
  (^Oscillator [^Oscillator oscillator ^OscillatorParams params ^double note]
   (render oscillator params note 0.0))
  (^Oscillator [^Oscillator oscillator ^OscillatorParams params ^double note ^double pm-in]
   (let [f0 (dsp/note->frequency note)
         gain (* (/ (* f0 131072.0)) (- 0.95 f0))
         cutoff (min 1.0 (* f0 table-size-f))
         phase (+ (.phase oscillator) f0)
         phase (if (>= phase 1.0) (dec phase) phase)
         phase (if (zero? (.pm params)) phase (+ phase (* pm-in (.pm params))))
         ^double phase (cond
                         (>= phase 1.0) (dec phase)
                         (neg? phase) (inc phase)
                         :else phase)
         p (* phase table-size-f)
         p-integral (long p)
         p-fractional (- p p-integral)
         a (read-wave (.off1 params) p-integral p-fractional)
         b (read-wave (.off2 params) p-integral p-fractional)
         ndiff (diff/process (.diff-out oscillator) cutoff (m/mlerp a b (.mix params)))]
     (Oscillator. phase
                  ndiff
                  (* gain (.lp ^Differentiator ndiff))))))

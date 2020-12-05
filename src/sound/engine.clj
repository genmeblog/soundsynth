(ns sound.engine
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

(defrecord WavetableEngineParams [^long off ^double frequency])

(defn init-params
  ^WavetableEngineParams [^long waveform ^double note]
  (WavetableEngineParams. (* waveform table-size) (dsp/note->frequency note)))

(defrecord WavetableEngine [^double phase ^Differentiator diff-out ^double out])

(defn init
  (^WavetableEngine [] (WavetableEngine. 0.0 (diff/init 0.0) 0.0))
  (^WavetableEngine [^WavetableEngineParams p]
   (WavetableEngine. 0.0 (diff/init (read-wave (.off p) 0 0.0)) 0.0)))

(defn render
  [^WavetableEngine engine ^WavetableEngineParams params]
  (let [f0 (.frequency params)
        gain (* (/ (* f0 131072.0)) (- 0.95 f0))
        cutoff (min 1.0 (* f0 table-size-f))
        phase (+ (.phase engine) f0)
        phase (if (>= phase 1.0) (dec phase) phase)
        p (* phase table-size-f)
        p-integral (long p)
        p-fractional (- p p-integral)
        mix (read-wave (.off params) p-integral p-fractional)
        ndiff (diff/process (.diff-out engine) cutoff mix)]
    (WavetableEngine. phase
                      ndiff
                      (* gain (.lp ^Differentiator ndiff)))))

(let [p (init-params 1 50)]
  (take 5 (iterate #(render % p) (init p))))

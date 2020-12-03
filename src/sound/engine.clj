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

(deftype WaveTableEngine [^double phase ^Differentiator diff-out ^double out])

(deftype WaveParams [^long off ^double frequency])

(defn ->WaveParams
  ^WaveParams [^long waveform ^double note]
  (WaveParams. (* waveform table-size) (dsp/note->frequency note)))

(defn init
  (^WaveTableEngine [] (WaveTableEngine. 0.0 (diff/init 0.0) 0.0))
  (^WaveTableEngine [^WaveParams p]
   (WaveTableEngine. 0.0 (diff/init (read-wave (.off p) 0 0.0)) 0.0)))

(defn render
  [^WaveTableEngine engine ^WaveParams params]
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
    (WaveTableEngine. phase
                      ndiff
                      (* gain (.lp ^Differentiator ndiff)))))

;; (map #(.out ^WaveTableEngine %) (take 250 (iterate render (init))))



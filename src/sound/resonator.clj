(ns sound.resonator
  (:require [fastmath.core :as m]
            [sound.filter :as filt]
            [sound.dsp :as dsp]
            [fastmath.vector :as v])
  (:import [fastmath.vector Vec3]
           [sound.filter SVF]))


(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def lut-stiffness (double-array (map (fn [^long i]
                                        (let [g (/ i 256.0)]
                                          (if (< i 255)
                                            (cond
                                              (< g 0.25) (* 0.25 (- g 0.25))
                                              (< g 0.3) 0.0
                                              (< g 0.9) (let [g (/ (- g 0.3) 0.6)]
                                                          (- (* 0.01 (m/pow 10.0 (* g 2.005))) 0.01))
                                              :else (let [g (m/sq (/ (- g 0.9) 0.1))]
                                                      (- 1.5 (* 0.5 (m/cos (* g m/PI))))))
                                            2.0))) (range 257))))

(def lut-4-decades (double-array (map (fn [^long i]
                                        (let [x (/ i 64.0)]
                                          (m/pow 10.0 x))) (range 257))))

(deftype Resonator [^double frequency ^double structure ^double brightness ^double dumping
                    ^double position ^long modes filters ^double result])

(defn- ensure-even-resolution-
  ^long [^long resolution]
  (m/constrain (if (odd? resolution) (dec resolution) resolution) 2 32))

(defn- ensure-even-resolution+
  ^long [^long resolution]
  (m/constrain (if (odd? resolution) (inc resolution) resolution) 2 32))

(defn- svfs
  [^long resolution]
  (let [r (ensure-even-resolution- resolution)]
    (repeatedly r filt/init)))

(defn init
  ([] (init 8))
  ([^long resolution]
   (let [flts (svfs resolution)]
     (Resonator. (/ 220.0 dsp/RATE)
                 0.25
                 0.5
                 0.3
                 0.999
                 (count flts)
                 flts
                 0.0))))

(defn modify
  [^Resonator r what value]
  (case what
    :frequency (Resonator. value (.structure r) (.brightness r) (.dumping r)
                           (.position r) (.modes r) (.filters r) (.result r))
    :structure (Resonator. (.frequency r) value (.brightness r) (.dumping r)
                           (.position r) (.modes r) (.filters r) (.result r))
    :brightness (Resonator. (.frequency r) (.structure r) value (.dumping r)
                            (.position r) (.modes r) (.filters r) (.result r))
    :dumping (Resonator. (.frequency r) (.structure r) (.brightness r) value
                         (.position r) (.modes r) (.filters r) (.result r))
    :position (Resonator. (.frequency r) (.structure r) (.brightness r) (.dumping r)
                          value (.modes r) (.filters r) (.result r))
    :resolution (let [flts (svfs (long value))]
                  (Resonator. (.frequency r) (.structure r) (.brightness r) (.dumping r)
                              (.position r) (count flts) flts (.result r)))
    r))


(deftype Pair [a b])

(comment (defn compute-filters
           [^Resonator r]
           (let [stiffness (dsp/interpolate lut-stiffness (.structure r) 256)
                 q (* 500.0 (dsp/interpolate lut-4-decades (.dumping r) 256))
                 brightness-attenuation (m/pow (- 1.0 (.structure r)) 8.0)
                 brightness (* (.brightness r)
                               (- 1.0 (* 0.2 brightness-attenuation)))
                 q-loss (+ 0.15 (* brightness (- 2.0 brightness) 0.85))
                 q-loss-damping-rate (* (.structure r) (- 2.0 (.structure r)) 0.1)
                 resolution (count (.filters r))
                 ^Pair n (loop [i (int 0)
                                num-modes (int 0)
                                harmonic (.frequency r)
                                stretch-factor 1.0
                                target []
                                filters (.filters r)
                                stiffness stiffness
                                q-loss q-loss
                                q q]
                           (if (not (seq filters))
                             (Pair. (if (zero? num-modes)
                                      resolution
                                      (ensure-even-resolution+ num-modes))
                                    target)
                             (let [partial-frequency (min 0.49 (* harmonic stretch-factor))
                                   nq-loss (+ q-loss (* q-loss-damping-rate (- 1.0 q-loss)))]
                               (recur (inc i)
                                      (if (and (>= partial-frequency 0.49)
                                               (zero? num-modes))
                                        (inc i)
                                        num-modes)
                                      (+ harmonic (.frequency r))
                                      (+ stretch-factor stiffness)
                                      (conj target (filt/set-fq (first filters) :fast partial-frequency (inc (* partial-frequency q))))
                                      (rest filters)
                                      (if (neg? stiffness)
                                        (* stiffness 0.93)
                                        (* stiffness 0.98))
                                      nq-loss
                                      (* q nq-loss)))))]
             (Resonator. (.frequency r) (.structure r) (.brightness r) (.dumping r)
                         (.position r) (.a n) (.b n) (.result r)))))


(comment (defn process
           [^Resonator r ^double in]
           (let [amplitudes (dsp/cosine-oscillator :approximate (.position r))
                 input (* 0.125 in)
                 filters (map-indexed (fn [^long idx f]
                                        (if (< idx (.modes r))
                                          (filt/process f in)
                                          f)) (.filters r))
                 result (reduce m/fast+ (map (fn [^Vec3 amp ^SVF f]
                                               (let [^SVF res (.result f)]
                                                 (* (+ 0.5 (.y amp))
                                                    (.bp res)))) amplitudes filters))]
             (Resonator. (.frequency r) (.structure r) (.brightness r) (.dumping r)
                         (.position r) (.modes r) filters result))))


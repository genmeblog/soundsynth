(ns sound.clock
  (:require [sound.dsp :as dsp]
            [fastmath.core :as m]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defn bpm->step
  "BPMs to number of samples"
  ^long [^long bpm]
  (m/round-even (/ (* 60.0 dsp/RATE) bpm)))

;; step between triggers and time of the open gate (both in number of samples)
(defrecord ClockParams [^int step ^int gate-time])

(defn init-params
  "Initialize clock configuration, bpms and gate time"
  (^ClockParams [] (init-params 120))
  (^ClockParams [^long bpm] (init-params bpm 0.9))
  (^ClockParams [^long bpm ^double gate-time-ratio]
   (let [step (bpm->step bpm)]
     (ClockParams. step (m/constrain (m/round-even (* gate-time-ratio step)) 2 (dec step))))))

;; clock context: where we are now, trigger status and gate status
(defrecord Clock [^long current-sample trigger gate-down])

(defn init
  ^Clock []
  (Clock. 0 true false))

(defn process
  "Calculate next clock state:
  - reset trigger just after beginning of the cycle
  - close the gate after achieving desired moment (controlled by `gate-time-ratio`)
  - reset clock and set trigger when `step` number of samples has passed
  - increment counter"
  ^Clock [^Clock cl ^ClockParams params]
  (let [ns (inc (.current-sample cl))]
    (condp = ns
      1 (Clock. ns false (.gate-down cl))
      (.step params) (Clock. 0 true false)
      (.gate-time params) (Clock. ns (.trigger cl) true)
      (Clock. ns (.trigger cl) (.gate-down cl)))))


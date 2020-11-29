(ns sound.adsr
  (:require [fastmath.core :as m]
            [sound.dsp :as dsp]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def ^:const ^double min-time 1.0e-3)
(def ^:const ^double max-time 10.0)
(def ^:const ^double lambda-base (/ max-time min-time))

(deftype ADSRParams [^double attack-lambda ^double decay-lambda ^double sustain ^double release-lambda])

(defn ->ADSRParams
  [^double attack ^double decay ^double sustain ^double release]
  (ADSRParams. (/ (m/pow lambda-base (- (m/constrain attack 0.0 1.0))) min-time)
               (/ (m/pow lambda-base (- (m/constrain decay 0.0 1.0))) min-time)
               (m/constrain sustain 0.0 1.0)
               (/ (m/pow lambda-base (- (m/constrain release 0.0 1.0))) min-time)))

(deftype ADSR [^ADSRParams params attacking? ^double env])

(defn ->ADSR [params ] (ADSR. params true 0.0))

(defn process
  [^ADSR adsr note?]
  (let [^ADSRParams params (.params adsr)
        target (if note?
                 (if (.attacking? adsr) 1.2 (.sustain params))
                 -0.01)
        lambda (if note?
                 (if (.attacking? adsr)
                   (.attack-lambda params)
                   (.decay-lambda params))
                 (.release-lambda params))
        env (max 0.0 (+ (.env adsr) (* (- target (.env adsr))
                                       lambda dsp/REV_RATE)))
        attacking? (if (> env 1.0) false (.attacking? adsr))
        attacking? (if note? attacking? false)]
    (ADSR. params attacking? env)))

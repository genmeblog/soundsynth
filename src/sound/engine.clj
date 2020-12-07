(ns sound.engine
  (:require [sound.clock :as clock]
            [sound.oscillator :as o]
            [sound.adsr :as adsr]
            [sound.sequencer :as sr]
            [sound.patch :as patch]
            [sound.filter :as f]
            [sound.dsp :as dsp]
            [fastmath.core :as m]
            [sound.waveshaper :as ws])
  (:import [sound.sequencer Sequencer]
           [sound.oscillator Oscillator]
           [sound.adsr ADSR]
           [sound.filter SVF]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

;; s1
(def clock-1 (clock/init))
(def oscillator-1 (o/init patch/oscillator-1))
(def phase-oscillator-1 (o/init patch/phase-oscillator-1))
(def adsr-oscillator-1 (adsr/init))
(def adsr-filter-1 (adsr/init))
(def filter-1 (f/init))
(def sequencer-1 (sr/init clock-1))

(def clock-2 (clock/init))
(def oscillator-2 (o/init patch/oscillator-2))
(def adsr-oscillator-2 (adsr/init))
(def adsr-filter-2 (adsr/init))
(def filter-2 (f/init))
(def sequencer-2 (sr/init clock-2))

(def clock-3 (clock/init))
(def oscillator-3 (o/init patch/oscillator-3))
(def adsr-oscillator-3 (adsr/init))
(def adsr-filter-3 (adsr/init))
(def filter-3 (f/init))
(def sequencer-3 (sr/init clock-3))


(defrecord Engine
    [^double prev-out ^double out
     oscillator-1 phase-oscillator-1 adsr-oscillator-1 adsr-filter-1 filter-1 sequencer-1
     oscillator-2 adsr-oscillator-2 adsr-filter-2 filter-2 sequencer-2
     oscillator-3 adsr-oscillator-3 adsr-filter-3 filter-3 sequencer-3])

(defn engine
  ([]
   (Engine. 0.0 0.0
            oscillator-1 phase-oscillator-1 adsr-oscillator-1 adsr-filter-1 filter-1 sequencer-1
            oscillator-2 adsr-oscillator-2 adsr-filter-2 filter-2 sequencer-2
            oscillator-3 adsr-oscillator-3 adsr-filter-3 filter-3 sequencer-3))
  ([^Engine e]
   (let [^Sequencer nsequencer-1 (sr/process (.sequencer-1 e) patch/sequence-1 patch/clock-1)
         note (.note nsequencer-1)
         ^Oscillator phase-oscillator-1 (o/render (.phase-oscillator-1 e) patch/phase-oscillator-1 (+ note ^double patch/phase-note-shift))
         ^Oscillator noscillator-1 (o/render (.oscillator-1 e) patch/oscillator-1 note (.out phase-oscillator-1))

         osc-sample-shaped (ws/process patch/waveshaper-1 (.out noscillator-1))
         
         ^ADSR nadsr-filter-1 (adsr/process (.adsr-filter-1 e) patch/adsr-filter-1 (.clock nsequencer-1))
         ^SVF nfilter-1 (f/process (.filter-1 e) (f/set-fq (* (inc (* (.env nadsr-filter-1) ^double patch/filter-amount-1))
                                                              ^double patch/filter-freq-1) patch/filter-q-1) osc-sample-shaped)
         
         ^ADSR nadsr-oscillator-1 (adsr/process (.adsr-oscillator-1 e) patch/adsr-oscillator-1 (.clock nsequencer-1))
         out-1 (* (.env nadsr-oscillator-1) (.hp nfilter-1))

         ;; 2
         ^Sequencer nsequencer-2 (sr/process (.sequencer-2 e) patch/sequence-2 patch/clock-2)
         note (.note nsequencer-2)
         ^Oscillator noscillator-2 (o/render (.oscillator-2 e) patch/oscillator-2 note)

         ^ADSR nadsr-filter-2 (adsr/process (.adsr-filter-2 e) patch/adsr-filter-2 (.clock nsequencer-2))
         ^SVF nfilter-2 (f/process (.filter-2 e) (f/set-fq (* (inc (* (.env nadsr-filter-2) ^double patch/filter-amount-2))
                                                              ^double patch/filter-freq-2) patch/filter-q-2) (.out noscillator-2))
         
         ^ADSR nadsr-oscillator-2 (adsr/process (.adsr-oscillator-2 e) patch/adsr-oscillator-2 (.clock nsequencer-2))         
         out-2 (* (.env nadsr-oscillator-2) (.lp nfilter-2))

         ;; 3
         ^Sequencer nsequencer-3 (sr/process (.sequencer-3 e) patch/sequence-3 patch/clock-3)
         note (.note nsequencer-3)
         ^Oscillator noscillator-3 (o/render (.oscillator-3 e) patch/oscillator-3 note)

         ^ADSR nadsr-filter-3 (adsr/process (.adsr-filter-3 e) patch/adsr-filter-3 (.clock nsequencer-3))
         ^SVF nfilter-3 (f/process (.filter-3 e) (f/set-fq (* (inc (* (.env nadsr-filter-3) ^double patch/filter-amount-3))
                                                              ^double patch/filter-freq-3) patch/filter-q-3) (.out noscillator-3))
         
         ^ADSR nadsr-oscillator-3 (adsr/process (.adsr-oscillator-3 e) patch/adsr-oscillator-3 (.clock nsequencer-3))         
         out-3 (* (.env nadsr-oscillator-3) (.lp nfilter-3))
         
         
         out (dsp/ONE-POLE (+ (* ^double patch/level-1 out-1)
                              (* ^double patch/level-2 out-2)
                              (* ^double patch/level-3 out-3)) (.prev-out e) 0.15)]
     (Engine. (.out e) out
              noscillator-1 phase-oscillator-1 nadsr-oscillator-1 nadsr-filter-1 nfilter-1 nsequencer-1
              noscillator-2 nadsr-oscillator-2 nadsr-filter-2 nfilter-2 nsequencer-2
              noscillator-3 nadsr-oscillator-3 nadsr-filter-3 nfilter-3 nsequencer-3))))

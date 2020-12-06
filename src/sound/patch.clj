(ns sound.patch
  (:require [sound.clock :as clock]
            [sound.oscillator :as o]
            [sound.adsr :as adsr]
            [sound.sequencer :as sr]
            [sound.filter :as f]))

;; sequencer 1
(def clock-1 (clock/init-params 300 0.4))

(def phase-oscillator-1 (o/init-params 10 20 0.7))
(def phase-note-shift -12.05)
(def oscillator-1 (o/init-params 15 100 0.7 0.01))
(def adsr-oscillator-1 (adsr/init-params 0.0 0.6 0.7 0.57))

(def adsr-filter-1 (adsr/init-params 0.25 0.6 0.9 0.57))
(def filter-amount-1 8)
(def filter-freq-1 0.01)
(def filter-q-1 5.0)

(def level-1 0.8)

(def sequence-1 (sr/init-sequence :F3  :A3  :B3  :rst
                                  :F3  :A3  :B3  :rst
                                  :F3  :A3  :B3  :E4
                                  :D4  :rst :B3  :C4
                                  :B3  :G3  :E3  :rst
                                  :rst :rst :rst :D3
                                  :E3  :G3  :E3  :rst
                                  :rst :rst :rst :rst))


;; sequencer 2
(def clock-2 (clock/init-params 150 0.7))
(def oscillator-2 (o/init-params 20 90 0.5))
(def adsr-oscillator-2 (adsr/init-params 0.3 0.01 0.7 0.45))

(def adsr-filter-2 (adsr/init-params 0.2 0.01 1.0 0.3))
(def filter-amount-2 0.1)
(def filter-freq-2 0.25)
(def filter-q-2 10.0)

(def level-2 0.8)

(def sequence-2 (sr/init-sequence :F2 :F2
                                  :F2 :F3
                                  :F2 :F2
                                  :F2 :F3
                                  :E2 :E2
                                  :E2 :C2
                                  :E2 :E2
                                  :E2 :C3))

;; sequencer 3
(def clock-3 (clock/init-params 300 0.3))
(def oscillator-3 (o/init-params 10 100 0.5))
(def adsr-oscillator-3 (adsr/init-params 0.05 0.5 0.9 0.4))

(def adsr-filter-3 (adsr/init-params 0.05 0.2 1.0 0.50))
(def filter-amount-3 0.5)
(def filter-freq-3 0.4)
(def filter-q-3 10.0)

(def level-3 0.3)

(def sequence-3 (sr/init-sequence :A5 :C5 :D5 :C5
                                  :A5 :C5 :D5 :C5
                                  :A5 :C5 :D5 :C5
                                  :A5 :C5 :D5 :C5
                                  :E5 :G5 :B5 :G5
                                  :E5 :G5 :B5 :G5
                                  :E5 :G5 :B5 :G5
                                  :E5 :G5 :B5 :G5))

(ns sound.patch
  (:require [sound.clock :as clock]
            [sound.oscillator :as o]
            [sound.adsr :as adsr]
            [sound.sequencer :as sr]
            [sound.filter :as f]
            [sound.waveshaper :as ws]
            [sound.dsp :as dsp]))

;; LIVE CODING ENABLED, CHANGE ANY VALUE BELOW

;; Lost Woods from Zelda - Koji Kondo

;; sequencer 1 - melody

;; 300 BPM with gate open 40% of time between triggers
(def clock-1 (clock/init-params 300 0.3))

;; use waveforms 10 and 20, interpolate them with 30-70% for phase modulation
(def phase-oscillator-1 (o/init-params 10 20 0.7))

;; phase modulation uses the original sequencer pitch which can be changes (below, go down 12 semitones with some slight detune)
(def phase-note-shift -12.05)

;; main oscillator, waveforms 15 and 100 are used, interpolated 30-70%, amount of phase modulation = 0.007
(def oscillator-1 (o/init-params 15 100 0.7 0.007))

;; waveshaper, mixed sine wave, amount of shaping 0.2, bipolar
(def waveshaper-1 (ws/init-params 0 256 0.5 0.9 false))

;; attack-delay-sustain-release for amplitude
(def adsr-oscillator-1 (adsr/init-params 0.0 0.6 0.7 0.57))

;; ADSR for filter
(def adsr-filter-1 (adsr/init-params 0.25 0.5 0.9 0.5))

;; ADSR filter amount
(def filter-amount-1 7)

;; filter frequency (probably ratio pitch / rate - haven't figure it out yet)
(def filter-freq-1 0.003)

;; filter resonance
(def filter-q-1 10.0)

;; track level
(def level-1 0.15)

;; score
(def sequence-1 (sr/init-sequence :F3  :A3  :B3  :rst
                                  :F3  :A3  :B3  :rst
                                  :F3  :A3  :B3  :E4
                                  :D4  :rst :B3  :C4
                                  :B3  :G3  :E3  :rst
                                  :rst :rst :rst :D3
                                  :E3  :G3  :E3  :rst
                                  :rst :rst :rst :rst))


;; sequencer 2 - bass line
(def clock-2 (clock/init-params 150 0.7))
(def oscillator-2 (o/init-params 20 90 0.5))
(def adsr-oscillator-2 (adsr/init-params 0.2 0.05 0.7 0.4))

(def adsr-filter-2 (adsr/init-params 0.2 0.01 1.0 0.3))
(def filter-amount-2 0.1)
(def filter-freq-2 0.03)
(def filter-q-2 3.0)

(def level-2 0.5)

(def sequence-2 (sr/init-sequence :F2 :F2
                                  :F2 :F3
                                  :F2 :F2
                                  :F2 :F3
                                  :E2 :E2
                                  :E2 :C2
                                  :E2 :E2
                                  :E2 :C3))

;; sequencer 3 - accompaniment
(def clock-3 (clock/init-params 300 0.3))
(def oscillator-3 (o/init-params 10 100 0.5))
(def adsr-oscillator-3 (adsr/init-params 0.05 0.5 0.9 0.4))

(def adsr-filter-3 (adsr/init-params 0.05 0.2 1.0 0.50))
(def filter-amount-3 0.5)
(def filter-freq-3 0.4)
(def filter-q-3 10.0)

(def level-3 0.2)

(def sequence-3 (sr/init-sequence :A5 :C5 :D5 :C5
                                  :A5 :C5 :D5 :C5
                                  :A5 :C5 :D5 :C5
                                  :A5 :C5 :D5 :C5
                                  :E5 :G5 :B5 :G5
                                  :E5 :G5 :B5 :G5
                                  :E5 :G5 :B5 :G5
                                  :E5 :G5 :B5 :G5))

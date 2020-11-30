(ns intro
  (:require [notespace.api :as api]
            [notespace.kinds :as k]
            [sound.waveforms :refer :all]
            [sound.adsr :as adsr]
            [sound.engine :as e]
            [sound.filter :as filt]
            [fastmath.core :as m])
  (:import [sound.engine WaveTableEngine]
           [sound.filter SVF SVFResult]))

^k/hidden
(defn line
  [data]
  [:p/vega {      ;:$schema "https://vega.github.io/schema/vega-lite/v4.json"
            :mode "vega-lite"
            :width 400
            :height 200
            :description "A simple bar chart with embedded data."
            :data {:values (map (partial zipmap [:x :y]) (map vector (range) data))}
            :mark {:type "line"
                   :interpolate "monotone"} 
            :encoding {:x {:field "x" :type "quantitative"}
                       :y {:field "y" :type "quantitative"}}}])


^k/hiccup
(line (analog-sine 6))

^k/hiccup
(line (fft-interpolate (analog-sine 6 128)))


^k/hiccup
(line (sine 6))

^k/hiccup
(line (fft-interpolate (sine 6 128)))

^k/hiccup
(line (comb 6))

^k/hiccup
(line (fft-interpolate (comb 6 128)))

;;

^k/hiccup
(line (pair 25))

^k/hiccup
(line (fft-interpolate (pair 25 128)))

;;

^k/hiccup
(line (analog-tri 3))

^k/hiccup
(line (fft-interpolate (analog-tri 3 128)))


^k/hiccup
(line (tri 3 2))

^k/hiccup
(line (fft-interpolate (tri 3 2 128)))

;;

^k/hiccup
(line (tri-stack 6))

^k/hiccup
(line (fft-interpolate (tri-stack 6 128)))

;;

^k/hiccup
(line (analog-saw 4))

^k/hiccup
(line (fft-interpolate (analog-saw 4 128)))


^k/hiccup
(line (saw 6 1))

^k/hiccup
(line (fft-interpolate (saw 6 1 128)))

;;

^k/hiccup
(line (saw-stack 3))

^k/hiccup
(line (fft-interpolate (saw-stack 3 128)))

;;

^k/hiccup
(line (square 3))

^k/hiccup
(line (fft-interpolate (square 3 128)))

;;

^k/hiccup
(line (quadra 4))

^k/hiccup
(line (fft-interpolate (quadra 4 128)))

;;


^k/hiccup
(line (drawbars (first bars1)))

^k/hiccup
(line (fft-interpolate (drawbars (first bars1) 128)))

^k/hiccup
(line (drawbars (second bars2)))

^k/hiccup
(line (fft-interpolate (drawbars (second bars2) 128)))

;;

^k/hiccup
(line (pulse 0.01))

^k/hiccup
(line (fft-interpolate (pulse 0.01 128)))

;;

^k/hiccup
(line (burst 0.2))

^k/hiccup
(line (fft-interpolate (burst 0.2 128)))

;;

^k/hiccup
(line (hybrid 0.1))

^k/hiccup
(line (fft-interpolate (hybrid 0.1 128)))

;;

^k/hiccup
(line (trisaw 4.5))

^k/hiccup
(line (fft-interpolate (trisaw 4.5 128)))


;;


^k/hiccup
(line (sawtri 4.5))

^k/hiccup
(line (fft-interpolate (sawtri 4.5 128)))

;;


^k/hiccup
(line (square-formant 8))

^k/hiccup
(line (fft-interpolate (square-formant 8 128)))

;;

^k/hiccup
(line (saw-formant 8))

^k/hiccup
(line (fft-interpolate (saw-formant 8 128)))

;;

^k/hiccup
(line (bandpass-formant 8))

^k/hiccup
(line (fft-interpolate (bandpass-formant 8 128)))


;;

^k/hiccup
(line (sine-power 0.3))

^k/hiccup
(line (fft-interpolate (sine-power 0.3 128)))

^k/hiccup
(line (sine-power 3))

^k/hiccup
(line (fft-interpolate (sine-power 3 128)))


;;

^k/hiccup
(line (formant-f 2))

^k/hiccup
(line (fft-interpolate (formant-f 2 128)))


;;

^k/hiccup
(line (digi-formant-f 2))

^k/hiccup
(line (fft-interpolate (digi-formant-f 2 128)))


;;

^k/hiccup
(line (signal :saw 4))

^k/hiccup
(line (fft-interpolate (signal :saw 4 128)))

^k/hiccup
(line (signal :square 4))

^k/hiccup
(line (fft-interpolate (signal :square 4 128)))

^k/hiccup
(line (signal :cut-triangle 8))

^k/hiccup
(line (fft-interpolate (signal :cut-triangle 8 128)))

^k/hiccup
(line (signal :triangle 4))

^k/hiccup
(line (fft-interpolate (signal :triangle 4 128)))

^k/hiccup
(line (signal :noise 14))

^k/hiccup
(line (fft-interpolate (signal :noise 14 128)))

;;


^k/hiccup
(line (wave 32))

^k/hiccup
(line (fft-interpolate (wave 32)))

;;

^k/hiccup
(line (integrate-signal (fft-interpolate (wave 32))))

^k/hiccup
(line (integrate-signal (sine 5)))

^k/hiccup
(line (integrate-signal (digi-formant-f 3)))


;;

(def adsr-params (adsr/->ADSRParams 0.3 0.2 0.5 0.3))
(def adsr (adsr/->ADSR adsr-params))

(def adsr-vals
  (let [a (take 5000 (iterate #(adsr/process % true) adsr))
        b (take 10000 (iterate #(adsr/process % false) (last a)))]
    (map #(.env ^sound.adsr.ADSR %) (concat a b))))

^k/hiccup
(line adsr-vals)

;;

(def from-engine
  (take 1000 (map #(.out ^WaveTableEngine %) (iterate e/render (e/init 30 40)))))

^k/hiccup
(line from-engine)

(defn apply-filter
  [xs]
  (reduce (fn [[f buff] sample]
            (let [^SVF nf (filt/process f sample)]
              [nf (conj buff (.lp ^SVFResult (.result nf)))])) [(filt/set-fq (filt/init) 0.02 10) []] xs))

^k/hiccup
(line (second (apply-filter from-engine)))

^k/hiccup
[:audio {:controls true}
 [:source {:src "../a.wav" :type "audio/wav"}]]

^k/hiccup
(line (m/sample (filt/TAN :dirty) 0 0.3 500))

^k/hiccup
(line (m/sample (filt/TAN :fast) 0 0.3 500))

^k/hiccup
(line (m/sample (filt/TAN :accurate) 0 0.3 500))

^k/hiccup
(line (m/sample (filt/TAN :exact) 0 0.3 500))

;;


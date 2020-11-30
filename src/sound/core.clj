(ns sound.core
  (:require [fastmath.core :as m]
            [fastmath.stats :as stats]
            [sound.waveforms :as wv]
            [sound.engine :as e]
            [sound.dsp :as dsp]
            [sound.adsr :as adsr]
            [sound.filter :as filt]
            [clojure.java.io :as io])
  (:import [javax.sound.sampled AudioFormat AudioFileFormat$Type AudioSystem SourceDataLine AudioInputStream]
           [org.jtransforms.fft DoubleFFT_1D]
           [sound.engine WaveTableEngine]
           [sound.filter SVF SVFResult]))

(def af (AudioFormat. dsp/RATE 8 1 true true))


(def ^SourceDataLine dl (AudioSystem/getSourceDataLine af))

(class dl)
;; => com.sun.media.sound.DirectAudioDevice$DirectSDL

(.open dl)

(.start dl)

(comment (.drain dl)

         (.stop dl)

         (.close dl)

         (.available dl))

(def adsr-params (adsr/->ADSRParams 0.3 0.4 0.5 0.5))
(def adsr (adsr/->ADSR adsr-params))

(/ 44100 4)

(def adsr-vals
  (let [a (take 5000 (iterate #(adsr/process % true) adsr))
        b (take (- 11025 5000) (iterate #(adsr/process % false) (last a)))]
    (map #(.env ^sound.adsr.ADSR %) (concat a b))))

(defn ->arr
  ([w adsr]
   (byte-array (take dsp/RATE (cycle (map (fn [^double x ^double env] (short (* 80.0 env x))) (wv/normalize w) (cycle adsr))))))
  ([w]
   (byte-array (take dsp/RATE (cycle (map (fn [^double x ^double env] (short (* 80.0 x))) (wv/normalize w)))))))

(defn save-wave
  [filename w]
  (let [f (io/file filename)
        is (AudioInputStream. (java.io.ByteArrayInputStream. w) af dsp/RATE)]
    (AudioSystem/write is AudioFileFormat$Type/WAVE f)
    (.close is)))

(defn play-wave
  [w]
  (let [n (->arr w adsr-vals)]
    (.write ^SourceDataLine dl n 0 22050)
    (.write ^SourceDataLine dl n 22050 22050)))

(defn play2
  [w & p]
  (let [p (vec p)]
    (play-wave (apply w p))
    (play-wave (wv/fft-interpolate (apply w (conj p 128))))))


(comment 
  (play-wave (wv/integrate-signal (wv/sine 1)))
  (play-wave (wv/fft-interpolate (wv/integrate-signal (wv/sine 1 128))))
  )

(play2 wv/analog-sine 2)

(defn apply-filter
  [xs]
  (reduce (fn [[f buff] sample]
            (let [^SVF nf (filt/process f sample)]
              [nf (conj buff (.lp ^SVFResult (.result nf)))])) [(filt/set-fq (filt/init) 0.1 100) []] xs))

(def note 50) ;; middle C
(def sample-id 30) ;; from 0-51
(play-wave (second (apply-filter (take dsp/RATE (map #(.out ^WaveTableEngine %) (iterate e/render (e/init sample-id note)))))))

(save-wave "a.wav" (->arr (take dsp/RATE (drop 100 (map #(.out ^WaveTableEngine %) (iterate e/render (e/init 40 60))))) adsr-vals))


(apply-filter [-1 1])

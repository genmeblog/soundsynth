(ns sound.core
  (:require [fastmath.core :as m]
            [fastmath.stats :as stats]
            [sound.waveforms :as wv]
            [sound.oscillator :as o]
            [sound.dsp :as dsp]
            [sound.adsr :as adsr]
            [sound.filter :as filt]
            [clojure.java.io :as io]
            [sound.engine :as e])
  (:import [javax.sound.sampled AudioFormat AudioFileFormat$Type AudioSystem SourceDataLine AudioInputStream]
           [sound.engine Engine]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defonce playing? (atom false))

(def level 0.9)

(defn quantize
  ^long [^double sample]
  (m/round-even (m/constrain (* ^double level 127.5 sample) -128.0 127.0)))

(defn play
  [^SourceDataLine dl]
  (try
    (let [size 256 ;; local buffer size
          buffer (byte-array size)]
      (loop [engine (e/engine)]
        (if @playing?
          ;; produce new samples only when there is space to store it, wait a little bit otherwise
          (if (> (.available ^SourceDataLine dl) size) 
            (let [curr-engine (loop [idx (int 0)
                                     curr-engine engine]
                                (if (< idx size)
                                  (let [^Engine new-engine (e/engine curr-engine)]
                                    (aset ^bytes buffer idx (byte (quantize (.out new-engine))))
                                    (recur (inc idx) new-engine))
                                  curr-engine))]
              (.write ^SourceDataLine dl buffer 0 size) ;; feed sound to data line
              (recur curr-engine))
            (do
              ;; (println "waiting")
              (Thread/sleep 1)
              (recur engine)))
          (println :stopped))))
    (catch Exception e (println e))))

(def af (AudioFormat. dsp/RATE 8 1 true true))
(defonce ^SourceDataLine dl (AudioSystem/getSourceDataLine af))

(defn toggle-playing
  []
  (if-not @playing?
    (do
      (reset! playing? true)
      (.open ^SourceDataLine dl af 2048)
      (.start dl)
      (future (play dl)))
    (do
      (reset! playing? false)
      (.drain dl)
      (.stop dl)
      (.close dl))))

(toggle-playing)

(def song (map #(quantize (.out ^Engine %)) (iterate e/engine (e/engine))))

#_(with-open [w (io/output-stream "song.raw")]
    (doseq [b (take (int (* 20 dsp/RATE)) song)]
      (.write w (byte b))))

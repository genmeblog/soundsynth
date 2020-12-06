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

(def playing? (atom false))

(def level 0.9)

(defn play
  [^SourceDataLine dl]
  (try
    (let [size 16
          buffer (byte-array size)]
      (loop [engine (e/engine)]
        (if @playing?
          (if (> (.available ^SourceDataLine dl) size)
            (let [curr-engine (loop [idx (int 0)
                                     curr-engine engine]
                                (if (< idx size)
                                  (let [^Engine new-engine (e/engine curr-engine)]
                                    (aset ^bytes buffer idx
                                          (byte (m/constrain (* ^double level 127.5 (.out new-engine)) -128.0 127.0)))
                                    (recur (inc idx) new-engine))
                                  curr-engine))]
              (.write ^SourceDataLine dl buffer 0 size)
              (recur curr-engine))
            (do
              ;; (println "waiting")
              (Thread/sleep 1)
              (recur engine)))
          (println :stopped))))
    (catch Exception e (println e))))

(def af (AudioFormat. dsp/RATE 8 1 true true))
(def ^SourceDataLine dl (AudioSystem/getSourceDataLine af))

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


(ns sound.sequencer
  (:require [sound.clock]
            [fastmath.core :as m])
  (:import [sound.clock Clock]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def sharp-flat [""  "#" "b" ""  "#" "b" ""  ""  "#" "b" ""  "#" "b" ""  "#" "b" ""])
(def notes      ["C" "C" "D" "D" "D" "E" "E" "F" "F" "G" "G" "G" "A" "A" "A" "B" "B"])
(def offsets    [24  25  25  26  27  27  28  29  30  30  31  32  32  33  34  34  35])

(def ^:const ^byte REST -1)
(def ^:const ^byte LAST -2)

(def note->midi (-> (->> (range -1 8 1)
                         (mapcat (fn [^long octave]
                                   (map (fn [note sf ^long offset]
                                          [(keyword (str note (inc octave) sf)) (+ offset (* 12 octave))])
                                        notes sharp-flat offsets)))
                         (into {}))
                    (assoc :rest REST
                           :last LAST)))

(defn init-sequence
  [notes]
  (byte-array (map note->midi notes)))

(defrecord Sequencer [^long idx ^long note])

(defn init
  []
  (Sequencer. 0 0))

(defn process
  [^Sequencer sequencer ^Clock clock ^bytes notes]
  (if (.trigger clock)
    (let [nidx (mod (inc (.idx sequencer)) (alength notes))
          note (aget notes nidx)]
      (condp = note
        REST (Sequencer. nidx REST)
        LAST (Sequencer. nidx (.note sequencer))
        :else (Sequencer. nidx note)))
    sequencer))

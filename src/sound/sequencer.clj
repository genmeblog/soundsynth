(ns sound.sequencer
  (:require [fastmath.core :as m]
            [sound.clock :as clock])
  (:import [sound.clock Clock]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def sharp-flat [""  "#" "b" ""  "#" "b" ""  ""  "#" "b" ""  "#" "b" ""  "#" "b" ""])
(def notes      ["C" "C" "D" "D" "D" "E" "E" "F" "F" "G" "G" "G" "A" "A" "A" "B" "B"])
(def offsets    [24  25  25  26  27  27  28  29  30  30  31  32  32  33  34  34  35])

(def ^:const ^byte REST -1)

(def note->midi (-> (->> (range -1 8 1)
                         (mapcat (fn [^long octave]
                                   (map (fn [note sf ^long offset]
                                          [(keyword (str note (inc octave) sf)) (+ offset (* 12 octave))])
                                        notes sharp-flat offsets)))
                         (into {}))
                    (assoc :rst REST)))

(defn init-sequence
  [& notes]
  (byte-array (map note->midi notes)))

(defrecord Sequencer [^long idx ^long note ^Clock clock])

(defn init
  [clock]
  (Sequencer. -1 0 clock))

(defn process
  [^Sequencer sequencer ^bytes notes clock-params]
  (let [^Clock nclock (clock/process (.clock sequencer) clock-params)
        len (alength notes)]
    (if (and (pos? len) (.trigger nclock))
      (let [nidx (mod (inc (.idx sequencer)) len)
            note (aget notes nidx)]
        (if (= note REST)
          (Sequencer. nidx (.note sequencer) (clock/resting nclock))
          (Sequencer. nidx note nclock)))
      (Sequencer. (.idx sequencer) (.note sequencer) nclock))))

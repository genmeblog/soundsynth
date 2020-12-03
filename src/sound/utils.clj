(ns sound.utils
  (:require [sound.dsp :as dsp]
            [sound.waveforms :as wv]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defn export-array
  [arr typ objname]
  (with-open [w (io/writer (str "c/" objname ".c"))]
    (.write w (str "// " objname "\n"))
    (.write w (str "#define "(str/upper-case objname) "_SIZE " (count arr) "\n"))
    (.write w (str "const " typ " " objname "[] = {\n"))
    (doseq [p (partition-all 16 arr)]
      (.write w (str (str/join ", " (map str p)) ",\n")))
    (.write w (str "};\n")))
  (println (str objname " saved")))

(export-array (map float dsp/pitch-ratio-high) "float" "pitch_ratio_high")
(export-array (map float dsp/pitch-ratio-low) "float" "pitch_ratio_low")
(export-array wv/wavetable "int16_t" "wavetable")


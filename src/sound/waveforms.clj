(ns sound.waveforms
  (:require [fastmath.core :as m]
            [fastmath.stats :as stats]
            [fastmath.vector :as v]
            [fastmath.signal :as sig]
            [fastmath.random :as r]
            [fastmath.interpolation :as i]
            [sound.dsp :as dsp])
  (:import [org.jtransforms.fft DoubleFFT_1D]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def ^:const ^int WAVETABLE_SIZE 256)

(defn normalize
  [s]
  (let [m (stats/mean s)
        n (map (fn [^double v] (- v m)) s)
        mx (stats/maximum (map m/abs n))]
    (map (fn [^double v] (/ v mx)) n)))

(defn fft-interpolate
  "Interpolate waveform to remove aliasing."
  [s]
  (let [cnt (count s)
        s (normalize s)
        fft-in (DoubleFFT_1D. cnt)
        fft-out (DoubleFFT_1D. (+ cnt cnt))
        arr (double-array (concat s (repeat cnt 0.0)))]
    (.realForward fft-in arr)
    (reduce (fn [^doubles arr ^long idx] ;; remove phase
              (let [re (aget arr idx)
                    im (aget arr (inc idx))]
                (aset arr idx 0.0)
                (aset arr (inc idx) (- (m/sqrt (+ (* re re)
                                                  (* im im)))))
                arr)) arr (range 0 (alength arr) 2))
    (.realInverse fft-out arr true)
    arr))



;;

(defn- quadratic-sine-approx
  ^double [^double v]
  (let [phase (m/frac v)
        hphase? (< phase 0.5)
        x (- phase ^double (if hphase? 0.25 0.75))
        v (- 1.0 (* 16.0 x x))]
    (if hphase? v (- v))))

(defn analog-sine
  ([^double frequency] (analog-sine frequency WAVETABLE_SIZE))
  ([^double frequency ^double size]
   (map (fn [^long i] (quadratic-sine-approx (* frequency (/ i size)))) (range (int size)))))

(defn sine
  ([^double frequency]
   (sine frequency WAVETABLE_SIZE))
  ([^double frequency ^double size]
   (map (fn [^long i] (m/sin (* m/TWO_PI (/ i size) frequency))) (range (int size)))))

(defn comb
  ([n]
   (comb n WAVETABLE_SIZE))
  ([^long n ^double size]
   (reduce v/add (map (fn [^long i] (sine (inc i) size)) (range n)))))

(defn pair
  ([n]
   (pair n WAVETABLE_SIZE))
  ([^long n ^double size]
   (reduce v/add (map (fn [^long i]
                        (v/add (-> (sine (inc i) size)
                                   (v/mult (+ i 0.5))
                                   (v/div (dec n)))
                               (-> (sine (* 4 (inc i)) size)
                                   (v/mult (+ i 0.5))
                                   (v/div (dec n))
                                   (v/mult 0.5)))) (range n)))))

(defn- exp-curve
  ^double [^double x]
  (/ (+ 3.0 (* x (+ -13.0 (* 5.0 x))))
     (+ 3.0 x x)))

(defn- analog-tri-approx
  ^double [^double phase]
  (let [x (+ 0.25 (m/frac phase))
        x (- x (m/trunc x))
        hx? (>= x 0.5)
        x (+ x x)
        x (- x (m/trunc x))]
    (if hx?
      (exp-curve x)
      (- (exp-curve x)))))

(defn analog-tri
  ([f] (analog-tri f WAVETABLE_SIZE))
  ([^double f ^double size]
   (map (fn [^long i] (analog-tri-approx (* f (/ i size)))) (range (int size)))))

(defn tri
  ([n] (tri n 1))
  ([n f] (tri n f WAVETABLE_SIZE))
  ([^long n ^long f ^double size]
   (reduce v/add (map (fn [^long i]
                        (let [i2 (inc (+ i i))]
                          (-> (sine (* f i2) size)
                              (v/div i2)
                              (v/sq)))) (range n)))))

(defn tri-stack
  ([n] (tri-stack n WAVETABLE_SIZE))
  ([^long n ^double size]
   (reduce v/add (map (fn [^long i] (tri (+ 15 (* 5 n)) (+ i (/ n 3)) size)) (range n)))))

(defn analog-saw
  ([f] (analog-saw f WAVETABLE_SIZE))
  ([^double f ^double size]
   (map (fn [^long i] (let [x (+ 0.5 (* f (/ i size)))
                           x (- x (m/trunc x))]
                       (- (exp-curve x)))) (range (int size)))))


(defn saw
  ([n] (saw n 1))
  ([n f] (saw n f WAVETABLE_SIZE))
  ([^long n ^double f ^double size]
   (reduce v/add (map (fn [^long i]
                        (-> (sine (* f (inc i)) size)
                            (v/div (inc i)))) (range n)))))

(defn saw-stack
  ([n] (saw-stack n WAVETABLE_SIZE))
  ([^long n ^double size]
   (reduce v/add (map (fn [^long i]
                        (-> (saw (inc (* 6 i)) (inc i) size)
                            (v/div (m/sqrt (inc i))))) (range n)))))

(defn square
  ([n] (square n WAVETABLE_SIZE))
  ([^long n ^double size]
   (reduce v/add (map (fn [^long i]
                        (-> (sine (inc (+ i i)) size)
                            (v/div (inc (+ i i))))) (range n)))))

(defn quadra
  ([n] (quadra n WAVETABLE_SIZE))
  ([^long n ^double size]
   (reduce v/add (map (fn [^long h ^double a]
                        (-> (sine (+ 1 n n h h) size)
                            (v/mult a))) (range 4) [1 0.5 1 0.5]))))

(defn drawbars
  ([bars] (drawbars bars WAVETABLE_SIZE))
  ([bars ^double size]
   (reduce v/add (map (fn [bar ^long freq]
                        (-> (sine freq size)
                            (v/mult (/ (Integer/parseInt (str bar)) 8.0)))) bars [1.0, 3.0, 2.0, 4.0, 6.0, 8.0, 10.0, 12.0, 16.0]))))

(def bars1 ["688600000"
            "686040000"
            "666806000"
            "655550600"
            "665560060" 
            "688500888"
            "660000888"
            "060000046"])

(def bars2 ["867000006" 
            "888876788" 
            "668744354"
            "448644054"
            "327645222"
            "204675300"
            "002478500"
            "002050321"])

(defn pulse
  ([duty] (pulse duty WAVETABLE_SIZE))
  ([^double duty ^double size]
   (let [last (dec (int size))]
     (map (fn [^long i]
            (let [v (/ (if (= i last) 0 i) size)]
              (if (< v duty) 1.0 -1.0))) (range (int size))))))

(defn burst
  ([duty] (burst duty WAVETABLE_SIZE))
  ([^double duty ^double size]
   (let [last (dec (int size))
         d (m/sqrt duty)]
     (v/emult (map (fn [^long i]
                     (let [v (/ (if (= i last) 0 i) size)]
                       (if (< v d) 1.0 0.0))) (range (int size)))
              (sine (/ duty) size)))))

(defn hybrid
  ([duty] (hybrid duty WAVETABLE_SIZE))
  ([^double duty ^double size]
   (let [isize (int size)
         c (map (fn [^long i]
                  (mod (int (+ i (* size (- duty 0.5)))) isize)) (range isize))
         x (pulse duty size)
         s- (vec (saw 80 1 size))
         s (map #(s- %) c)]
     (v/add x s))))

(defn trisaw
  ([harmonic] (trisaw harmonic WAVETABLE_SIZE))
  ([^double harmonic ^double size]
   (v/add (tri 80 1 size)
          (-> (saw 80 harmonic size)
              (v/mult (if (not= harmonic 1.0) 0.5 0.125))))))

(defn sawtri
  ([harmonic] (sawtri harmonic WAVETABLE_SIZE))
  ([^double harmonic ^double size]
   (v/add (v/mult (saw 80 1 size) 0.5)
          (-> (tri 80 harmonic size)
              (v/mult (if (not= harmonic 1.0) 1.0 0.25))))))

(defn square-formant
  ([ratio] (square-formant ratio WAVETABLE_SIZE))
  ([^double ratio ^double size]
   (let [r (m/sqrt ratio)
         phase (map (fn [^long i]
                      (let [p (* (/ i size) r 0.5)]
                        (min p 1.0))) (range (int size)))
         amp (map (fn [^double p] (* 0.5 (inc (m/cos (* p m/PI))))) phase)]
     (v/emult amp
              (map (fn [^double v] (inc v)) (sine (* ratio 0.75) size))))))

(defn saw-formant
  ([ratio] (saw-formant ratio WAVETABLE_SIZE))
  ([^double ratio ^double size]
   (let [amp (map (fn [^long i]
                    (* 0.5 (- 1.0 (/ i size)))) (range (int size)))]
     (v/emult amp
              (map (fn [^double v] (inc v)) (sine ratio size))))))

(defn bandpass-formant
  ([ratio] (bandpass-formant ratio WAVETABLE_SIZE))
  ([^double ratio ^double size]
   (let [amp (map (fn [^long i]
                    (* 0.5 (- 1.0 (/ i size)))) (range (int size)))]
     (v/emult amp
              (map (fn [^double v] (inc v)) (sine (* 1.5 ratio) size))))))

(defn sine-power
  ([power] (sine-power power WAVETABLE_SIZE))
  ([^double power ^double size]
   (let [p (m/pow 2.0 power)]
     (map #(let [s (m/sgn %)]
             (* s (m/pow (m/abs %) p))) (v/add (sine 1.0 size)
                                               (saw 16 1 size))))))
(defn formant-f
  ([index] (formant-f index WAVETABLE_SIZE))
  ([^long index ^double size]
   (let [f1 (/ (* 3.9 (inc index)) 8.0)
         f2 (* f1 (- 1.0 (m/cos (* f1 m/PI 0.8))))
         t (map (fn [^long i]
                  (/ i size)) (range (int size)))
         a1 (map (fn [^double v] (* (m/pow (- 1.0 v) 0.2)
                                   (m/exp (* -4.0 v)))) t)
         a2 (map (fn [^double v] (* (m/pow (- 1.0 v) 0.2)
                                   (m/exp (* -2.0 v)))) t)
         f3 (-> a2
                (v/emult (sine (inc (* 2.8 (+ f1 f2))) size))
                (v/mult 1.7))
         f1 (-> a1
                (v/emult (sine (inc (* 3.0 f1)) size)))
         f2 (-> a2
                (v/emult (sine (inc (* 4.0 f2)) size))
                (v/mult 1.5))]
     (v/add (v/add f1 f2) f3))))

(defn distort
  ^double [^double x]
  (/ (m/atan (* 8.0 x)) m/PI))

(defn digi-formant-f
  ([index] (digi-formant-f index WAVETABLE_SIZE))
  ([^long index ^double size]
   (let [f1 (/ (* 3.9 (inc index)) 8.0)
         f2 (* f1 (- 1.0 (m/cos (* f1 m/PI 0.8))))
         t (map (fn [^long i]
                  (/ i size)) (range (int size)))
         a1 (map (fn [^double v] (* (m/pow (- 1.0 v) 0.2)
                                   (m/exp (* -4.0 v)))) t)
         a2 (map (fn [^double v] (* (m/pow (- 1.0 v) 0.2)
                                   (m/exp (* -2.0 v)))) t)
         f3 (-> a2
                (v/emult (map distort (sine (inc (* 2.9 (+ f1 f2))) size)))
                (v/mult 0.7))
         f1 (-> a1
                (v/emult (map distort (sine (inc (* 3.2 f1)) size))))
         f2 (-> a2
                (v/emult (map distort (sine (inc (* 4.1 f2)) size)))
                (v/mult 0.7))]
     (v/add (v/add f1 f2) f3))))

;; fastmath

(defmethod sig/oscillator :noise1 [_ ^double f ^double a ^double p]
  (let [n (r/fbm-noise {:noise-type :simplex
                        :octaves 2
                        :normalize? false
                        :seed 1337})]
    (fn ^double [^double x]
      (* a ^double (n (* (+ p x) f) 1.23456789)))))

(defmethod sig/oscillator :noise2 [_ ^double f ^double a ^double p]
  (let [n (r/fbm-noise {:noise-type :value
                        :octaves 2
                        :interpolation :none
                        :normalize? false
                        :seed 1337})]
    (fn ^double [^double x]
      (* a ^double (n (* (+ p x) f) 1.23456789)))))

(defmethod sig/oscillator :noise3 [_ ^double f ^double a ^double p]
  (let [n (r/billow-noise {:noise-type :value
                           :octaves 2
                           :interpolation :linear
                           :normalize? false
                           :seed 1337})]
    (fn ^double [^double x]
      (* a ^double (n (* (+ p x) f) 1.23456789)))))

(defmethod sig/oscillator :noise4 [_ ^double f ^double a ^double p]
  (let [n (r/ridgedmulti-noise {:noise-type :value
                                :octaves 2
                                :interpolation :hermite
                                :normalize? false
                                :seed 1337})]
    (fn ^double [^double x]
      (* a ^double (n (* (+ p x) f) 1.23456789)))))

(defn signal
  ([type freq] (signal type freq WAVETABLE_SIZE))
  ([type ^double freq ^double size]
   (let [t (sig/oscillator type freq 1 0)]
     (map (fn [^long i]
            (t (/ i size))) (range (int size))))))

;; braids

(def gains (conj (map (fn [^double x]
                        (m/exp (* 0.184 x))) (range 1 16)) 0.0))

(def formant-square-256 (concat (take 128 (cycle gains))
                                (take 128 (cycle (map (fn [^double v] (- v)) gains)))))

(def formant-square-128 (take-nth 2 formant-square-256))


(def formant-sine-256
  (mapcat (fn [^double x]
            (map (fn [^double g]
                   (* g 4.0 x)) gains))
          (map (fn [^long i]
                 (m/sin (* m/TWO_PI (/ i 16.0)))) (range 16))))

(def formant-sine-128 (take-nth 2 formant-sine-256))

;;

(defn wrap [^long size] (map (fn [^long i] (mod (+ i (/ size 2)) size)) (range (inc size))))
(defn step [^long size] (map (fn [^long i] (mod (+ i (/ size 32)) size)) (range (inc size))))
(defn fill [^long size] (map (fn [^long i] (mod i size)) (range (inc size))))

(defn- bandlimited-comb-
  ([^long zone ^long size]
   (let [f0 (if (>= zone 14)
              (dec (/ dsp/RATE 2.0))
              (min (/ dsp/RATE 2.0) (* 440.0 (m/pow 2.0 (/ (- (+ 18.0 (* 8.0 zone)) 69.0) 12.0)))))
         period (/ dsp/RATE f0)
         m (inc (* 2.0 (m/floor (/ period 2.0))))
         pulse (mapv (fn [^long i]
                       (if (zero? i)
                         1.0
                         (let [i (/ i (double size))]
                           (/ (m/sin (* m/PI i m))
                              (+ 1.0e-9 (* m (m/sin (* m/PI i)))))))) (range (/ size -2) (/ size 2)))]
     (mapv #(pulse %) (fill size)))))

(defn bandlimited-comb
  ([^long zone] (bandlimited-comb zone WAVETABLE_SIZE))
  ([^long zone ^long size]
   (let [pulse (bandlimited-comb- zone size)]
     (butlast pulse))))

(defn bandlimited-square
  ([^long zone] (bandlimited-square zone WAVETABLE_SIZE))
  ([^long zone ^long size]
   (let [pulse (bandlimited-comb- zone size)]
     (butlast (reductions m/fast+ (map-indexed (fn [^long i ^long w]
                                                 (- ^double (pulse i) ^double (pulse w))) (wrap size)))))))

(defn bandlimited-triangle
  ([^long zone] (bandlimited-triangle zone WAVETABLE_SIZE))
  ([^long zone ^long size]
   (let [pulse (bandlimited-comb- zone size)
         square (reductions m/fast+ (map-indexed (fn [^long i ^long w]
                                                   (- ^double (pulse i) ^double (pulse w))) (wrap size)))
         smean (stats/mean square)]
     (butlast (map (fn [^double v]
                     (/ (- v) size)) (reductions m/fast+ (map (fn [^double s]
                                                                (- s smean)) (reverse square))))))))

(defn bandlimited-nes-triangle
  ([^long zone] (bandlimited-nes-triangle zone WAVETABLE_SIZE))
  ([^long zone ^long size]
   (let [pulse (bandlimited-comb- zone size)
         steps (step size)
         nes-square (first (reduce (fn [[nes-triangle curr-pulse] ^long i]
                                     (let [m (if (< i 16) 1.0 -1.0)]
                                       [(map (fn [^double x ^double y]
                                               (+ y (* m x))) curr-pulse nes-triangle)
                                        (mapv #(curr-pulse %) steps)])) [(repeat (inc size) 0.0)
                                                                         pulse] (range 32)))]
     (butlast (map (fn [^double v] (- v))
                   (reductions m/fast+ nes-square))))))

;;

(defn slurp-bytes
  "Slurp the bytes from a slurpable thing"
  [x]
  (let [t (with-open [out (java.io.ByteArrayOutputStream.)]
            (clojure.java.io/copy (clojure.java.io/input-stream x) out)
            (.toByteArray out))]
    (map #(- (m/bit-and 0xff (byte %)) 128.0) t)))

(def waves (slurp-bytes "waves.bin"))

(defn wave
  [idx & _]
  (take 128 (drop (* 129 ^long idx) waves)))

(defn resample
  ([xs] (resample xs 128))
  ([xs cnt]
   (let [i (i/monotone (range (count xs)) xs)]
     (m/sample i 0 (count xs) cnt))))

(def ^:const ^double scaler (/ (* 4.0 32768.0) WAVETABLE_SIZE))

(defn integrate-signal
  [xs]
  (let [z (double (reduce m/fast+ (take 4 xs)))
        n (count xs)
        x (reductions m/fast+ (normalize (map (fn [^double v] (+ v z)) (concat xs xs))))
        xmean (stats/mean x)
        x (map (fn [^double v] (m/round (* scaler (- v xmean)))) x)]
    (drop (- n 4) x)))

(def wavetable
  (short-array (mapcat integrate-signal (mapcat identity [(map (comp fft-interpolate wave) (range 256))
                                                          (map sine [1 2 3 4 5 8 12 16 32])]))))

(def ^:const ^int table-size 260)
(def ^:const ^double table-size-f 256.0)

(defn read-wave
  ^double [^long off ^long phase-integral ^double phase-fractional]
  (dsp/interpolate-wave-hermite wavetable (+ off phase-integral) phase-fractional))

(ns sound.filter
  (:require [fastmath.core :as m]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def ^:const ^double M_PI_POW_2 (* m/M_PI m/M_PI))
(def ^:const ^double M_PI_POW_3 (* M_PI_POW_2 m/M_PI))
(def ^:const ^double M_PI_POW_5 (* M_PI_POW_3 M_PI_POW_2))
(def ^:const ^double M_PI_POW_7 (* M_PI_POW_5 M_PI_POW_2))
(def ^:const ^double M_PI_POW_9 (* M_PI_POW_7 M_PI_POW_2))
(def ^:const ^double M_PI_POW_11 (* M_PI_POW_9 M_PI_POW_2))

(def ^:const ^double DIRTY_A (* 3.739e-01 M_PI_POW_3))
(def ^:const ^double FAST_A (* 3.26e-01 M_PI_POW_3))
(def ^:const ^double FAST_B (* 1.823e-01 M_PI_POW_5))
(def ^:const ^double ACCURATE_A (* 3.333314036e-01 M_PI_POW_3))
(def ^:const ^double ACCURATE_B (* 1.333923995e-01 M_PI_POW_5))
(def ^:const ^double ACCURATE_C (* 5.33740603e-02 M_PI_POW_7))
(def ^:const ^double ACCURATE_D (* 2.900525e-03 M_PI_POW_9))
(def ^:const ^double ACCURATE_E (* 9.5168091e-03 M_PI_POW_11))

(defn tan-exact
  ^double [^double f]
  (m/tan (* m/M_PI (min f 0.497))))

(defn tan-dirty
  ^double [^double f]
  (* f (+ m/M_PI (* f f DIRTY_A))))

(defn tan-fast
  ^double [^double f]
  (let [f2 (* f f)]
    (->> FAST_B
         (* f2)
         (+ FAST_A)
         (* f2)
         (+ m/M_PI)
         (* f))))

(defn tan-accurate
  ^double [^double f]
  (let [f2 (* f f)]
    (->> ACCURATE_E
         (* f2)
         (+ ACCURATE_D)
         (* f2)
         (+ ACCURATE_C)
         (* f2)
         (+ ACCURATE_B)
         (* f2)
         (+ ACCURATE_A)
         (* f2)
         (+ m/M_PI)
         (* f))))

(defmacro TAN
  ([type]
   (case type
     :dirty `tan-dirty
     :fast `tan-fast
     :accurate `tan-accurate
     `tan-exact))
  ([type x]
   (case type
     :dirty `(tan-dirty ~x)
     :fast `(tan-fast ~x)
     :accurate `(tan-accurate ~x)
     `(tan-exact ~x))))

(defrecord SVFParams [^double g ^double r ^double h])

(defn set-gq
  ^SVFParams [^double g ^double q]
  (let [r (/ q)]
    (SVFParams. g r (/ (+ 1.0 (* r g) (* g g))))))

(defmacro set-fq
  ([type f q]
   `(set-gq (TAN ~type ~f) ~q))
  ([f q]
   `(set-fq :dirty ~f ~q))
  ([f]
   `(set-fq ~f 100.0))
  ([]
   `(set-fq 0.01 100.0)))

(defrecord SVF [^double state1 ^double state2
                ^double lp ^double bp ^double bpn ^double hp])

(defn init
  ^SVF []
  (SVF. 0.0 0.0 0.0 0.0 0.0 0.0 ))

(defn process
  ^SVF [^SVF filt ^SVFParams params ^double in]
  (let [hp (* (.h params)
              (- in
                 (* (.r params) (.state1 filt))
                 (* (.g params) (.state1 filt))
                 (.state2 filt)))
        bp (+ (* (.g params) hp)
              (.state1 filt))
        lp (+ (* (.g params) bp)
              (.state2 filt))]
    (SVF. (+ (* (.g params) hp) bp)
          (+ (* (.g params) bp) lp)
          lp bp (* (.r params) bp) hp)))

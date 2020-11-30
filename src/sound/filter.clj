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

(deftype SVFResult [^double lp ^double bp ^double bpn ^double hp])

(deftype SVF [^double g ^double r ^double h ^double state1 ^double state2
              ^SVFResult result])

(defn set-gq
  ^SVF [^SVF filt ^double g ^double q]
  (let [r (/ q)]
    (SVF. g r (/ (+ 1.0 (* r g) (* g g))) (.state1 filt) (.state2 filt) nil)))

(defmacro set-fq
  ([filt type f q]
   `(set-gq ~filt (TAN ~type ~f) ~q))
  ([filt f q]
   `(set-fq ~filt :dirty ~f ~q))
  ([filt f]
   `(set-fq ~filt ~f 100.0)))

(defn reset
  ^SVF [^SVF filt]
  (SVF. (.g filt) (.r filt) (.h filt) 0.0 0.0 nil))

(defn init
  ^SVF []
  (set-fq (SVF. 0.0 0.0 0.0 0.0 0.0 nil) 0.01))

(defn process
  ^SVF [^SVF filt ^double in]
  (let [hp (* (.h filt)
              (- in
                 (* (.r filt) (.state1 filt))
                 (* (.g filt) (.state1 filt))
                 (.state2 filt)))
        bp (+ (* (.g filt) hp)
              (.state1 filt))
        lp (+ (* (.g filt) bp)
              (.state2 filt))]
    (SVF. (.g filt) (.r filt) (.h filt)
          (+ (* (.g filt) hp) bp)
          (+ (* (.g filt) bp) lp)
          (SVFResult. lp bp (* (.r filt) bp) hp))))

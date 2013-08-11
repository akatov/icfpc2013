(ns enum
  [:require [functions :as f]])

(def arity
  {'not 1
   'shl1 1
   'shr1 1
   'shr4 1
   'shr16 1
   'or 2
   'and 2
   'xor 2
   'plus 2
   'if0 3})

(defn progs
  "returns seq of possible programs of size `size` using operators `ops`."
  ([size ops rops]
     (if (= size 1)
       (if (empty? rops) [0 1 'x] [])
       (concat
        (for [o ops :when (= 1 (arity o)) 
              p (progs (- size 1) ops (disj rops o))] 
          (list o p))
        (for [o  ops :when (= 2 (arity o))
              s  (range 1 (/ size 2))
              p1 (progs s ops  (disj rops o))
              p2 (progs (- size (+ 1 s)) ops  (disj rops o))] 
          (list o p1 p2)))))
  ([size ops]
     (progs size ops ops)))

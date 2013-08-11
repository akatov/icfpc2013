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

(defn symbols [sexp]
  (cond
   (symbol? sexp) #{sexp}
   (list? sexp) (apply clojure.set/union (map symbols sexp))))

(defn progsAux
  "returns seq of possible programs of size `size` using operators `ops`."
  [size ops]
  ;; (println "size: " size ", ops: " ops)
  (if (= size 1)
    [0 1 'x]
    (concat
     (for [o ops :when (= 1 (arity o))
           p (progsAux (- size 1) ops)]
       (list o p))
     (for [o  ops :when (= 2 (arity o))
           s  (range 1 (Math/floor (/ size 2)))
           p1 (progsAux s ops)
           p2 (progsAux (- size 1 s) ops)]
       (list o p1 p2))
     (for [o ops :when (= 2 (arity o))
           :when (odd? size)
           :let [s (/ (- size 1) 2)]
           p1 (progsAux s ops)
           p2 (progsAux s ops)
           :when (>= (compare ( f/to-string p1) (f/to-string p2)) 0)]
       (list o p1 p2)))))

(defn progs [size ops]
  (->> (progsAux size ops)
       (filter #(clojure.set/subset? ops (symbols %)))))

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

(defn minSize [ops] 
  "minimal size of a program having the set of operators `ops`"
  (reduce + 1 (map arity ops)))

(defn guessCountsAux [size ops upb]
  "guess the number of occurence of each operators knowing size and set of operators
   when upb is not specified, it is assumed that each operator must occur at least once
   when given, upb is a mapping providing an upper bound on the number of each operator" 
  (let [o (first ops) 
        a (arity o) 
        u (if (upb o) (upb o) size) 
        tail (disj ops (first ops))] 
   (if (empty? ops) (seq (list {})) 
    (for [ts (range (minSize tail) (if (empty? tail) 2 (+ 1 (- size (if (upb o) 0 a)))))
          :let [os (- size ts)]
          :let [cnt (int (Math/floor (/ os a)))]
          :when (= os (* cnt a))
          :when (<= cnt u)
          tres (guessCountsAux ts tail upb)]
    (if (= 0 cnt) tres (merge tres {o cnt}))))))

(defn guessCounts [size ops] 
   "guess the number of occurence of each opperator, given a set ops" 
   (guessCountsAux size ops {}))

(defn guessSubCounts [size opsmap] 
   "guess the possible counts of each operator in a sub expression" 
   (guessCountsAux size (set (keys opsmap)) opsmap))

(defn decr [k n m] 
  "decrement the value of key k by n in map m and delete the key if new value is 0"
  (if (= n (m k)) (dissoc m k) (merge m {k (- (m k) n)})))

(defn decrMap [m1 m2] 
  "apply decr [key value] to m2 for each key-value pair in m1 "
  (reduce #(decr %2 (m1 %2) %1) m2 (keys m1))
)


(defn progsAuxCnt
  "possible programs of given size knowning the number of occurence of each operator"
  [size opsmap]
  ;; (println "size: " size ", opsmap: " opsmap)
  (if (= size 1)
    [0 1 'x]
    (concat
     (for [o  (set (keys opsmap)) :when (= 1 (arity o))
           p (progsAuxCnt (- size 1) (decr o 1 opsmap))]
       (list o p))
     (for [o  (set (keys opsmap)) :when (= 2 (arity o))
           s  (range 1 (Math/floor (/ size 2)))
           m  (guessSubCounts s (decr o 1 opsmap))
           p1 (progsAuxCnt s m)
           p2 (progsAuxCnt (- size 1 s) (decrMap m (decr o 1 opsmap)))]
       (list o p1 p2))
     (for [o  (set (keys opsmap)) :when (= 2 (arity o))
           :when (odd? size)
           :let [s (/ (- size 1) 2)]
           m  (guessSubCounts s (decr o 1 opsmap))
           p1 (progsAuxCnt s m)
           p2 (progsAuxCnt s (decrMap m (decr o 1 opsmap)))
           :when (>= (compare ( f/to-string p1) (f/to-string p2)) 0)]
       (list o p1 p2)))))

(defn progsAux2 [size ops]
 "Same behaviour as progAux"
 (reduce concat #{} (map #(progsAuxCnt size %) (guessCounts size ops)))  )



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

(defn progs
  ([size ops]
     (progs size ops [] []))
  ([size ops inputs outputs]
     (->> (progsAux size ops)
          (filter #(clojure.set/subset? ops (symbols %)))
          (map #(list 'lambda (list 'x) %))
          (filter #(= outputs (f/eval % inputs))))))


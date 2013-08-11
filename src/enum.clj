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

(defn guessCounts [size ops]
  "guess the number of occurence of each operators knowing size and set of operators" 
  (let [o (first ops) 
        a (arity o) 
        tail (disj ops (first ops))] 
   (if (empty? ops) (seq (list {})) 
    (for [ts (range (minSize tail) (if (empty? tail) 2 (+ 1 (- size a))))
          :let [os (- size ts)]
          :let [cnt (int (Math/floor (/ os a)))]
          :when (= os (* cnt a))
          tres (guessCounts ts tail)]
    (merge tres {o cnt})))))

(defn decr [k m] 
  "decrement the value of key k in map m and delete the key if new value is 0"
  (if (= 1 (m k)) (dissoc m k) (merge m {k (- (m k) 1)})))

;; (defn progsAuxCnt
;;  "possible programs of given knowning the number of occurence of each operator"
;;  [size opsmap]
;;  ;; (println "size: " size ", opsmap: " opsmap)
;;  (if (= size 1)
;;    [0 1 'x]
;;    (concat
;;     (for [o (key opsmap) :when (= 1 (arity o))
;;           p (progsAux (- size 1) (decr o opsmap))]
;;       (list o p))
;;     (for [o  (key opsmap) :when (= 2 (arity o))
;;           s  (range 1 (Math/floor (/ size 2)))
;;           p1 (progsAux s (decr o opsmap)
;;           p2 (progsAux (- size 1 s) ops)]
;;       (list o p1 p2))
;;     (for [o ops :when (= 2 (arity o))
;;           :when (odd? size)
;;           :let [s (/ (- size 1) 2)]
;;           p1 (progsAux s ops)
;;           p2 (progsAux s ops)
;;           :when (>= (compare ( f/to-string p1) (f/to-string p2)) 0)]
;;       (list o p1 p2)))))


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


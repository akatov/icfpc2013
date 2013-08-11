
(ns enum)

(def arrity {:not 1 :shl1 1 :shr1 1 :shr4 1 :shr16 1 :or 2 :and 2 :xor 2 :plus 2 :if0 3})

(defn progsAux [size ops rops]  
   (if (= size 1)
     (if (empty? rops) (seq [:1 :0 :x]) (seq []))
     (concat 
       (for [o ops :when (= 1 (arrity o)) 
             p (progsAux (- size 1) ops (disj rops o))] 
             (list o p)        
       )
       (for [o  ops :when (= 2 (arrity o))
             s  (range 1 (/ size 1))
             p1 (progsAux s ops  (disj rops o))
             p2 (progsAux (- size (+ 1 s)) ops  (disj rops o))] 
            (list o p1 p2)
       )
     ) 
   )
)

(defn progs [size ops] (progsAux size ops ops)) 

(progs 5 #{:and :or})

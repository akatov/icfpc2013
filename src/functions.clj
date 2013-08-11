(ns functions
  (:refer-clojure :exclude [or and not]))

(def maxInteger "max 64-bit integer"
  (new BigInteger "ffffffffffffffff" 16))

(defmacro lambda [args & body]
  `(fn [~@args] ~@body))

(defn if0 [e1 e2 e3]
  (if (== e1 0) e2 e3))

(defn and [x y]
  (.and x y))

(defn not [x]
  (.and maxInteger (.not x)))

(defn or [x y]
  (.and maxInteger (.or x y)))

(defn xor [x y]
  (.and maxInteger (.xor x y)))

(defn shl1 [x]
  (.and maxInteger (.shiftLeft x 1)))

(defn shr1 [x]
  (.and maxInteger (.shiftRight x 1)))

(defn shr4 [x]
  (.and maxInteger (.shiftRight x 4)))

(defn shr16 [x]
  (.and maxInteger (.shiftRight x 16)))

(defn plus [x y]
  (.and maxInteger (+ x y)))

(defn eval
  "fun is a quoted function. args is a vector of arguments.
exapmle: (eval '(lambda (x) (not x)) (map from-long [0 1 2]))"
  [fun args]
  (binding [*ns* *ns*]
    (in-ns 'functions)
    (map (clojure.core/eval fun) args)))

(defn to-string [f]
  (cond
   (= f 0) "0"
   (= f 1) "1"
   (symbol? f) (name f)
   (list? f) (str "(" (clojure.string/join " " (map to-string f)) ")")
   :otherwise "unknown"))

(defn read-string [str]
  (clojure.core/read-string str))

(defn to-hex [n]
  (format "0x%016X" n))

(defn to-num [hex]
  (new BigInteger (subs hex 2) 16))

(defn from-long [n]
  (BigInteger/valueOf n))

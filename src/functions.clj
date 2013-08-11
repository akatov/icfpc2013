(ns functions
  (:refer-clojure :exclude [or and not eval read-string]))

(defn from-long [n]
  (BigInteger/valueOf n))

(defn to-num
  "converts strings and longs to BigIntegers"
  [x]
  (let [t (type x)]
    (cond
     (= t String)
     (if (= "0X" (.toUpperCase (subs x 0 2)))
       (new BigInteger (subs x 2) 16)
       (new BigInteger x 16))
     (= t Long) (BigInteger/valueOf x)
     (= t BigInteger) x
     :otherwise (throw (Exception. "unknown type")))))

(def maxInteger "max 64-bit integer"
  (new BigInteger "ffffffffffffffff" 16))

(defn num-to-list
  "converts a number to a list of numbers for use in `fold`.
example:
  (map to-hex (num-to-list (to-num \"0x1122334455667788\"))) ;;=>
    (0x88, 0x77, 0x66, 0x55, 0x44, 0x33, 0x22, 0x11)"
  [n]
  (map #(.and (to-num 0xff) (.shiftRight n (* 8 %)))
       (range 0 8)))

(defmacro lambda [args & body]
  `(fn [~@args] ~@body))

(defmacro fold [x y & fun] ;; y is accumulator
  `(reduce ~@fun ~y (num-to-list ~x)))

(defn- new-bindings [args]
  (vec (mapcat (fn [args-name]
                 [args-name (list 'to-num args-name)])
               args)))

(defmacro def-bi-fn
  "Defines a function with each argument first being cooerced to a
  BigInteger"
  [fn-name args & body]
  `(defn ~fn-name ~args
     (let ~(new-bindings args)
       ~@body)))

(defn if0 [e1 e2 e3]
  (if (== e1 0) e2 e3))

(def-bi-fn and [x y]
  (.and x y))

(def-bi-fn not [x]
  (.and maxInteger (.not x)))

(def-bi-fn or [x y]
  (.and maxInteger (.or x y)))

(def-bi-fn xor [x y]
  (.and maxInteger (.xor x y)))

(def-bi-fn shl1 [x]
  (.and maxInteger (.shiftLeft x 1)))

(def-bi-fn shr1 [x]
  (.and maxInteger (.shiftRight x 1)))

(def-bi-fn shr4 [x]
  (.and maxInteger (.shiftRight x 4)))

(def-bi-fn shr16 [x]
  (.and maxInteger (.shiftRight x 16)))

(def-bi-fn plus [x y]
  (.and maxInteger (+ x y)))

(defn eval
  "fun is a quoted function. args is a vector of arguments.
exapmle: (eval '(lambda (x) (not x)) (map to-num [0 1 2]))"
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

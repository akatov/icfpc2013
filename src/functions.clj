(ns functions
  (:refer-clojure :exclude [or and not]))

(def maxInteger 18446744073709551616N)

(defmacro lambda [args & body]
  `(fn [~@args] ~@body))

(defn if0 [e1 e2 e3]
  (if (== e1 0) e2 e3))

(defn not [x]
  (-> x bit-not (mod maxInteger)))

(defn shl1 [x]
  (-> x (bit-shift-left 1) (mod maxInteger)))

(defn shr1 [x]
  (-> x (bit-shift-right 1) (mod maxInteger)))

(defn shr4 [x]
  (-> x (bit-shift-right 4) (mod maxInteger)))

(defn shr16 [x]
  (-> x (bit-shift-right 16) (mod maxInteger)))

(defn and [x y]
  (mod (bit-and x y) maxInteger))

(defn or [x y]
  (mod (bit-or x y) maxInteger))

(defn xor [x y]
  (mod (bit-xor x y) maxInteger))

(defn plus [x y]
  (mod (+ x y) maxInteger))

(defn eval
  "fun is a quoted function. args is a vector of arguments"
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

(defn to-hex [n]
  (format "0x%016X" n))

(defn to-num [hex]
  (new BigInteger (subs hex 2) 16))

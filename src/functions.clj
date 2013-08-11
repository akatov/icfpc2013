(ns functions
  (:refer-clojure :exclude [or and not]))

(defmacro lambda [args & body]
  `(fn [~@args] ~@body))

(defn if0 [e1 e2 e3]
  (if (== e1 0) e2 e3))

(defn not [x]
  (bit-not x))

(defn shl1 [x]
  (bit-shift-left x 1))

(defn shr1 [x]
  (bit-shift-right x 1))

(defn shr4 [x]
  (bit-shift-right x 4))

(defn shr16 [x]
  (bit-shift-right x 16))

(defn and [x y]
  (bit-and x y))

(defn or [x y]
  (bit-or x y))

(defn xor [x y]
  (bit-xor x y))

(defn plus [x y]
  (+ x y))

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

(ns functions)

(defmacro lambda [args & body]
  `(fn [~(first args)] ~(first body)))

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

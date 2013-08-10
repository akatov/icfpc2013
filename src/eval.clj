(ns eval
  [:require [clojure.string :as string]])

(defn replace-several [content & replacements]
  (let [replacement-list (partition 2 replacements)]
    (reduce #(apply string/replace %1 %2) content replacement-list)))

(defn shl1 [x] (bit-shift-left x 1))
(defn shr1 [x] (bit-shift-right x 1))
(defn shr4 [x] (bit-shift-right x 4))
(defn shr16 [x] (bit-shift-right x 16))

(defn evaluate [func value]
  ((eval
    (read-string
      (replace-several func #"lambda" "fn"
                            #"\(x_(\d*)\)"  "[x_$1]"
                            #"and" "bit-and"
                            #"or" "bit-or"
                            #"not" "bit-not"
                            #"xor" "bit-xor")))
   value))

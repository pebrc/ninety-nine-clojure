(ns ninety-nine-clojure.logic
  (:require [clojure.pprint :only print-table]))

(defn and [x y]
  (if x (if y y false) false))

(defn or [x y]
  (if x x y))

(defn nand [x y]
  (not (and x y)))

(defn nor [x y]
  (not (or x y)))

(defn equ [x y]
  (or (and x y)
      (and (not x) (not y))))

(defn xor [x y]
  (not (= x y)))

(defn impl [x y]
  (or (not x)
      (and x y)))

(def inputs [[true true] [true false] [false true] [false false]])

(defn table [expr]
  (let [header  [:a :b :result]
        rows    (->> inputs
                     (map #(interleave header (flatten (vector % (apply expr %)))))
                     (map #(apply sorted-map %))
                     )]
    (print-table rows)))

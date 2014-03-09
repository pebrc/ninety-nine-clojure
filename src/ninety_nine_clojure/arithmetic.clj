(ns ninety-nine-clojure.arithmetic
  (:require [clojure.math.numeric-tower :as math]))

(defn prime? [x]
  "P31 (**) Determine whether a given integer number is prime."
  (empty?  (filter #(= 0 (rem x %)) (range 2 (inc (int (math/sqrt x)))))))

(defn gcd [x y]
  "P32 (**) Determine the greatest common divisor of two positive
  integer numbers."
  (if (= y 0)
    x
    (gcd y (rem x y))))

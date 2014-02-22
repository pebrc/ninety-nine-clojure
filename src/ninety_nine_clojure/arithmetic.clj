(ns ninety-nine-clojure.arithmetic
  (:require [clojure.math.numeric-tower :as math]))

(defn prime? [x]
  "P31 (**) Determine whether a given integer number is prime."
  (empty?  (filter #(= 0 (rem x %)) (range 2 (inc (int (math/sqrt x)))))))

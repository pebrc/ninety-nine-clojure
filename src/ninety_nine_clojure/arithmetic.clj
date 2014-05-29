(ns ninety-nine-clojure.arithmetic
  (:require [clojure.math.numeric-tower :as math]))


(defn prime? [x]
  "P31 (**) Determine whether a given integer number is prime."
  (empty?  (filter #(= 0 (rem x %)) (range 2 (inc (int (math/sqrt x)))))))

(defn square [x] (* x x))

(defn expmod [base exp m]
  (cond (= exp 0) 1
        (even? exp) (rem (square (expmod base (/ exp 2) m )) m)
        :else (rem (* base (expmod base (- exp 1) m )) m )))

(defn fermat-test [n]
  (letfn [(try-it [r]
            (= (expmod r n n) r))]
    (try-it (inc (rand-int (dec n))))))

(defn fast-prime? [x]
  "P31 (**) Determine whether a given integer number is prime. This
  uses the fermat test from SICP (which fails for Carmichael numbers).
  Also: the number ofo 100 tests is arbitrary)"
  (empty?  (filter not (repeatedly 100  #(fermat-test x)))))


(defn gcd [x y]
  "P32 (**) Determine the greatest common divisor of two positive
  integer numbers."
  (if (= y 0)
    x
    (recur y (rem x y))))


(defn coprime? [x y]
  "P33 (*) Determine whether two positive integer numbers are coprime."
  (= 1 (gcd x y)))

(defn totient-euler [x]
  "P34 (**) Calculate Euler's totient function phi(m)."
  (->> (range 1 (inc x))
       (map #(/ % x))
       (filter #(and (ratio? %) (= x (denominator %))))
       (count)))

(defn totient [x]
  (->> (range 1 (inc x))
       (filter #(coprime? % x))
       (count)))

(defn primes []
  "Calculates a lazy seq of primes "
  (->> (range )
       (map inc)
       (filter #(fast-prime? %))))

(defn prime-factors 
  ([x] (prime-factors x (primes) []))
  ([x primes acc]
     (cond (>= 1 x) acc
           (= 0 (rem x (first primes))) (recur (quot x (first primes)) primes  (conj acc (first primes)))
           :else (recur  x (rest primes) acc)
           )) )

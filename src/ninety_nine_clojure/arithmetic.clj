(ns ninety-nine-clojure.arithmetic
  (:require [clojure.math.numeric-tower :as math]
            [ninety-nine-clojure.lists :as list]))


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
  "P34 (**) Calculate Euler's totient function phi(m). Euler's
  so-called totient function phi(m) is defined as the number of
  positive integers r (1 <= r <= m) that are coprime to m."
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
  "P35 (**) Determine the prime factors of a given positive integer."
  ([x] (prime-factors x (primes) []))
  ([x primes acc]
     (cond (>= 1 x) acc
           (= 0 (rem x (first primes))) (recur (quot x (first primes)) primes  (conj acc (first primes)))
           :else (recur  x (rest primes) acc)
           )) )

(defn prime-factors-multiplicity
  "P36 (**) Determine the prime factors of a given positive integer
  (2). Construct a list containing the prime factors and their
  multiplicity. Alternately, use a map for the result."
  [x]
  (apply assoc {} (mapcat reverse (list/encode (prime-factors x)))))

(defn fast-totient
  "P37 (**) Calculate Euler's totient function phi(m) (improved).
See problem P34 for the definition of Euler's totient function. If the
list of the prime factors of a number m is known in the form of
problem P36 then the function phi(m>) can be efficiently calculated as
follows: Let [[p1, m1], [p2, m2], [p3, m3], ...] be the list of prime
factors (and their multiplicities) of a given number m. Then phi(m)
can be calculated with the following formula: phi(m) = (p1-1)*p1(m1-1)
* (p2-1)*p2(m2-1) * (p3-1)*p3(m3-1) * ... Note that ab stands for the
bth power of a."
  [x]
  (->> (prime-factors-multiplicity x)
       (reduce #(* %1 (dec (key %2)) (math/expt (key %2) (dec (val %2)))) 1 )))

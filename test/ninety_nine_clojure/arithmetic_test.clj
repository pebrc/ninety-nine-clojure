(ns ninety-nine-clojure.arithmetic-test
  (:require [clojure.test :refer :all]
            [ninety-nine-clojure.arithmetic :refer :all]))

(deftest p31-test-primes
  (is (not (prime? 4)))
  (is (prime? 7)))

(deftest p32-euclids-algorithm
  (is (= 9 (gcd 36 63)))
  (is (= 21 (gcd 462 1071)))
  (is (= 1  (gcd 1 1))))


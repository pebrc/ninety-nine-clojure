(ns ninety-nine-clojure.arithmetic-test
  (:require [clojure.test :refer :all]
            [ninety-nine-clojure.arithmetic :refer :all]))

(deftest p31-test-primes
  (is (not (prime? 4)))
  (is (prime? 7))
  (is (fast-prime? 7))
  (is (not (fast-prime? 4))))

(deftest p31-fermat-fails-on-carmichael
  "561 is a Carmichael number and a incorrectly identified as prime by
  tests based on Fermat's Little Theorem"
  (is (fast-prime? 561)))

(deftest p32-euclids-algorithm
  (is (= 9 (gcd 36 63)))
  (is (= 21 (gcd 462 1071)))
  (is (= 1  (gcd 1 1))))


(ns ninety-nine-clojure.arithmetic-test
  (:require [clojure.test :refer :all]
            [ninety-nine-clojure.arithmetic :refer :all]))

(deftest p31-test-primes
  (is (not (prime? 4)))
  (is (prime? 7)))

(ns ninety-nine-clojure.arithmetic-test
  (:require [clojure.test :refer :all]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer :all]
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


(deftest p33-coprime-integers
  (is (coprime? 14 15))
  (is (not (coprime? 14 21))))

(deftest p34-totient
  (is (= 8 (totient-euler 20)))
  (is (= 4 (totient-euler 5))))

(deftest p35-prime-factors
  (is (= '() (prime-factors 1 )))
  (is (= '(2) (prime-factors 2 )))
  (is (= '(3) (prime-factors 3 )))
  (is (= '(2 2) (prime-factors 4 )))
  (is (= '(5) (prime-factors 5 )))
  (is (= '(2 3) (prime-factors 6 )))
  (is (= '(3 3 5 7) (prime-factors 315))))

(deftest p36-prime-factors-multiplicity
  (is (= {3 2, 5 1, 7 1} (prime-factors-multiplicity 315) )))

(deftest p37-totient-improved
  (is (= 144 (fast-totient 315))))

(deftest p39-primes-range
  (is (= '(7 11 13 17 19 23 29 31) (primes-range 7 31))))

(deftest p40-goldbach
  (is (= [5 23] (goldbach 28))))

(def even-nats-greater-two (gen/such-that #(< 2 %) (gen/fmap #(* 2 %) gen/nat)))


(defspec goldbach-conjecture
  100
  (prop/for-all [i even-nats-greater-two]
                (let [res (goldbach i)]
                  (and
                   (= 2 (count res))
                   (every? prime? res)
                   (= i (reduce + res))))))

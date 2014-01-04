(ns ninety-nine-clojure.lists-test
  (:require [clojure.test :refer :all]
            [ninety-nine-clojure.lists :refer :all]))

(deftest p01-builtin-test
  (let [in (list 1 2 3)]
    (is (= 3 (last-builtin in)))))

(deftest p01-test
  (let [in (list 1 2 3)]
    (is (= 3 (my-last in)))))

(deftest p02-test
  (let [in (list 1 2 3)]
    (is (= 2 (my-butlast in)))))

(deftest p02-empty-input
  (let [in ()]
    (is (= nil (my-butlast in)))))

(deftest p02-non-seq
  (is (thrown? AssertionError (my-butlast :not-a-seq))))

(deftest p03-builtin-test
  (let [in (list 1 2 3 4)]
    (is (= 3 (kth-builtin 2 in)))))

(deftest p03-test
  (let [data (list 1 2 3 4)]
    (is (= 3 (kth 2 data)))))

(deftest p03-empty-input
  (is (= nil (kth 2 '()))))

(deftest p03-index-out-of-bounds
  (is (= nil (kth 10 (list 1)))))

(deftest p04-builtin-test
  (is (= 3 (count-builtin (list 1 2 3)))))

(deftest p04-test
  (is (= 3 (my-count (list 1 2 3)))))

(deftest p04-test-2
  (is (= 3 (my-count-reduce (list 1 2 3)))))

(deftest p05-test
  (is (= '(3 2 1) (my-reverse '(1 2 3)))))

(deftest p05-test-2
  (is (= '(3 2 1) (my-reverse-reduce '(1 2 3)))))

(deftest p06-test
  (is (= true (palindrome? '(1 2 3 3 2 1))))
  (is (= false (palindrome? '(1 2 3)))))

(deftest p07-test
  (is (= '(1 1 2 3 5 8) (flatten-recur (list (list 1 1) 2 (list 3 (list 5 8)))))))

(deftest p07-flat-list-is-flat
  (is (= '(1) (flatten-recur '(1)))))

(deftest p07-one-level-flatten
  (is (= '(1 2) (flatten-recur (list 1 (list 2))))))

(deftest p07-destructured-flatten-with-one-elem
  (is (= '(1) (flatten-destructured '(1)) )))

(deftest p07-one-level-flatten-destructured
  (is (= '(1 2) (flatten-destructured (list 1 (list 2))))))

(deftest p07-test-destructured
  (is (= '(1 1 2 3 5 8) (flatten-destructured (list (list 1 1) 2 (list 3 (list 5 8)))))))
(deftest p07-reduce-flatten-with-one-elem
  (is (= '(1) (flatten-reduce '(1)) )))

(deftest p07-one-level-flatten-reduce
  (is (= '(1 2) (flatten-reduce (list 1 (list 2))))))

(deftest p07-test-reduce
  (is (= '(1 1 2 3 5 8) (flatten-reduce (list (list 1 1) 2 (list 3 (list 5 8)))))))

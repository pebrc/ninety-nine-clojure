(ns ninety-nine-clojure.bintrees-test
  (:require [clojure.test :refer :all]
            [ninety-nine-clojure.bintrees :refer :all]))

(deftest p54-nil-is-a-tree
  (is (= true (tree? nil))))

(deftest p54-root-only-tree
  (is (= true (tree? [:root nil nil]))))

(deftest p54-nested-two-levels
  (is (= true (tree? [:a [:b nil nil] nil]))))

(deftest p54-missing-branch
  (is (= false (tree? [:a [:b nil nil]]))))

(deftest p54-invalid-successor
  (is (= false (tree? [:a [:b] nil]))))

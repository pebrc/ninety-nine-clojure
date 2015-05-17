(ns ninety-nine-clojure.bintrees-test
  (:require [clojure.test :refer :all]
            [ninety-nine-clojure.bintrees :refer :all]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer :all]))

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

(defspec p55-balanced-trees-are-balanced
  20
  (prop/for-all [i gen/nat]
                (->> (balanced-trees i 'x)
                     (apply  breadth-first-traverse)
                     (map balanced?)
                     (reduce #(and %1 %2)))))

(deftest p56-detects-symmetric-trees
  (is (= true (symmetric? [:a [:b nil nil] [:c nil nil]]))))

(deftest p56-fails-for-asymmetric-trees
  (is (= false (symmetric? [:a nil [:b nil nil]]))))

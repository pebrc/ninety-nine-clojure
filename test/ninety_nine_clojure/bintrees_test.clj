(ns ninety-nine-clojure.bintrees-test
  (:require [clojure.test :refer :all]
            [ninety-nine-clojure.bintrees :refer :all]
            [ninety-nine-clojure.logictrees :as l]
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
  (is (symmetric? [:a [:b nil nil] [:c nil nil]])))

(deftest p56-fails-for-asymmetric-trees
  (is (not (symmetric? [:a nil [:b nil nil]]))))

(deftest p56-empty-tree-is-symmetric
  (is (symmetric? nil)))

(deftest p57-construct-bst
  (is (= [3 [2 [1 nil nil] nil] [5 nil [7 nil nil]]] (->binary-search-tree 3 2 5 7 1))))

(deftest p57-test-p56
  (is (symmetric? (->binary-search-tree 5 3 18 1 4 12 21)))
  (is (not (symmetric? (->binary-search-tree 3 2 5 7 4)))))

(defspec p58-symmetric-and-completely-balanced
  20
  (prop/for-all [i (gen/such-that #(and (< % 100) (< 0 %)) gen/nat)]
                (->> (symmetric-cbalanced-trees i 'x)
                     (map #(and (balanced? %) (symmetric? %)))
                     (reduce #(and %1 %2) true))))



(defspec p59-height-balanced-trees-are-height-balanced
  3
  (prop/for-all [i (gen/choose 1 5) ]
                (->> (height-balanced-trees i 'x)
                     (mapcat depth-first)
                     (map height-balanced?)
                     (reduce #(and %1 %2)))))

(defspec p59-core-logic-height-balanced-trees-are-height-balanced
  3
  (prop/for-all [i (gen/choose 1 5) ]
                (->> (l/height-balanced-trees i)
                     (mapcat depth-first)
                     (map height-balanced?)
                     (reduce #(and %1 %2)))))

(defspec p59-logic-vs-non-rel-produce-same-results
  1
  (prop/for-all [i (gen/choose 1 5)]
                (is (= (set (l/height-balanced-trees i)) (set (height-balanced-trees i 'x))))))

(defspec p60-minimal-height-balanced-tree
  3
  (prop/for-all [i (gen/choose 1 5)]
                (= (min-hbal-nodes i)
                   (->> (height-balanced-trees i 'x)
                              (filter #(= i (height %)))
                              (map num-nodes)
                              (apply min)))))

(defspec p60-logic-vs-non-relational
  1
  (prop/for-all [i (gen/choose 1 9)]
                (is (= (set (all-hbal-trees i 'x)) (set (l/all-hbal-trees i))))))


(deftest p61-count-leaves
  (is  (= (leaf-count '[x [x [x nil nil] nil] [x nil nil]]) 2)))


(deftest p61a-leaves
  (is (= '(b d e) (leaves '[a [b nil nil] [c [d nil nil] [e nil nil]]]))))

(deftest p62-internals
  (is (= '(a c) (internals '[a [b nil nil] [c [d nil nil] [e nil nil]]]))))

(deftest p62b-at-level
  (is (= '(b c) (at-level '[a [b nil nil] [c [d nil nil] [e nil nil]]] 2))))

(deftest p63-complete-binary-tree-predicate
  (is (complete-tree? '[x [x nil nil] nil]))
  (is (not (complete-tree? '[x nil [x nil nil]]))))

(defspec p63-complete-binary-tree
  3
  (prop/for-all [i (gen/choose 1 6)]
                (is (complete-tree? (complete-binary-tree i 'x)))))


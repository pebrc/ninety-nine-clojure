(ns ninety-nine-clojure.bintrees
  (:require [clojure.core.match :refer [match]]
            [clojure.math.numeric-tower :refer [abs]]))

(defn tree? [t]
  (match t
         [label (true :<< tree?) (true :<< tree?)] true
         nil                                       true
         :else                                     false))

(defn lefts [t]
  (first (next t)))

(defn rights [t]
  (last t))

(defn num-nodes [tree]
  (if (nil? tree)
    0
    (+ 1 (num-nodes (lefts tree)) (num-nodes (rights tree)))))

(defn balanced? [t]
  (>= 1 (abs (-
              (num-nodes (lefts t))
              (num-nodes (rights t))))))


(defn breadth-first-traverse [& trees]
  (when trees
    (concat trees 
      (->> trees
        (mapcat #(vector (first (next %)) (last %)))
        (filter #(not (nil? %) ))
        (apply breadth-first-traverse)))))


(defn balanced-trees
  "P55 (**) Construct completely balanced binary trees In a completely
  balanced binary tree, the following property holds for every node:
  The number of nodes in its left subtree and the number of nodes in
  its right subtree are almost equal, which means their difference is
  not greater than one.

  Write a predicate cbal_tree/2 to construct completely balanced binary
  trees for a given number of nodes. The predicate should generate all
  solutions via backtracking. Put the letter 'x' as information into
  all nodes of the tree. 

  Example: ?- cbal_tree(4,T). T = t(x, t(x,
  nil, nil), t(x, nil, t(x, nil, nil))) ; T = t(x, t(x, nil, nil),
  t(x, t(x, nil, nil), nil)) ; etc......No"
  [n v]
  (let [n0 (dec n)
        n1 (int (/ n0 2))
        n2  (- n0 n1)
        distrib (fn
                  ([subtrees]
                   (mapcat (fn [l] (map (fn [r] (vector v l r)) subtrees))subtrees) )
                  ([lefts rights]
                   (mapcat (fn [l] (mapcat (fn [r] [(vector v l r) (vector v r l)]) rights))lefts)))]
    (cond
      (< n 1)  [nil] 
      (= n1 n2) (let [subtrees (balanced-trees n1 v)]
                  (distrib subtrees))
      :odd  (let [lower (balanced-trees n1  v)
                  higher (balanced-trees n2 v)]
              (distrib lower higher )) )) )
    

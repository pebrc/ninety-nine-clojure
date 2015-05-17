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
                   (mapcat (fn [l] (map (fn [r] (vector v l r)) subtrees)) subtrees) )
                  ([lefts rights]
                   (mapcat (fn [l] (mapcat (fn [r] [(vector v l r) (vector v r l)]) rights)) lefts)))]
    (cond
      (< n 1)  [nil] 
      (= n1 n2) (distrib (balanced-trees n1 v))
      :odd  (let [lower (balanced-trees n1  v)
                  higher (balanced-trees n2 v)]
              (distrib lower higher )) )) )

(defn mirror? [l r]
  (match [l r]
    [nil nil] true
    [[_, l1, r1] [_, l2, r2]] (and (mirror? l1 r2) (mirror? l2 r1)) 
    :else false))

(defn symmetric?
  "P56 (**) Symmetric binary trees 

  Let us call a binary tree symmetric if you can draw a vertical line
  through the root node and then the right subtree is the mirror image
  of the left subtree.

  Write a predicate symmetric/1 to check whether a given binary tree is
  symmetric. Hint: Write a predicate mirror/2 first to check whether
  one tree is the mirror image of another. We are only interested in
  the structure, not in the contents of the nodes."
  [t]
  {:pre [(tree? t)]}
  (mirror? (lefts t) (rights t)))
    

(ns ninety-nine-clojure.bintrees
  (:require [clojure.core.match :refer [match]]
            [clojure.math.numeric-tower :refer [abs]]))

(defn tree? [t]
  (match t
         [label (true :<< tree?) (true :<< tree?)] true
         nil                                       true
         :else                                     false))

(defn num-nodes [[_ l r]]
  (if (= nil l r)
    0
    (+ 1 (num-nodes l) (num-nodes r))))

(defn balanced? [[_ l r]]
  (>= 1 (abs (-
              (num-nodes l)
              (num-nodes r)))))


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

  Write a function symmetric/1 to check whether a given binary tree is
  symmetric. Hint: Write a function mirror/2 first to check whether
  one tree is the mirror image of another. We are only interested in
  the structure, not in the contents of the nodes."
  [[_ l r]]  
  (mirror? l r))

(defn insert-val [tree v]
  (match [tree v]
    [t nil]  t     
    [nil val]  [val nil nil]
    [[(rv :guard (partial < v)) l r] _] [rv (insert-val l v) r]
    [[rv l r] _] [rv l (insert-val r v)]))

(defn ->binary-search-tree
  "Write a function to construct a binary search tree from a list of integer numbers."
  [& xs]
  (reduce insert-val nil xs ))
    

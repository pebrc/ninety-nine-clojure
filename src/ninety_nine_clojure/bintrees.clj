(ns ninety-nine-clojure.bintrees
  (:require [clojure.core.match :refer [match]]
            [clojure.math.numeric-tower :refer [abs]]))

(defn tree? [t]
  (match t
         [label (true :<< tree?) (true :<< tree?)] true
         nil                                       true
         :else                                     false))

(defn leaf? [[_ l r]]
  (= nil l r))

(def branch? 
  (complement leaf?))

(defn num-nodes [[v l r]]
  (cond
    (nil? v) 0  
    (and (nil? l) (nil? r)) 1
    :else (+ 1 (num-nodes l) (num-nodes r))))

(defn height [[_ l r]]
  (if (= nil l r)
    1
    (inc (max (height l) (height r)))))

(defn balanced? [[_ l r]]
  (>= 1 (abs (-
              (num-nodes l)
              (num-nodes r)))))

(defn height-balanced? [[_ l r]]
  (>= 1 (abs (-
              (height l)
              (height r)))))


(defn breadth-first-traverse [& trees]
  (when trees
    (concat trees 
      (->> trees
        (mapcat #(vector (first (next %)) (last %)))
        (filter #(not (nil? %) ))
        (apply breadth-first-traverse)))))


(defn depth-first
  [& trees]
  (mapcat #(tree-seq branch? next %) trees))


(defn distrib
  "Creates new trees based on the given subtrees. When given two sets
  ot subtrees it returnes trees of all combinations of the given
  subtrees."
  ([v subtrees]
   (mapcat (fn [l] (map (fn [r] (vector v l r)) subtrees)) subtrees) )
  ([v lefts rights]
   (mapcat (fn [l] (mapcat (fn [r] [(vector v l r) (vector v r l)]) rights)) lefts)))


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
        n2  (- n0 n1)]
    (cond
      (< n 1)  [nil] 
      (= n1 n2) (distrib v (balanced-trees n1 v))
      :odd  (let [lower (balanced-trees n1  v)
                  higher (balanced-trees n2 v)]
              (distrib v lower higher )) )) )

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
  "P57: Write a function to construct a binary search tree from a list
  of integer numbers."
  [& xs]
  (reduce insert-val nil xs ))

(defn symmetric-cbalanced-trees
  "P58: Apply the generate-and-test paradigm to construct all symmetric,
  completely balanced binary trees with a given number of nodes."
  [n v]
  (if (= 0 (rem n 2 ))
    []; a tree with an even number of nodes can never be symmetric
    (->> (balanced-trees n v)
       (filter symmetric?))) )

(declare height-balanced-trees)

(defn height-balanced-trees*
  "P59 (**) Construct height-balanced binary trees In a
  height-balanced binary tree, the following property holds for every
  node: The height of its left subtree and the height of its right
  subtree are almost equal, which means their difference is not
  greater than one.

  Write a function to construct height-balanced binary
  trees for a given height. The predicate should generate all
  solutions via backtracking. Put the letter 'x' as information into
  all nodes of the tree."
  [h v]
  (cond (= 0 h) [nil]
        (= 1 h) [[v nil nil]]
        :else (let [n1 (height-balanced-trees (dec h) v)
                    n2 (height-balanced-trees (dec (dec h)) v)]
                (concat (distrib v n1) (distrib v n1 n2)) )))

(def height-balanced-trees 
  (memoize height-balanced-trees*))

(defn min-hbal-nodes
  "P60 (**) Construct height-balanced binary trees with a given number of nodes.
  Consider a height-balanced binary tree of height H. What is the
  maximum number of nodes it can contain? Clearly, MaxN = 2H - 1.

  However, what is the minimum number MinN? This question is more
  difficult. Try to find a recursive statement and turn it into a
  function minHbalNodes that takes a height and returns MinN."
  [h]
  (cond
    (= 0 h) 0
    (= 1 h) 1
    :else (+ 1 (min-hbal-nodes (dec h)) (min-hbal-nodes (dec (dec h))))))


(defn min-height-hbal
  [n]
  (if (< n 1)
    0
    (inc (min-height-hbal (/ n 2)))))


(defn max-height-hbal
  "P60 (**) What is the maximum height H a height-balanced binary tree
  with N nodes can have?"
  [n]
  (last (take-while (partial >= n) (map min-hbal-nodes (iterate inc 1)))))

(defn all-hbal-trees
  "P60 (**)  ... Now, we can attack the main problem: construct all the
  height-balanced binary trees with a given nuber of nodes."
  [n v]
  (let [xform (comp
               (mapcat #(height-balanced-trees % v))
               (filter #(= n (num-nodes %))))]
    (into [] xform  (range (min-height-hbal n) (inc (max-height-hbal n))))))



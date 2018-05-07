(ns ninety-nine-clojure.bintrees
  (:require [clojure.core.match :refer [match]]
            [clojure.math.numeric-tower :as m]
            [clojure.zip :as z]))

(defmacro spy [f]
  `(let [res# ~f]
     (println (apply str (repeat 10 "-" )))
     (println  '~f "->" res#)
     res#))

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
  (>= 1 (m/abs (-
              (num-nodes l)
              (num-nodes r)))))

(defn height-balanced? [[_ l r]]
  (>= 1 (m/abs (-
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
  [tree]
  (tree-seq (complement nil?)  #(remove nil? (next %)) tree))


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



(defn height-balanced-trees
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
  (int (Math/ceil (/ (Math/log (inc n)) (Math/log 2)))))


(defn max-height-hbal
  "P60 (**) What is the maximum height H a height-balanced binary tree
  with N nodes can have?"
  [n]
  (last (take-while #(>= n (min-hbal-nodes %)) (iterate inc 1))))


(defn all-hbal-trees
  "P60 (**)  ... Now, we can attack the main problem: construct all the
  height-balanced binary trees with a given nuber of nodes."
  [n v]
  (->>  (range (min-height-hbal n) (inc (max-height-hbal n)))
        (mapcat #(height-balanced-trees % v))
        (filter #(= n (num-nodes %)))))


(defn leaves
  "61A (*) Collect the leaves of a binary tree in a list.
  A leaf is a node with no successors. Write a method leafList to
  collect them in a list."
  [t]
  (map first (filter leaf?  (depth-first t))))

(defn leaf-count
 "P61 (*)A leaf is a node with no successors. Write a method leafCount
  to count them."
  [t]
  (count (leaves t)))

(defn internals
  "P62 (*) Collect the internal nodes of a binary tree in a list.
  An internal node of a binary tree has either one or two non-empty
  successors. Write a method internals to collect them in a list."
  [t]
  (map first (filter branch? (depth-first t))))


(defn at-level 
  "P62B (*) Collect the nodes at a given level in a list.
  A node of a binary tree is at level N if the path from the root to the
  node has length N-1. The root node is at level 1. Write a method
  at-level to collect all nodes at a given level in a list."
  [t l]
  (let [offset (dec (m/expt 2 (dec l)))]
    (map first (take (inc offset)  (drop offset (depth-first t))))))


(defn complete-binary-tree
  "P63 (**) Construct a complete binary tree.
  A complete binary tree with height H is defined as follows: The levels
  1,2,3,...,H-1 contain the maximum number of nodes (i.e 2(i-1) at the
  level i, note that we start counting the levels from 1 at the
  root). In level H, which may contain less than the maximum possible
  number of nodes, all the nodes are \"left-adjusted\". This means that
  in a levelorder tree traversal all internal nodes come first, the
  leaves come second, and empty successors (the Ends which are not
  really nodes!) come last.  Particularly, complete binary trees are
  used as data structures (or addressing schemes) for heaps.

  We can assign an address number to each node in a complete binary
  tree by enumerating the nodes in levelorder, starting at the root
  with number 1. In doing so, we realize that for every node X with
  address A the following property holds: The address of X's left and
  right successors are 2*A and 2*A+1, respectively, supposed the
  successors do exist. This fact can be used to elegantly construct a
  complete binary tree structure. Write a method completeBinaryTree
  that takes as parameters the number of nodes and the value to put in
  each node."
  [n v]
  (let [build (fn build [a]
               (if (> a n)
                 nil
                 [v (build (* 2 a)) (build (inc (* 2 a)))]))]
    (build 1)))

(defn complete-tree? [t]
  (let [minmax (fn minmax [[v l r] idx]
                 (cond
                   (nil? v) [0 idx]
                   :else (let [[max-l min-l] (minmax l (* 2 idx))
                               [max-r min-r] (minmax r (inc (* 2 idx)))]
                           [(max idx max-l max-r) (min min-l min-r)])))]
    (apply < (minmax t 1))))

(defn tree-zip
  "Returns a zipper for the binary trees defined in this namespace."
  [t]
  (z/zipper (complement nil?) rest (fn [n c] (with-meta (apply vector (first n) c) (meta n))) t))

(defn first-in-order
  "Moves to the first loc in an in-order traversal. If already at the
  end, stays there. Stays in the current subtree."
  [loc]
  (if (= :ioe (loc 1))
    loc
    (loop [p loc]
      (if (z/down p)
        (recur (z/down p))
        p))))

(defn next-in-order 
  "Moves to the next loc in an in-order traversal. When reaching the
  end, returns a distinguished loc detectable via end-in-order?. If
  already at the end, stays there."
  [loc]
  (if (= :ioe (loc 1))
    loc
    (or
     (and (z/branch? loc) (z/right (z/down loc)) (first-in-order (z/right (z/down loc))))
     (and (not (z/right loc)) (loop [p loc]
                                (if-let [one-up (z/up p)]
                                  (or (-> one-up z/right z/up) (recur one-up))
                                  [(z/node loc) :ioe])))
     (and (not (z/left loc)) (z/up loc)))))

(defn end-in-order?
  "Returns true if loc represents the end of an in-order traversal"
  [loc]
  (= :ioe (loc 1)))

(defn tree-edit [zipper f next end?]
  (loop [loc zipper state {}]  
    (let [node (z/node loc)
          path (z/path loc)
          depth (inc (count path))
          [new-node new-state :as res] (f node (assoc state :depth depth))
          new-loc (if (= new-node node)
                    loc
                    (z/replace loc new-node))
          next-loc (next new-loc)]
      (if (end? next-loc)
        (z/root new-loc)
        (recur next-loc new-state)))))

(defn inorder-tree-edit [zipper f]
  (tree-edit zipper f next-in-order end-in-order?))

(defn depth-first-tree-edit [zipper f]
  (tree-edit zipper f z/next z/end?))

(defn layout1
  "P64 (**) Layout a binary tree.
  Given a binary tree as the usual Prolog term t(X,L,R) (or nil). As a
  preparation for drawing the tree, a layout algorithm is required to
  determine the position of each node in a rectangular grid.  In this
  layout strategy, the position of a node v is obtained by the
  following two rules: x(v) is equal to the position of the node v in
  the inorder y(v) is equal to the depth of the node v in the tree
  sequence "
  [t]
  (inorder-tree-edit
   (first-in-order (tree-zip t))
   (fn [[v & children :as node] {:keys [x depth] :or {x 1} :as state}]
     (if node
       [(apply vector {:v v :x x :y depth } children) (assoc state :x (inc x))]
       [node state]))))



(defn layout2
  [t]
  (let [h (height t)
        x-offset-at (fn [d] (/ (m/expt 2 (- (inc h) d)) 2))
        x-offset (fn [d prev-d] (if (nil? prev-d) 0 (x-offset-at (max d prev-d))))] 
    (inorder-tree-edit
     (first-in-order (tree-zip t))
     (fn [[v & children :as node] {:keys [x depth prev-depth] :or {x 1} :as state}]
       (if node
         (let [new-x (+ x (x-offset depth prev-depth ))]
           [(apply vector {:v v :x new-x  :y depth } children) (assoc state :x new-x :prev-depth depth) ])
         [node state])))))


(defn right [[_ _ r]] r)
(defn left [[_ l _]] l)

(defn n-bounds [t dir]
  (loop [cnt 0 n (dir t)]
    (if (nil? n)
      cnt
      (recur (inc cnt) (dir n)))))

(defn bounds [l r]
  (let [bounds  [(n-bounds r left) (n-bounds l right)]
        ;_ (println "bounds: " bounds )
        ]      
    (apply + bounds))
  )

(defn depth-diff [d prev-d]
  (m/abs (- prev-d d)))

(defn x-compact [x node depth prev-depth left-offset]
;  (println x node depth prev-depth left-offset)
  (match [node depth]
         [_ (_ :guard #(= 1 (- % prev-depth) ))] (inc x) ; level down
         [(true :<< leaf?) _ ] x 
         [_ (_ :guard #(< 2 (- prev-depth %)))] (+ x (- 2 (depth-diff depth prev-depth))) ;multi level up
         [[_ l r] (_ :guard #(= 1 (- prev-depth %)))] (+ x (max 1 (+ (n-bounds l right) (n-bounds r left)))) ;; level up
         [[_ l r] (_ :guard #(> 0 (- prev-depth %)))] (+ x (bounds l r) (- left-offset (dec (depth-diff prev-depth depth)))) ;down max subtree bounds
         [[_ l r] _] (+ x (bounds l r) (- left-offset (depth-diff prev-depth depth))))) ;default: max subtree bounds


(defn offset-to-left [x [v l r]]
  ;(println "left-offset: " x v l r)
  (if v
    (- x (:x v))
    1))

(defn layout3
  [t]
  (inorder-tree-edit
   (first-in-order (tree-zip t))
   (fn [[v l r :as node] {:keys [x depth prev-depth left-offset] :or {x 1 left-offset 1 prev-depth 0} :as state} ]
     (if node
       (let [new-x (x-compact x node depth prev-depth left-offset)]
         [(conj [] {:v v :x new-x :y depth} l r) (assoc state :x new-x :prev-depth depth :left-offset (offset-to-left new-x l) ) ])
       [node state]))))


(comment 
  (def t '[f [b [a nil nil] [d [c nil nil] [e nil nil]]] [g nil [i [h nil nil] nil]]])
  (def t2 '[a [b nil [c nil nil]] [d nil nil]])
  (def t3 '[n [k [c [a nil nil] [e [d nil nil] [g nil nil]]] [m nil nil]] [u [p nil [q nil nil]] nil]])
  (def t4 '[{:v a, :x 2, :y 1}
   [{:v b, :x 1, :y 2}
    [{:v c, :x 0, :y 3} nil [{:v d, :x 1, :y 4} nil nil]]
    [{:v e, :x 2, :y 3} nil [{:v f, :x 3, :y 4} nil nil]]]
   [{:v g, :x 3, :y 2} [{:v h, :x 2, :y 3} nil nil] nil]])
  (->> (layout3 t4)
       depth-first
       (map (fn [[n _ _]] (select-keys n [:x :y])))
       set
       count)
       


  
  (->> (tree-zip t3)
       (iterate z/next)
       (take-while #(not (z/end? %))) ;; Zipper's "end of iteration" condition. 
       (map #(first (z/node %))))

  (->> (first-in-order (tree-zip t3))
       (iterate next-in-order)
       (take-while #(not (end-in-order? %))) ;; Zipper's "end of iteration" condition. 
       (map #(first (z/node %))))
)



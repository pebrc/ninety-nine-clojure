(ns ninety-nine-clojure.logictrees
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :refer :all]
            [clojure.core.logic.fd :as fd]))

(defne distr [d1 d2 o1 o2]
  ([d1 _ d1 d1])
  ([d1 d2 d1 d2])
  ([d1 d2 d2 d1]))


(defne hbal-tree 
   "P59 (**) Construct height-balanced binary trees In a
  height-balanced binary tree, the following property holds for every
  node: The height of its left subtree and the height of its right
  subtree are almost equal, which means their difference is not
  greater than one.

  Write a function to construct height-balanced binary
  trees for a given height. The predicate should generate all
  solutions via backtracking. Put the letter 'x' as information into
  all nodes of the tree."
  [d t]
  ([0 nil])
  ([1 ['x nil nil]])
  ([d ['x l r]]
   (fd/> d 1)
   (fresh [d1 d2 dl dr]
     (fd/in d1 d2 d (fd/interval 0 Integer/MAX_VALUE))
     (fd/eq (= (- d 1) d1))
     (fd/eq (= (- d 2) d2))
     (distr d1 d2 dl dr)
     (hbal-tree dl l)
     (hbal-tree dr r))))

(defn height-balanced-trees [h]
  (run* [q]
    (hbal-tree h q)))


(defne min-nodes [h n]
  ([0 0])
  ([1 1])
  ([h n]
   (fd/> h 1)
   (fresh [h1 h2 n1 n2]
     (is h1 h dec)
     (is h2 h1 dec)
     (min-nodes h1 n1)
     (min-nodes h2 n2)
     (fd/in n1 n2 n (fd/interval 0 Integer/MAX_VALUE))
     (fd/eq (= (+ 1 n1 n2) n)))))

(defn min-hbal-nodes
  "P60 (**) Construct height-balanced binary trees with a given number of nodes.
  Consider a height-balanced binary tree of height H. What is the
  maximum number of nodes it can contain? Clearly, MaxN = 2H - 1.

  However, what is the minimum number MinN? This question is more
  difficult. Try to find a recursive statement and turn it into a
  function minHbalNodes that takes a height and returns MinN."
  [h]
  (run 1 [q]
    (min-nodes h q)))

(defne min-height [n h]
  ([0 0])
  ([n h]
   (fresh [n1 h1]
     (fd/in n1 (fd/interval 0 Integer/MAX_VALUE))
     (fd/> n 0)
     (project [n n1]
              (== (quot n 2) n1))
     (min-height n1 h1)
     (fd/+ h1 1 h))))

(defne num-nodes [t n]
  ([nil 0])
  ([[_ l r] n]
   (fresh [nl nr]
     (num-nodes l nl)
     (num-nodes r nr)
     (fd/in nl nr n (fd/interval 0 Integer/MAX_VALUE))
     (fd/eq (= (+ 1 nl nr) n)))))


(defne max-height [n h h1 n1]
  ([n h h1 n1]
   (fd/> n1 n)
   (is h h1 dec))
  ([n h h1 n1]
   (fd/<= n1 n)
   (fresh [h2 n2]
     (is h2 h1 inc)
     (min-nodes h2 n2)
     (max-height n h h2 n2))))

(defn max-height-hbal
   "P60 (**)  ... Now, we can attack the main problem: construct all the
  height-balanced binary trees with a given nuber of nodes."
  [n h]
  (max-height n h 1 1))


(defn hbal-tree-nodes [n t]
  (fresh [hmin hmax h]
    (min-height n hmin)
    (max-height n hmax 1 1)
    (fd/>= h hmin)
    (fd/<= h hmax)
    (hbal-tree h t)
    (num-nodes t n)))

(defn all-hbal-trees [n]
  (run* [q]
    (hbal-tree-nodes n q)))

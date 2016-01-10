(ns ninety-nine-clojure.logictrees
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :refer :all]
            [clojure.core.logic.fd :as fd]))

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
     (is d1 d dec)
     (is d2 d1 dec)
     (permuteo [d1 d2] [dl dr])
     (hbal-tree dl l)
     (hbal-tree dr r))))


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

(min-hbal-nodes 5)




(comment (run* [q]
           (hbal-tree 4 q)))


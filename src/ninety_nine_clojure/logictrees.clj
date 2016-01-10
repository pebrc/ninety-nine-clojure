(ns ninety-nine-clojure.logictrees
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :refer :all]
            [clojure.core.logic.arithmetic :as a]))

(defne hbal-tree [d t]
   "P59 (**) Construct height-balanced binary trees In a
  height-balanced binary tree, the following property holds for every
  node: The height of its left subtree and the height of its right
  subtree are almost equal, which means their difference is not
  greater than one.

  Write a function to construct height-balanced binary
  trees for a given height. The predicate should generate all
  solutions via backtracking. Put the letter 'x' as information into
  all nodes of the tree."
  ([0 nil])
  ([1 ['x nil nil]])
  ([d ['x l r]]
   (a/> d 1)
   (fresh [d1 d2 dl dr]
     (is d1 d dec)
     (is d2 d1 dec)
     (permuteo [d1 d2] [dl dr])
     (hbal-tree dl l)
     (hbal-tree dr r))))

(run* [q]
  (hbal-tree 4 q))


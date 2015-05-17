(ns ninety-nine-clojure.lists
  (:require [clojure.core.match :refer (match)])
  (:use [clojure.set :only [difference]]) )

(defn last-builtin ([input] (last input)))

(defn my-last 
  "P01 (*) Find the last element of a list."
  [input]
  (if (next input)
    (recur (next input))
    (first input)))

(defn penultimate [xs]
  {:doc  "P02 (*) Find the last but one element of a list."
   :pre [(seq? xs)]}
  (loop [ret (first xs) xs xs]
    (if (next xs)
      (recur (first xs) (next xs))
      ret)))

(defn kth-builtin [n xs] (nth xs n))

(defn kth [n xs]  
  {:doc "P03 (*) Find the Kth element of a list." 
   :pre [(seq? xs)]}
  (if (= 0 n)
    (first xs)
    (recur (- n 1) (next xs)))
  )

(defn count-builtin [xs] (count xs))

(defn my-count 
  "P04 (*) Find the number of elements of a list."
  [xs]
  (loop [num 0 xs xs]
    (if (next xs)
      (recur (+ num 1) (next xs))
      (+ num 1))))

(defn my-count-reduce 
  "P04 (*) Find the number of elements of a list."
  [xs]
  (reduce (fn [c xs] (inc c)) 0 xs))

(defn my-reverse 
  "P05 (*) Reverse a list"
  [xs]
  (loop [ret [] xs xs]
    (if (empty? xs)
      ret
      (recur (cons (first xs) ret) (next xs)))))

(defn my-reverse-reduce 
  "P05 (*) Reverse a list"
  [xs]
  (reduce #(cons %2 %1) '() xs))

(defn palindrome? 
  "P06 (*) Find out whether a list is a palindrome."
  [xs]
  (= xs (reverse xs)))


(defn flatten-recur
  "P07 (**) Flatten a nested list structure."
  ([xs] (flatten-recur [] xs))
  ([acc xs]
     (if-not
      (seq? xs) (conj acc xs)
      (if (empty? xs)
        acc
        (-> (flatten-recur acc (first xs))
            (flatten-recur (rest xs)))))))

(defn flatten-destructured
  "P07 solution I saw on cchandler's github repo"
  [x & tail]
  (concat (if (seq? x)
            (apply flatten-destructured x)
            [x])
          (if (nil? tail)
            nil
            (apply flatten-destructured tail))))

(defn flatten-reduce
  "P07 solution I saw on rodnaph's github repo "
  [xs]
  (reduce #(if (seq? %2)
             (concat %1 (flatten-reduce %2))
             (concat %1 (list %2)))
          '() xs))

(defn compress
  "P08 (**) Eliminate consecutive duplicates of list elements."
  [xs]
  (reduce #(if-not (= (last %1) %2)
             (conj %1 %2)
             %1)
          []
          xs))

(defn pack
  "P09 (**) Pack consecutive duplicates of list elements into sublists."
  [xs]
  (partition-by identity xs))


(defn encode
  "P10 (*) Run-length encoding of a list.
Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as tuples (N, E) where N is the number of duplicates of the element E."
  [xs]
  (map #(list (count  %) (first  %)) (pack xs)))


(defn encode-modified
  "P11 (*) Modified run-length encoding.
Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N, E) terms."
  [xs]
  (map #(if (= 1 (first %))
          (second %)
          %) (encode xs)))

(defn decode
  "P12 (**) Decode a run-length encoded list.
Given a run-length code list generated as specified in problem P10, construct its uncompressed version."
  [xs]
  (flatten (map #(repeat (first %) (second %)) xs)))

(defn encode-direct
  "P13 (**) Run-length encoding of a list (direct solution).
Implement the so-called run-length encoding data compression method directly. I.e. don't use other methods you've written (like P09's pack); do all the work directly."
  [xs]
  (map #(list (count %) (first %)) (partition-by identity xs)))

(defn duplicate
  "P14 (*) Duplicate the elements of a list."
  [xs]
  (reduce #(conj %1 %2 %2 ) [] xs ))

(defn duplicate-n
  "P15 (**) Duplicate the elements of a list a given number of times."
  [n xs]
  (reduce #(concat %1 (repeat n %2 )) '() xs))

(defn drop-every
  "P16 (**) Drop every Nth element from a list."
  [n coll]
  (let [drop-lazily (fn drop-lazily [n coll cnt]
                      (lazy-seq
                       (cond
                        (not (seq coll)) []
                        (= (mod cnt n) 0) (drop-lazily n (rest coll) (inc cnt))
                        :else (cons (first coll) (drop-lazily n (rest coll) (inc cnt)))) ))]
    (drop-lazily n coll 1))
  )

(defn drop-every-2
  "P16 (**) Drop every Nth element from a list."
  [n coll]
  (keep-indexed #(if-not (= (mod (inc %1) n) 0) %2) coll))

(defn drop-every-x [lst n]
    "P16 solution from rodnaph"
    (->> (partition-all n lst)
         (map (partial take (dec n)))
         (flatten)))

(defn split 
  "P17 (*) Split a list into two parts."
  [n coll]
  (let [split-acc (fn split-acc [n coll acc]
                    (if (and (seq coll) (> n 0))                      
                      (recur (dec n) (rest coll) (conj acc (first coll)))
                      [acc coll]))]
    (split-acc n coll [])))


(defn slice 
  "P18 (**) Extract a slice from a list."
  [s e xs]
  (take (- e s) (drop s xs)))

(defn rotate 
  "P19 (**) Rotate a list N places to the left."
  [n xs]
  (let [ at (mod n (count xs)) 
        [newtail newhead] (split at xs)]
    (concat newhead newtail)))

(defn remove-at 
  "P20 (*) Remove the Kth element from a list."
  [k xs]
  (let [[head tail] (split k xs)]
    (list (concat head (rest tail)) (first tail))))

(defn insert-at 
  "P21 (*) Insert an element at a given position into a list."
  [elem pos xs]
  (let [[head tail] (split pos xs)]
    (concat head (cons elem tail))))

(defn my-range 
  "P22 (*) Create a list containing all integers within a given range."
  [start end]
  (lazy-seq (cons start (if (= start end)
                          '()
                          (my-range (inc start) end))) )
  )

(defn random-select 
  "P23 (**) Extract a given number of randomly selected elements from a list."
  [n from]
  (loop [n n from from res []]
    (if (= 0 n)
      res
      (let [[rem elem] (remove-at (rand (dec (count from))) from)]           
           (recur (dec n) rem  (cons elem res))))))

(defn random-select-idiomatic 
  "P23 as seen @rodnaph 's"
  [n from]
  (take n (shuffle from)))

(defn lotto 
  "P24 (*) Lotto: Draw N different random numbers from the set 1..M"
  [n m]
  (random-select n (range 1 (inc m))))

(defn random-permute
  "P25 (*) Generate a random permutation of the elements of a list."
  [xs]
  (random-select (count xs) xs))

(defn node-join [l r]
  (match [l r]
         [[:leaf _] [:leaf _]]           [:node 2 l r]
         [[:node c & _] [:leaf _]]       [:node (inc c) l r]
         [[:leaf _] [:node c & _]]       [:node (inc c) l r]
         [[:node cl & _] [:node cr & _]] [:node (+ cl cr) l r]))

(defn pair-leaves
  ([l] l)
  ([l1 l2 & r]
     (if r       
       (recur (node-join l1 l2) (apply pair-leaves r) nil)
       (node-join l1 l2))))

(defn build-tree
  "Build a complete, binary search tree, as described in
  http://okmij.org/ftp/Haskell/perfect-shuffle.txt"
  [vals]
  (apply pair-leaves (map #(conj [:leaf ] %) vals)))

(defn extract-tree 
  "Extracts the n-th element from the tree and returns that element
  paired with a tree with the element deleted. See:
  http://okmij.org/ftp/Haskell/perfect-shuffle.txt"
  [n tree]
  (match [n tree]
         [0 [:node _ [:leaf e] r]]                                           [e r]
         [n [:node 2  ([:leaf _] :as l) [:leaf r]]]                          [r l]
         [n [:node c ([:leaf _] :as l) r]]
           (let [[res newr] (extract-tree (dec n) r)] [res  [:node (dec c) l newr]])
         [n [:node (n1 :guard (partial  >= (inc n))) l [:leaf e]]]           [e l]
         [n [:node c ([:node (c1 :guard (partial < n)) _ _] :as l) r]]
           (let [[res newl] (extract-tree n l)] [res [:node (dec c) newl r]])
         [n [:node c ([:node c1 _ _ ] :as l) r]]
           (let [[res newr] (extract-tree (- n c1) r)] [res [:node (dec c) l newr]] ) 
         ))

(defn perfect-functional-shuffle 
  "Given a non-empty, complete binary search tree for a sequence of
  elements and a corresponding sequence of numbers such that each
  number is an independent sample from a uniform random distribution,
  computes the corresponding permutation of sequence of elements. See
  also: http://okmij.org/ftp/Haskell/perfect-shuffle.txt"
  [bst rnds]
  (loop [bst bst rnds rnds acc []]
    (cond
     (= :leaf (first bst)) (conj acc (second bst))
     :otherwise   (let [[el sub-bst] (extract-tree (first rnds) bst)]
        (recur sub-bst (next rnds) (conj acc el))))))

(defn random-permute-functional 
  "P25 (*) Generate a random permutation of the elements of a list. An
  attempt at a Clojure implemention of the algorithm/Haskell
  implementation described here:
  http://okmij.org/ftp/Haskell/perfect-shuffle.txt"
  [xs]
  (let [num-elems (count xs)]
    (perfect-functional-shuffle (build-tree xs)  (map  #(rand-int (- num-elems %)) (range num-elems)))))



(defn combinations 
  "P26 (**) Generate the combinations of K distinct objects chosen
  from the N elements of a list."
  [k n]
  (cond (= k 0) '(nil)
        (empty? n) nil
        :else (concat (map #(conj % (first n))
                           (combinations (dec k) (rest n)))
                      (combinations k (rest n)))))


(defn group3
  "P27 (**) Group the elements of a set into disjoint subsets. a) In
  how many ways can a group of 9 people work in 3 disjoint subgroups
  of 2, 3 and 4 persons?"
  [coll]
  (for [fours (combinations 4 coll)
        threes (combinations 3 (difference (set coll) (set fours)))
        twos (combinations 2 (difference (set  coll) (set fours) (set threes)))]
    [fours threes twos]))


(defn group
  "P27 (**) Group the elements of a set into disjoint subsets.
  Generalize the above predicate in a way that we can specify a list
  of group sizes and the predicate will return a list of groups."
  [groups coll]
  (if (empty? groups)
    '(nil)
    (mapcat (fn [g]
           (map  #(conj % g)
                 (group (rest groups) (difference (set coll) (set g)))))
         (combinations (first groups) coll))))

(defn lsort 
  "Sorting a list of lists according to length of sublists. We suppose
  that a list contains elements that are lists themselves. The
  objective is to sort the elements of the list according to their
  length. E.g. short lists first, longer lists later, or vice versa."
  [coll]
  (sort-by #(count %) coll))

(defn lsort-freq 
  "Sorting a list of lists according to length of sublists. Again, we
  suppose that a list contains elements that are lists themselves. But
  this time the objective is to sort the elements according to their
  length frequency; i.e. in the default, sorting is done ascendingly,
  lists with rare lengths are placed first, others with a more
  frequent length come later."
  [coll]
  (let [freqs (frequencies (map #(count %) coll))]
    (sort #(< (freqs (count %1)) (freqs (count %2))) coll)))

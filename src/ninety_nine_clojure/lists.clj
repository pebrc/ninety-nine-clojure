(ns ninety-nine-clojure.lists)

(defn last-builtin ([input] (last input)))

(defn my-last [input]
  "P01 (*) Find the last element of a list."
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

(defn my-count [xs]
  "P04 (*) Find the number of elements of a list."
  (loop [num 0 xs xs]
    (if (next xs)
      (recur (+ num 1) (next xs))
      (+ num 1))))

(defn my-count-reduce [xs]
  "P04 (*) Find the number of elements of a list."
  (reduce (fn [c xs] (inc c)) 0 xs))

(defn my-reverse [xs]
  "P05 (*) Reverse a list"
  (loop [ret [] xs xs]
    (if (empty? xs)
      ret
      (recur (cons (first xs) ret) (next xs)))))

(defn my-reverse-reduce [xs]
  "P05 (*) Reverse a list"
  (reduce #(cons %2 %1) '() xs))

(defn palindrome? [xs]
  "P06 (*) Find out whether a list is a palindrome."
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

(defn split [n coll]
  "P17 (*) Split a list into two parts."
  (let [split-acc (fn split-acc [n coll acc]
                    (if (and (seq coll) (> n 0))                      
                      (recur (dec n) (rest coll) (conj acc (first coll)))
                      [acc coll]))]
    (split-acc n coll [])))


(defn slice [s e xs]
  "P18 (**) Extract a slice from a list."
  (take (- e s) (drop s xs)))

(defn rotate [n xs]
  "P19 (**) Rotate a list N places to the left."
  (let [ at (if (< n 0) (+ (count xs) n) n) 
        [newtail newhead] (split at xs)]
    (concat newhead newtail)))

(defn remove-at [k xs]
  "P20 (*) Remove the Kth element from a list."
  (let [[head tail] (split k xs)]
    (list (concat head (rest tail)) (first tail))))

(defn insert-at [elem pos xs]
  "P21 (*) Insert an element at a given position into a list."
  (let [[head tail] (split pos xs)]
    (concat head (cons elem tail))))

(defn my-range [start end]
  "P22 (*) Create a list containing all integers within a given range."
  (lazy-seq (cons start (if (= start end)
                          '()
                          (my-range (inc start) end))) )
  )

(defn random-select [n from]
  "P23 (**) Extract a given number of randomly selected elements from a list."
  (loop [n n from from res []]
    (if (= 0 n)
      res
      (let [[rem elem] (remove-at (rand (dec (count from))) from)]           
           (recur (dec n) rem  (cons elem res))))))

(defn random-select-idiomatic [n from]
  "P23 as seen @rodnaph 's"
  (take n (shuffle from)))

(defn lotto [n m]
  "P24 (*) Lotto: Draw N different random numbers from the set 1..M"
  (random-select n (range 1 (inc m))))

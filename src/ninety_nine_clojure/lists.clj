(ns ninety-nine-clojure.lists)

(defn last-builtin ([input] (last input)))

(defn my-last [input]
  "P01 (*) Find the last element of a list."
  (if (next input)
    (recur (next input))
    (first input)))

(defn my-butlast [xs]
  {:doc  "P02 (*) Find the last but one element of a list."
   :pre [(seq? xs)]}
  (loop [ret (first xs) xs xs]
    (if (next xs)
      (recur (first xs) (next xs))
      ret)))

(defn kth-builtin [n xs] (nth xs n))

(defn kth [n xs]  
  {:doc "P03 (*) Find the Kth element of a list." 
   :pre [(seq? xs) (< 0 n)]}
  (loop [n n xs xs]
    (if (= 0 n)
      (first xs)
      (recur (- n 1) (next xs))))
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
     (if
      (not (seq? xs)) (conj acc xs)
      (if (empty? xs)
        acc
        (-> (flatten-recur acc (first xs))
            (flatten-recur (rest xs)))))))

(defn flatten-destructured
  "P07 solution I saw on cchandler's github repo"
  [x & tail]
  (print x)
  (concat (if (seq? x)
            (apply flatten-destructured x)
            [x])
          (if tail
            (apply flatten-destructured tail)
            nil)))

(defn flatten-reduce
  "P07 solution I saw on rodnaph's github repo "
  [xs]
  (reduce #(if (seq? %2)
             (concat %1 (flatten-reduce %2))
             (concat %1 (list %2)))
          '() xs))

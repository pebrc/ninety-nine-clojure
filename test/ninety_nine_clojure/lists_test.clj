(ns ninety-nine-clojure.lists-test
  (:require [clojure.test :refer :all]
            [ninety-nine-clojure.lists :refer :all]))

(deftest p01-builtin-test
  (let [in (list 1 2 3)]
    (is (= 3 (last-builtin in)))))

(deftest p01-last
  (is (= 3 (my-last (list 1 2 3)))))

(deftest p02-penultimate
  (is (= 2 (penultimate (list 1 2 3)))))

(deftest p02-empty-input
  (let [in ()]
    (is (= nil (penultimate in)))))

(deftest p02-non-seq
  (is (thrown? AssertionError (penultimate :not-a-seq))))

(deftest p03-builtin-test
  (let [in (list 1 2 3 4)]
    (is (= 3 (kth-builtin 2 in)))))

(deftest p03-kth
  (is (= 3 (kth 2 (list 1 2 3 4)))))

(deftest p03-empty-input
  (is (thrown? AssertionError (kth 2 '()))))

(deftest p03-index-out-of-bounds
  (is (thrown? AssertionError (kth 10 (list 1)))))

(deftest p04-builtin-test
  (is (= 3 (count-builtin (list 1 2 3)))))

(deftest p04-test
  (is (= 3 (my-count (list 1 2 3)))))

(deftest p04-test-2
  (is (= 3 (my-count-reduce (list 1 2 3)))))

(deftest p05-test
  (is (= '(3 2 1) (my-reverse '(1 2 3)))))

(deftest p05-test-2
  (is (= '(3 2 1) (my-reverse-reduce '(1 2 3)))))

(deftest p06-test
  (is (= true (palindrome? '(1 2 3 3 2 1))))
  (is (= false (palindrome? '(1 2 3)))))

(deftest p07-test
  (is (= '(1 1 2 3 5 8) (flatten-recur (list (list 1 1) 2 (list 3 (list 5 8)))))))

(deftest p07-flat-list-is-flat
  (is (= '(1) (flatten-recur '(1)))))

(deftest p07-one-level-flatten
  (is (= '(1 2) (flatten-recur (list 1 (list 2))))))

(deftest p07-destructured-flatten-with-one-elem
  (is (= '(1) (flatten-destructured '(1)) )))

(deftest p07-one-level-flatten-destructured
  (is (= '(1 2) (flatten-destructured (list 1 (list 2))))))

(deftest p07-test-destructured
  (is (= '(1 1 2 3 5 8) (flatten-destructured (list (list 1 1) 2 (list 3 (list 5 8)))))))
(deftest p07-reduce-flatten-with-one-elem
  (is (= '(1) (flatten-reduce '(1)) )))

(deftest p07-one-level-flatten-reduce
  (is (= '(1 2) (flatten-reduce (list 1 (list 2))))))

(deftest p07-test-reduce
  (is (= '(1 1 2 3 5 8) (flatten-reduce (list (list 1 1) 2 (list 3 (list 5 8)))))))

(deftest p08-compress
  (is (= '[a b c a d e]  (compress  '[a a a a b c c a a d e e e e]))))

(deftest p09-pack
  (is (= '( ( a a a a) ( b) ( c c) ( a a) ( d) ( e e e e)) (pack  '( a a a a b c c a a d e e e e)))))

(deftest p10-encode
  (is (= '((4 a) (1 b) (2 c) (2 a) (1 d) (4 e)) (encode '( a a a a b c c a a d e e e e) ))))

(deftest p10-encode-modified
  (is (= '((4 a)  b  (2 c)  (2 a)  d  (4 e)) (encode-modified '( a a a a b c c a a d e e e e)))))

(deftest p12-decode
  (is (= '( a a a a b c c a a d e e e e) (decode '((4 a) (1 b) (2 c) (2 a) (1 d) (4 e))) )))

(deftest p13-encode-direct
  (is (= '((4 a) (1 b) (2 c) (2 a) (1 d) (4 e)) (encode-direct '( a a a a b c c a a d e e e e) ))))

(deftest p14-duplicate
  (is (= '(a a b b c c c c d d) (duplicate '(a b c c d)))))

(deftest p15-duplicate-n
  (is (= '(a a a b b b c c c c c c d d d) (duplicate-n 3 '(a b c c d)))))

(deftest p16-drop-every-nth
  (is (= '( a  b  d  e  g  h  j  k) (drop-every 3  '(a b c d e f g h i j k)
                                                ))))

(deftest p16-drop-every-nth-2
  (is (= '( a  b  d  e  g  h  j  k) (drop-every-2 3  '(a b c d e f g h i j k)
                                                  ))))

(deftest p17-split
  (is (= ['(a b c) '(d e f g h i j k)]) (split 3 '(a b c d e f g h i j k))))

(deftest p18-slice
  (is (= '(d e f g) (slice 3 7 '(a b c d e f g h i j k)))))

(deftest p19-rotate
  (is (= '(d e f g h i j k a b c) (rotate 3 '(a b c d e f g h i j k))))
  (is (= '(j k a b c d e f g h i) (rotate -2 '(a b c d e f g h i j k)))))

(deftest p20-remove-at
  (is (= '((a c d) b) (remove-at 1 '(a b c d)))))

(deftest p21-insert-at
  (is (= '(1 new 2 3 4) (insert-at 'new 1 '(1 2 3 4)))))

(deftest p22-range
  (is (= '(4 5 6 7 8 9) (my-range 4 9))))

(deftest p23-random-select
  (is (= 3 (count (random-select 3 '(1 2 3 4 5 6))) )))

(deftest p24-lotto
  (let [draw (lotto 6 49)]
    (is (= 6 (count draw)))
    (is (>= 49 (apply max draw))))  
  )

(deftest p25-random-permute
  (let [original (range 20)
        permutation (random-permute original)]
    (is (= (count original) (count permutation)))
    (is (not= original permutation))))


(deftest p25-functional-shuffle
  (is '(a b c d e) (perfect-functional-shuffle (build-tree '(a b c d e)) [0 0 0 0]))
  (is '(e d c b a) (perfect-functional-shuffle (build-tree '(a b c d e)) [4 3 2 1]))
  (is '(c b e a d) (perfect-functional-shuffle (build-tree '(a b c d e)) [2 1 2 0])))

(deftest p25-random-permute-functional
  (let [original (range 20)
        permutation (random-permute-functional original)]
    (is (= (count original) (count permutation)))
    (is (not= original permutation))))

(defn ! [n]
  (apply * (range 1 (inc n))))

(deftest p26-combinations
  (is (= '((0 1) (0 2) (1 2)) (combinations 2 (range 3))))
  (is (= 220 (count (combinations 3 (range 12))))))

(deftest p27-multinomial-coefficients
  "Number of permutations is n!/(k1!*k2!...km!)"
  (let [people '("Aldo" "Beat" "Carla" "David" "Evi" "Flip" "Gary" "Hugo" "Ida")
        num-permutations (/ (! 9) (* (! 4) (! 3) (! 2)))
        group3-res (group3 people)
        group-gen-res (group [4 3 2] people)]
    (is (= num-permutations (count group3-res)))
    (is (= num-permutations (count group-gen-res)))
    (is (= group3-res group-gen-res))))

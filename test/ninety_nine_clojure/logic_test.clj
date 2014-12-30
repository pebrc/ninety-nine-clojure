(ns ninety-nine-clojure.logic-test
  (:require [clojure.test :refer :all]
            [ninety-nine-clojure.logic :refer :all]))


(deftest infix-and
  (is true (infix true and true)))

(deftest infix-nested
  (is true (infix true and (false or true) )))

(deftest infix-double-operator
  (is true (infix true and not false)))

(deftest infix-nested-first
  (is true (infix (false or true) and true)))

(deftest infix-one-unary
  (is true (infix (not false and true))))

(deftest infix-with-unary-last
  (is true (infix (false or not false))))

(deftest infix-with-two-unary-ops
  (is true (infix (not false and not false))))

(deftest gray-1
  (is (= ["0" "1"]( gray 1))))

(deftest gray-2
  (is (= ["00" "01" "11" "10"] (gray 2))))

(deftest gray-3
  (is (= ["000" "001" "011" "010" "110" "111" "101" "100"] (gray* 3))))

(deftest gray-3-memoized
  (is (= ["000" "001" "011" "010" "110" "111" "101" "100"] (gray 3))))

(deftest gray-bitwise-1
  (is (= ["0" "1"]( gray-seq-bitwise 1))))

(deftest gray-bitwise-2
  (is (= ["00" "01" "11" "10"] (gray-seq-bitwise 2))))

(deftest gray-bitwise-3
  (is (= ["000" "001" "011" "010" "110" "111" "101" "100"] (gray-seq-bitwise 3))))

(deftest huffmann-acceptance
  (is (= '([a "0"] [c "100"] [b "101"] [f "1100"] [e "1101"] [d "111"])
         (-> '([a 45] [b 13] [c 12] [d 16] [e 9] [f 5])
             build-huffman-tree
             map-symbols
             ))))

(deftest huffmann-empties-both-queues
  (is (= '([\a "00"] [\space "01"] [\b "10"] [\c "11"])
         (-> (frequencies "a bc")
             build-huffman-tree
             map-symbols))))

(defn normalize-to-length [encoded]
  (->  (map (fn [[v code]] [v (count code)]) encoded)
       sort))

;;; I assume subtle differences during tree building are OK as long as
;;; the resulting code lengths for each value are identical
;;; Therefore I am comparing code lengths by normalizing the expected
;;; results and the actual results to a sorted sequence of tuples of
;;; value and code length

(deftest huffman-wikipedia-example
  (is (=  (-> '([\space "111"] [\a "010"] [\e "000"] [\f "1101"] [\h "1010"] [\i "1000"] [\m "0111"] [\n "0010"] [\s "1011"] [\t "0110"] [\l "11001"] [\o "00110"] [\p "10011"] [\r "11000"] [\u "00111"] [\x "10010"])
              normalize-to-length)
         (-> (frequencies "this is an example of a huffman tree")
             build-huffman-tree
             map-symbols
             normalize-to-length))))

(deftest huffman-rosetta-example
  (is (= (->  '([\n "000"] [\s "0010"] [\m "0011"] [\o "0100"] [\t "01010"] [\x "01011"] [\p "01100"] [\l "01101"] [\r "01110"] [\u "01111"] [\c "10000"] [\d "10001"] [\i "1001"] [\space "101"] [\a "1100"] [\e "1101"] [\f "1110"] [\g "11110"] [\h "11111"])
              normalize-to-length) 
         (-> (frequencies "this is an example for huffman encoding")
             build-huffman-tree
             map-symbols
             normalize-to-length))))

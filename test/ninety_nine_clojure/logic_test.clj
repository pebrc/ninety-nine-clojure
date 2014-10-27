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

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

(deftest grey-1
  (is (= ["0" "1"]( grey 1))))

(deftest grey-2
  (is (= ["00" "01" "11" "10"] (grey 2))))

(deftest grey-3
  (is (= ["000" "001" "011" "010" "110" "111" "101" "100"] (grey* 3))))

(deftest grey-3-memoized
  (is (= ["000" "001" "011" "010" "110" "111" "101" "100"] (grey 3))))

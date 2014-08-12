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

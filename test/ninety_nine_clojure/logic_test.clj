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

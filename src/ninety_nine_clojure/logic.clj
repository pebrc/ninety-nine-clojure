(ns ninety-nine-clojure.logic
  (:require [clojure.pprint :only print-table]))

(defn and-fn [x y]
  (if x (if y y false) false))

(defn or-fn [x y]
  (if x x y))

(defn nand [x y]
  (not (and x y)))

(defn nor [x y]
  (not (or x y)))

(defn equ [x y]
  (or (and x y)
      (and (not x) (not y))))

(defn xor [x y]
  (not (= x y)))

(defn impl [x y]
  (or (not x)
      (and x y)))

(def inputs [[true true] [true false] [false true] [false false]])



(defmacro evaluator [expr]
  `(fn [& args#]
     (eval `(~'~expr ~@args#))))


(defmacro table [expr]
  `(let [header#  [:a :b :result]
         ev# (evaluator ~expr)
         rows# (map (fn [x#] (interleave header# (flatten (vector x# (apply ev# x#) )))) inputs)
         sorted-rows# (map (fn [y#] (apply sorted-map y#)) rows#)]
     (clojure.pprint/print-table sorted-rows#)
     ))

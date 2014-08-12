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
         rows# (->> inputs
                    (map (fn [x#] (interleave header# (flatten [x# (apply ev# x#)] ))))
                    (map (fn [x#] (apply sorted-map x#))))]
     (clojure.pprint/print-table rows#)))

(defmacro infix
  ([] '())
  ([x] (if (seq? x)
         `(infix ~@x)
         x))
  ([op a]
     `(~op ~a))
  ([a op b]
     `(~op (infix ~a) (infix ~b)))
  ([a op1 op2 b]
     `(~op1 (infix ~a) (~op2 (infix ~b)))))

(defmacro i
  "Creates a Clojure function from a logical expression in infix
  syntax like 'a and b'. Binds two variable a and b by convention"
  [& args]
  
  (let [a (gensym)
        b (gensym)
        code (clojure.walk/postwalk-replace {'a a 'b b} args)]
    `(fn [~a ~b] (infix ~@code))))

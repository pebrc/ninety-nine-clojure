(ns ninety-nine-clojure.logic
  (:require [clojure.pprint :only print-table]
            [clojure.math.combinatorics :as combo]))

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

(def inputs [true false])

(defn gen-inputs [arity]
  (apply combo/cartesian-product (repeat arity inputs)))

(defmacro evaluator [expr]
  `(fn [& args#]
     (eval `(~'~expr ~@args#))))


(defmacro table
  ([expr]
     `(table (first (:arglists (meta ~expr))) ~expr))
  ([arglist expr]     
     `(let [header#  (concat (map keyword ~arglist) [:result])
            ev# (evaluator ~expr)
            rows# (->> (gen-inputs (count ~arglist))
                       (map (fn [x#] (interleave header# (flatten [x# (apply ev# x#)] ))))
                       (map (fn [x#] (apply sorted-map x#))))] 
        (clojure.pprint/print-table rows#))))

(def unary? {'not true})

(defmacro infix
  ([] '())
  ([x] (if (seq? x)
         `(infix ~@x)
         x))
  ([op a]
     `(~op ~a))
  ([a op b]
     `(~op (infix ~a) (infix ~b)))
  ([a b c d]
     (if (unary? a)
       `(~c (~a (infix ~b)) (infix ~d) )
       `(~b (infix ~a) (~c (infix ~d)))
       ))
  ([a b c d & more]
     (cond
      (and (unary? d) (unary? a)) `(~c (~a (infix ~b)) (~d (infix ~more)))
      (xor (unary? c) (unary? a)) (let [exp (first more)
                                       tail (rest more)]
                                    `(~exp (infix ~a ~b ~c ~d) (infix ~tail)))
      :else  `(~d (~b (infix ~a) (infix ~c)) (infix ~@more)))))


(defmacro i
  "Creates a Clojure function from a logical expression in infix
  syntax like 'a and b'. Takes a vector of used symbols as its first
  argument and the logical expression in infix syntax as its second"
  [bindings & e]  
  (let [bindingslist (list bindings) ]
    `(with-meta (fn  ~bindings (infix ~@e)) {:arglists '~bindingslist})))

(declare grey)

(defn grey* [bits]
  "Gray code.
An n-bit Gray code is a sequence of n-bit strings constructed
according to certain rules. For example, n = 1: C(1) = ('0', '1'). n =
2: C(2) = ('00', '01', '11', '10'). n = 3: C(3) = ('000', '001',
'011', '010', '110', '111', '101', '100'). Find out the construction
rules and write a function to generate Gray codes."
  (cond
   (= bits 1) ["0" "1"]
   :else (let [lower (grey (dec bits))
               upper (->> (reverse lower)
                          (map #(str "1" %)))
               lower-prefixed (map #(str "0" %) lower)]
           (concat lower-prefixed upper))))

(def grey (memoize grey*))

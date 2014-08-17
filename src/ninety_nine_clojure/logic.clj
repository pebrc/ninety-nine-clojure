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
  syntax like 'a and b'. Binds two variable a and b by convention"
  [bindings & args]  
  (let [bindingslist (list bindings) ]
    `(with-meta (fn  ~bindings (infix ~@args)) {:arglists '~bindingslist})))

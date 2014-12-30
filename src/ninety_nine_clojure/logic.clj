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

(declare gray)

(defn gray* [bits]
  "Gray code.
An n-bit Gray code is a sequence of n-bit strings constructed
according to certain rules. For example, n = 1: C(1) = ('0', '1'). n =
2: C(2) = ('00', '01', '11', '10'). n = 3: C(3) = ('000', '001',
'011', '010', '110', '111', '101', '100'). Find out the construction
rules and write a function to generate Gray codes."
  (cond
   (= bits 1) ["0" "1"]
   :else (let [lower (gray (dec bits))
               upper (->> (reverse lower)
                          (map #(str "1" %)))
               lower-prefixed (map #(str "0" %) lower)]
           (concat lower-prefixed upper))))

(def gray (memoize gray*))

(defn gray-encode [n]
  (bit-xor n (bit-shift-right n 1)))

(defn gray-decode [n]
  (loop [g 0 bits n]
    (if (zero? bits)
      g
      (recur (bit-xor g bits) (bit-shift-right bits 1)))))

(defn gray-seq-bitwise [n]
  (->> (range (reduce * (repeat n 2)))
       (map gray-encode)
       (map #(clojure.pprint/cl-format nil (str "~" n "'0b") %))))

;;; Problem 50: Huffman code

(defn by-lowest-freq [leaves]
  (sort #(< (:freq %1) (:freq %2)) leaves))

(defn next-lowest-freq [q1 q2]
  (let [leaf (first q1)
        leaves  (rest q1)
        node  (first q2)
        nodes  (rest q2)]
    (if (> (:freq leaf (Long/MAX_VALUE)) (:freq node (Long/MAX_VALUE)))
      [node q1 nodes]
      [leaf leaves q2])))

(defn leaf [[val freq]]
  (assoc {} :type :leaf :freq freq :val val))

(defn node [l r]
  (assoc {:type :node} :left l :right r :freq (+ (:freq l) (:freq r))))


(defn  build-tree
  ([input] (build-tree input []))
  ([input nodes]
   (if (and  (empty? input) (= 1 (count nodes)))
     (first nodes)
     (let [[l q1 q2]  (next-lowest-freq input nodes)
           [r q3 q4] (next-lowest-freq q1 q2)         
           node (node l r)]
       (recur  q3 (by-lowest-freq (conj (vec q4) node)))))))


(defn make-leaves [input]
  (map leaf input))

(defn build-huffman-tree [input]
  (build-tree (by-lowest-freq (make-leaves input)) '()))

(defn branch? [n]
  (= :node (:type n)))

(defn map-symbols
  ([tree] (map-symbols tree ""))
  ([tree prefix]
     (if (branch? tree)
       (concat  (map-symbols (:left tree) (str prefix "0"))
                (map-symbols (:right tree) (str prefix "1")))
       [[(:val tree) prefix]])))

(defn probs [items]
  (let [freqs (frequencies items) sum (reduce + (vals freqs))]
    (into '() (map (fn [[k v]] [k (/ v sum)]) freqs))))

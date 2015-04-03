(ns ninety-nine-clojure.typedtrees
  (:require [clojure.core.typed :as t]))


(t/ann-datatype Node [val :- Any
                      l   :- Tree
                      r   :- Tree])
(deftype Node [val l r])

(t/defalias Tree
  (t/Nilable Node))


(->Node "nil is a valid tree" nil nil)

(->Node :a (->Node :b nil nil) nil)

;; ArityException: Wrong number of args (2)
;; (->Node :a (->Node :b nil nil ))

;; Does not typecheck
;; (->Node :a :b :c)


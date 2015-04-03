(ns ninety-nine-clojure.bintrees
  (:require [clojure.core.match :refer [match]]))

(defn tree? [t]
  (match t
         [label (true :<< tree?) (true :<< tree?)] true
         nil                                       true
         :else                                     false))


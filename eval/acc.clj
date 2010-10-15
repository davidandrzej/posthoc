(comment

  General accuracy evaluations

)

(ns posthoc.eval.acc
  (:use [clojure.contrib.generic.math-functions :only (sqr)]))

(defn arithmetic-mean 
  [vals] 
  (/ (double (reduce + vals)) (count vals)))

(defn variance [vals] 
  (let [m (arithmetic-mean vals)]
    (arithmetic-mean (map #(sqr (- %1 m)) vals))))

(defn binary-relevance
  "Convert list of items to list of 1's (relevant) and 0's (not)"
  [vals relevancepred]
  (map #(if (relevancepred %1) 1 0) vals))

(defn accuracy [vals pospred]
  "Standard accuracy calculation"
  [vals pospred]
  (/ (double (reduce + (binary-relevance vals pospred))) 
     (count vals)))


(comment

  Mean average precision of a ranking

)

(ns posthoc.eval.mean-avg-prec
  (:use [clojure.contrib.seq-utils :only (positions)])
  (:use [posthoc.util])
  (:use [posthoc.eval.acc]))

;
; TEST VALUES
;
(def testranked (range 10))
(def testrelevancepred even?)

(defn precision-at
  "Given relevance predicate, calc precision up to and incl position"
  [vals relevancepred position]
  (accuracy (take (+ 1 position) vals) relevancepred))
  
(defn mean-avg-prec
  " Calculate mean average precision of ranking, given by mean of
    precision at each relevant item in ranking "
  [ranked relevancepred]
  (arithmetic-mean (map #(precision-at vals relevancepred %1)
                        (positions relevancepred ranked))))

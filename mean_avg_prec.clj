(ns posthoc.mean-avg-prec
  (:use [clojure.contrib.seq-utils :only (positions)])
  (:use [posthoc.util]))

;
; TEST VALUES
;
(def testranked (range 10))
(def testrelevancepred even?)

(defn arithmetic-mean [lst] (/ (double (reduce + lst)) (count lst)))

(defn calc-precision
  "Given binary relevance, precision up to and including position"
  [binvals position]
  (let [numvals (+ 1 position)]    
    (/ (double (reduce + (take numvals binvals))) numvals)))

(defn binary-relevance
  "Convert list of items to list of 1's (relevant) and 0's (not)"
  [vals relevancepred]
  (for [val vals]
    (if (relevancepred val) 1 0)))
  
(defn mean-avg-prec
  "Calculate mean average precision of ranking"
  [ranked relevancepred]
  (let [binvals (binary-relevance ranked relevancepred) ; convert ranked to 1/0's
        rpositions (positions not-zero? binvals)  ; get relevant positions
        precisions (map #(calc-precision binvals %1) 
                        rpositions)] ; calc prec at each relevant
    (arithmetic-mean precisions))) ; return arithmetic mean 

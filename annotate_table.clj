(ns posthoc.annotate-table
  (:use [clojure.set])
  (:use [clojure.contrib.duck-streams :only 
         (read-lines append-spit write-lines)])
  (:use [clojure.contrib.generic.math-functions :only (sqr)])
  (:use [posthoc.mean-avg-prec])
  (:use [posthoc.fileio.parse-topics])
  (:use [posthoc.fileio.parse-labels])
  (:use [posthoc.best-topics])
  (:use [posthoc.util]))


; File containing labels
(def labelfile "./labels/%d.labelled.txt")

(defn annotate
  [tidx words]
  (let [labels (read-label-file (format labelfile tidx))
        poswords (set (:pos labels))
        seedwords (set (:seed labels))]
    (doseq [word words]
      (if (or (contains? poswords word) (contains? seedwords word))
        (println (format "\\textbf{%s}" word))
        (println word)))))

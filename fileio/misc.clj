(comment

  Misc file/text/io functions

)

(ns posthoc.fileio.misc
  (:use [clojure.set])
  (:use [clojure.contrib.duck-streams :only 
         (read-lines file-str append-spit write-lines)])
  (:use [clojure.contrib.string :only (split trim blank?)])
  (:use [incanter.core])
  (:use [posthoc.util]))


;
; Lazy read space-delimitd text file
;
(defn read-tok-line
  "Break single line into space-separated tokens"
  [line]
  (filter not-blank? (split #"\s+" line)))

(defn read-tok-lines
  "Lazy helper for read-int-file"
  [lines]
  (if (empty? lines)
    '()
    (concat (read-tok-line (first lines))
            (lazy-seq (read-tok-lines (rest lines))))))

(defn read-tok-file
  "Parse file containing line(s) of space-separated integers"
  [filename]
  (read-tok-lines (read-lines filename)))


;
; Read file where each line is a sequence of comma separated n-grams
;
(defn tok-trim
  "Given string, return seq of trimmed strings split on whitespace"
  [s]
  (filter not-blank? (map trim (split #"\s+" s))))

(defn read-str-lists
  "Each line is a sequence of comma separated n-grams"
  [filename]
  (for [line (read-lines filename)]
    (map tok-trim (split #"," line))))


;
; Write java matrix out to text file
;
(defn write-matrix
  "Assume mat is double[][], write out to filename (one row / line)"
  [mat filename]
  (write-lines filename
               (for [row (seq mat)]
                 (apply str (interpose " " (map (partial format "%f") 
                                                (seq row)))))))

(defn read-matrix
  "Read double matrix from plaintext (one line per row)"
  [matfn]
  (matrix (for [line (read-lines matfn)]
            (map #(Double/parseDouble %1)
                 (read-tok-line line)))))


(def phifn "/Users/andrzejewski1/code/eval-logic/macvspc/macvspc.phi")
(def phi (read-matrix phifn))

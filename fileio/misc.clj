(comment

  Misc file/text/io functions

)

(ns posthoc.fileio.misc
  (:use [clojure.set])
  (:use [clojure.contrib.duck-streams :only 
         (read-lines file-str append-spit write-lines)])
  (:use [clojure.contrib.string :only (split trim blank?)])
  (:use [posthoc.util]))

;
; Lazy read a text file full of integers
;

(defn read-int-line
  "Parse single line of space-separated integers" 
  [line]
  (map #(Integer/parseInt %1) 
       (filter not-blank? (split #"\s+" line))))

(defn read-int-lines
  "Lazy helper for read-int-file"
  [lines]
  (if (empty? lines)
    '()
    (concat (read-int-line (first lines))
            (lazy-seq (read-int-lines (rest lines))))))

(defn read-int-file
  "Parse file containing line(s) of space-separated integers"
  [filename]
  (read-int-lines (read-lines filename)))

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

(defn write-matrix-java 
  "Assume mat is double[][], write out to filename (one row / line)"
  [mat filename]
  (write-lines filename
               (for [row (seq mat)]
                 (apply str (interpose " " (map (partial format "%f") 
                                                (seq row)))))))

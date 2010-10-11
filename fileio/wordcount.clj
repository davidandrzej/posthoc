(comment

  Get the word counts for a set of documents 

)

(ns wordcount
  (:import (opennlp.tools.lang.english Tokenizer SentenceDetector))
  (:use [clojure.contrib.duck-streams :only (read-lines file-str 
                                                        append-spit
                                                        write-lines)])
  (:use [clojure.contrib.string :only (split trim blank? lower-case)]))

(def tokModel "./models/EnglishTok.bin.gz")

(defn wc-line
  "Get lowercased word counts for a single line" 
  [tokenizer line]
  (frequencies (map lower-case (. tokenizer tokenize line))))

(defn wc-file
  "Get tokenized word counts from a single file" 
  [tokenizer filename]
  (loop [lines (read-lines filename)
         counts (hash-map)]
    (if (empty? lines)
      counts
      (recur 
       (rest lines)
       (merge-with + counts (wc-line tokenizer (first lines)))))))

(defn wc-files
  "Get word counts over these files"
  [filenames]
  (let [tokenizer (new Tokenizer tokModel)]
    (loop [files filenames
           counts (hash-map)]
      (if (empty? files)
        counts
        (recur 
         (rest files)
         (merge-with + counts (wc-file tokenizer (first files))))))))


; Example usage
(def docfile "test.docs")
(let [filenames (read-lines docfile)
      P 10
      filegroups (k-way-partition filenames P)
      mcounts (reduce (partial merge-with +) (pmap wc-files filegroups))
      words (reverse (sort-by mcounts (keys mcounts)))]
  (write-lines "counts" 
               (map #(format "%s = %d" %1 (get mcounts %1)) words)))

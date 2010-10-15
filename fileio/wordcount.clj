(comment

  Get the word counts for a set of documents 

)

(ns posthoc.fileio.wordcount
  (:use [posthoc.fileio.tokenize :only (get-tokenizer file-to-toks)])
  (:use posthoc.util)
  (:use [clojure.contrib.duck-streams :only (read-lines file-str 
                                                        append-spit
                                                        write-lines)])
  (:use [clojure.contrib.string :only (split trim blank? lower-case)]))

(defn wc-file
  "Get word counts for a single file"
  [tkizer stop f]
  (frequencies (remove #(contains? stop %1)
                       (file-to-toks tkizer f))))

(defn fast-count-merge
  "ct1 must be a transient"
  [ct1 ct2]
  (loop [accum ct1
         newct (rest ct2)
         [k v] (first ct2)]
    (if (nil? k)
      accum
      (recur
       (assoc! accum k (+ (get accum k 0) v))
       (rest newct)
       (first newct)))))
  
(defn wc-filegroup
  "Get word counts over group of files"
  [stop fgroup]
  (loop [tkizer (get-tokenizer)
         counts (transient (hash-map))
         files fgroup]
    (if (empty? files)
      (persistent! counts)
      (recur
       tkizer
       (fast-count-merge counts (wc-file tkizer stop (first files)))
       (rest files)))))
           
(defn wc-files
  "Get word counts over all files"
  [files stop P]
  (reduce (partial merge-with +)
          (pmap
           (partial wc-filegroup stop)
           (k-way-partition files P))))

(comment

  Parse grouped-list text file, for example
  "Topic 0" followed by "foo = 0.385", etc 
  then "Topic 1"...

)

(ns posthoc.fileio.parse-topics
  (:use [clojure.contrib.duck-streams :only (read-lines)])
  (:use [posthoc.util]))

  
(defn process-group
  "Return hashmap of group idx --> sorted words"
  [group-re word-re headerline wordlines]
  (hash-map (nth-match group-re (first headerline) 1)
	    (filter not-nil? (map #(nth-match word-re %1 1)
				  wordlines))))
  
; !!! drop-while necessary to make this robust to leading newlines in file
(defn read-groups
  "Process lines into seq of (group header line, word lines) pairs"
  [group-re lines]
  (take-pairs (partition-by (partial re-matches group-re) 
                            (drop-while #(not (re-matches group-re %1))
                                              lines))))


;
; This is the externally called function
;
; Both group-re and word-re should have a string capturing group
; group-re must match only group labels
;
(defn parse-word-file
  "Return hash-map of group label --> item list"
  [filename group-re word-re]
  (let [lines (read-lines filename)
        pairs (read-groups group-re lines)
        processor #(process-group group-re word-re 
                                  (first %1) (second %1))]
    (apply merge (map processor pairs))))

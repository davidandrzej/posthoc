(comment  

  Evaluate sentence inclusion logic by finding all cases where
  includEE occurs in a sentence without the co-occurrence of the
  includER topic

  For each includEE topic, print non-co-occuring words sorted by count
)

(ns posthoc.check-incl
  (:use [clojure.contrib.duck-streams :only (read-lines file-str)])
  (:use [posthoc.util])
  (:use [posthoc.topicutil])
  (:use [posthoc.fileio.misc]))


; Default includer/ee topics
(def iner 6)
(def inee (set (range 6)))


; Test dataset
;; (def vocab (vector "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "X"))
;; (def w (list 0 1 2 3 4 5 6 7 8 9))
;; (def s (list 0 0 0 1 1 1 2 2 2 3))
;; (def z (list 0 1 0 2 2 2 0 2 1 5))
;; (def iner 0)
;; (def inee (set [1 2]))


; Represent each sentence as a hash-map of 
; includer/includee topic indices to words
;
(defstruct incl-sent :includerwords :includees)


(defn process-sentence
  "Find all occurrences of includer/includee topics in a sentence"
  [sentence]
  (let [wzs (map vector (map :word sentence) (map :topic sentence))
        includerwords (map first (filter #(== iner (second %1)) wzs))
        includeepairs (filter #(contains? inee (second %1)) wzs)
        includees (map #(hash-map (second %1) (list (first %1))) includeepairs)
        inclhash (apply merge-with concat includees)]
    (struct incl-sent includerwords inclhash)))
        

(defn incl-fail?
  "Does this incl-sent have topic *without* also having includER?"
  [isent topic]
  (let [hasincludee (contains? (:includees isent) topic)
        noincluder (empty? (:includerwords isent))]    
    (and hasincludee noincluder)))

(defn word-fail-counts
  [isentences topic]
  "Given all incl-sent, return violating includEE word counts"
  (let [sentfail (filter #(incl-fail? %1 topic) isentences)]
    (frequencies (apply concat (map #(get (:includees %1) topic) 
                                   sentfail)))))

(defn fail-report  
  "Print out how many times each word occurs without includER"
  [wordcts vocab]  
  (doseq [failword (reverse (sort-by #(get wordcts %1) (keys wordcts)))]
    (println (format "%d : %s"
                     (get wordcts failword) (nth vocab failword)))))
    

; 
; Externally called posthoc function
;
; For each includee topic, count non-co-occuring includee words
; and print sorted list
;
(defn incl-posthoc
  "Do sentence inclusion posthoc and output report files"
  [basefn]
  (let [vocab (read-words (format "%s.vocab" basefn))
        w (read-int-file (format "%s.words" basefn))
        s (read-int-file (format "%s.sent" basefn))
        z (read-int-file (format "%s.sample" basefn))        
        allidx (map (partial struct idx) s z w) ; All corpus indices     
        sentences (partition-by :sent allidx)
        isentences (map process-sentence sentences)]  
  (doseq [topic inee]
    (let [wordcts (word-fail-counts isentences topic)]
      (spit (format "%d.sentincl" topic)
            (with-out-str
              (println (format "Includee topic %d - %d total violations" 
                               topic (reduce + (vals wordcts))))
              (fail-report wordcts vocab)))))))


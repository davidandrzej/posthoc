(comment

  Evaluate sentence exclusion logic by finding all sentence-level
  co-occurrences of the excludER topic with each of the excludEE
  topics

  For each excludEE topic, print co-occurring word pairs sorted by count
)

(ns posthoc.check-excl
  (:use [clojure.contrib.combinatorics])
  (:use [clojure.contrib.duck-streams :only (read-lines file-str append-spit)])
  (:use [posthoc.util])
  (:use [posthoc.fileio.misc])
  (:use [posthoc.topicutil]))


; Default excluder/ee topics
(def exer 7)
(def exee (set (range 6)))


; Test dataset
;; (def w (list "a" "b" "a" "d" "e" "f" "g" "h" "i"))
;; (def s (list 0 0 0 1 1 1 2 2 2))
;; (def z (list 0 1 0 2 2 2 0 2 1))
;; (def exer 0)
;; (def exee (set [1 2]))


; A co-occurring pair of words
(defstruct copair :exerword :exeeword)


(defn add-pair
  "Add this word pair to our co-occur count"
  [counts newpair]
  (merge-with + counts (hash-map newpair 1)))

(defn add-exee-counts
  "Add newpair to the co-occur count for excludee topic"
  [allcounts newpair exeetopic]
  (assoc allcounts exeetopic 
	 (add-pair (get allcounts exeetopic) newpair)))

(defn record-co-occur
  "Record all sentence co-occurring (excluder, excludee) pairs"
  [allpairs vocab]
  (let [allcounts (hash-map)]
    (loop [pairs allpairs    ; Each pair of idx structs
	   counts allcounts] ; Total count hash exeetopic-->pairs-->counts
      (if (empty? pairs)
	counts ; We've processsed all pairs
	(let [curhit (first pairs)
	      exeetopic (:topic (second curhit))
	      newpair (struct copair
			      (nth vocab (:word (first curhit)))
			      (nth vocab (:word (second curhit))))]
	  (recur 
	   (rest pairs)
	   (add-exee-counts counts newpair exeetopic)))))))

(defn find-sent-co-occur
  "Within a sentence, find all (exluder, excludee) pairs"
  [sentence]
  (let [erhits (filter #(== exer (:topic %1)) sentence)
	eehits (filter #(contains? exee (:topic %1)) sentence)]
    (cartesian-product erhits eehits)))

(defn find-all-co-occur
  "Find all sentence-co-occurring (excluder, excludee) pairs"
  [allidx]
  (let [sentences (partition-by :sent allidx)]
    (apply concat (map find-sent-co-occur sentences))))

(defn get-sorted-co
  "Sorted pairs co-occur cts for one excludee topic"
  [coct]
  (sort-by #(get coct %1) (keys coct)))

(defn sorted-report
  "Print out co-occurring word pairs sorted by frequency" 
  [coct]
  (let [pairs (reverse (get-sorted-co coct))]
    (println (format "%d total" (reduce + (vals coct))))
    (doseq [pair pairs]
      (println (format "\t%d occur - %s" (get coct pair) pair)))))
		       



; 
; Externally called posthoc function
;
; For each excludee topic, count (excluder, excludee) sentence
; co-occurring word pairs and print sorted list
;
(defn excl-posthoc
  "Do sentence exclusion posthoc and output report files"
  [basefn]
  (let [vocab (read-words (format "%s.vocab" basefn))
        w (read-int-file (format "%s.words" basefn))
        s (read-int-file (format "%s.sent" basefn))
        z (read-int-file (format "%s.sample" basefn))        
        allidx (map (partial struct idx) s z w) ; All corpus indices
        allco (find-all-co-occur allidx) ; Get co-occurrences 
        cocts (record-co-occur allco vocab)] ; Map by excludee topic
    (doseq [exee (keys cocts)] ; Each excludee topic
      (spit (format "%d.sentco" exee) 
            (with-out-str
              (println (format "Excludee topic %d" exee))
              (sorted-report (get cocts exee)))))))

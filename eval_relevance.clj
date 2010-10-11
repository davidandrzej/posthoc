(comment

  Evaluate Top N topic words wrt relevance to target concept
  
)

(ns posthoc.eval-relevance
  (:use [posthoc.topicutil])
  (:use [posthoc.mean-avg-prec])
  (:use [posthoc.fileio.parse-topics])
  (:use [posthoc.fileio.parse-labels])
  (:use [posthoc.util]))                          

(defn relevance-report
  [concepts topics seeds irrelevant cidx]
  (let [conceptname (format "Concept %d" cidx)
        topicname (format "Topic %d" cidx)
        seedname (format "Seed %d" cidx)
        conceptwords (get concepts conceptname)
        topicwords (get topics topicname)
        seedwords (set (get seeds seedname))
        conceptpred #(contains? (set conceptwords) %1)
        topicnonseed (filter #(not (contains? seedwords %1)) topicwords)]
    (println conceptname)
    (println 
     (format "\tnum hits CONCEPT = %d"
             (reduce + (binary-relevance topicwords conceptpred))))
    (println (format "\tmean avg prec CONCEPT = %f"
                     (mean-avg-prec topicwords conceptpred)))
    (println 
     (format "\tnum hits CONCEPT (EXCLUDE SEED) = %d"
             (reduce + (binary-relevance topicnonseed conceptpred))))
    (println (format "\tmean avg prec CONCEPT (exclude SEED) = %f"
                     (mean-avg-prec topicnonseed conceptpred)))
    (println 
     (format "\tnum hits FAILURE = %d"
             (reduce + (binary-relevance topicwords 
                                         #(contains? irrelevant %1)))))
    (println (format "\tmean avg prec FAILURE = %f"
                     (mean-avg-prec topicwords
                                    #(contains? irrelevant %1))))
    (println)))
    


;; (let [topicfile (format "%s.topics" ename)
;;       alltopics (parse-word-file topicfile topic-re topicword-re)
;;       topics (select-keys alltopics (map #(format "Topic %d" %1) (range 6)))

;;       conceptfile "pos-labels.txt"
;;       concepts (parse-word-file conceptfile concept-re conceptword-re)

;;       failfile "neg-labels.txt"
;;       failures (parse-word-file failfile failure-re failword-re)

;;       seedfile "seedwords.txt"
;;       seeds (parse-word-file seedfile seed-re seedword-re)

;;       irrelevant (values-vecmap failures)
;;       ]
;;   (spit (format "%s.ranking" ename) 
;;         (with-out-str
;;           (doseq [cidx (range 6)]
;;               (relevance-report concepts topics seeds irrelevant cidx)))))
    
;  (doseq [conceptname (sort-by-re #"Concept (\d+)" (keys concepts))]			       
  ;; (doseq [[conceptname topicname] 
  ;; 	  (map list (sort-by-re #"Concept (\d+)" (keys concepts))
  ;; 	       (sort-by-re #"Topic (\d+)" (keys topics)))]

(comment

  Collect Top N concept-topic words from different runs
  
)

(ns posthoc.concept-words
  (:use [clojure.set :only (difference)])
  (:use [clojure.contrib.duck-streams :only (read-lines)])
  (:use [posthoc.mean-avg-prec])
  (:use [posthoc.fileio.parse-topics])
  (:use [posthoc.best-topics])
  (:use [posthoc.util])
  (:use [posthoc.topicutil]))
                          
(def stdbase "./datasets/standard-%d/hdg.topics")
(def zlabelbase "./datasets/zlabel-%d/hdg.topics")
(def inclbase "./datasets/incl-%d/hdg.topics")
(def exclbase "./datasets/excl-%d/hdg.topics")
(def allbase "./datasets/allsent-%d/hdg.topics")

(def seedfile "./labels/seedwords.txt")

(def conceptsfile "./labels/original_concepts.txt")


(defn seed-augment
  "Add SEEDWORD annotations"
  [seedwords word]
  (if (contains? seedwords word)
    (format "%s-SEEDWORD" word)
    word))

(let [concepts (vec (read-lines conceptsfile))

      seeds (parse-word-file seedfile seed-re seedword-re)

      stdfiles (map (partial format stdbase) (range 10))
      stdtopics (map #(parse-word-file %1 topic-re topicword-re) stdfiles)
      convertedstd (map #(convert-all-standard %1 seeds) stdtopics)

      zlabelfiles (map (partial format zlabelbase) (range 10))
      zlabeltopics (map #(parse-word-file %1 topic-re topicword-re) zlabelfiles)

      inclfiles (map (partial format inclbase) (range 10))
      incltopics (map #(parse-word-file %1 topic-re topicword-re) inclfiles)

      exclfiles (map (partial format exclbase) (range 10))
      excltopics (map #(parse-word-file %1 topic-re topicword-re) exclfiles)

      allfiles (map (partial format allbase) (range 10))
      alltopics (map #(parse-word-file %1 topic-re topicword-re) allfiles)]      

  (doseq [topic (range 6)] ; For each topic

    (let [topicname (format "Topic %d" topic)

          getter #(take 50 (get %1 topicname)) 

          allruns (flatten (list 
                            (map getter convertedstd)
                            (map getter zlabeltopics)
                            (map getter incltopics)
                            (map getter excltopics)
                            (map getter alltopics))) ; Pooling words over logic
          

          deduped (set allruns)
          seedset (apply hash-set (get seeds (format "Seed %d" topic)))
          annotated (map (partial seed-augment seedset) deduped)]
      (println topicname)
      (println (to-lines (map (partial seed-augment seedset)
			      (get (first convertedstd) topicname)))))))
;      (spit (format "%d.tolabel" topic)
;            (format "%s\nConcept seed words: %s\n\n%s" 
;                    topicname 
;                    (nth concepts topic)
;                    (to-lines (sort annotated)))))))


;standard (set (flatten (map getter convertedstd))) ; keep standard separate
  
; standard (set (flatten (map getter convertedstd))) ; keep standard separate      
; (spit (format "%d.standard-diff-25" topic)
; (print (to-lines (sort (difference standard deduped)))))))

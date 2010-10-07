(comment

  Evaluate Top N topic words wrt relevance to target concept
  
)

(ns posthoc.eval-labels
  (:use [clojure.set])
  (:use [clojure.contrib.duck-streams :only 
         (read-lines append-spit write-lines)])
  (:use [clojure.contrib.generic.math-functions :only (sqr)])
  (:use [posthoc.mean-avg-prec])
  (:use [posthoc.fileio.parse-topics])
  (:use [posthoc.fileio.parse-labels])
  (:use [posthoc.best-topics])
  (:use [posthoc.util]))

(defn mean [vals] (/ (float (reduce + vals)) (count vals)))
(defn variance [vals] 
  (let [m (mean vals)]
    (mean (map #(sqr (- %1 m)) vals))))

(def topic-re #"(Topic \d+).*")
(def topicword-re #"(\S+) = [\d\.]+")

(def stdbase "./datasets/standard-%d/hdg.topics")
(def zlabelbase "./datasets/zlabel-%d/hdg.topics")
(def inclbase "./datasets/incl-%d/hdg.topics")
(def exclbase "./datasets/excl-%d/hdg.topics")
(def allbase "./datasets/allsent-%d/hdg.topics")

; Seed word information
(def seed-re #"(Seed \d+).*")
(def seedword-re #"(\S+)")
(def seedfile "./labels/seedwords.txt")

; New labels
(def labelfile "./labels/%d.labelled.txt")

(defstruct topic-report 
  :acc 
  :acc-noseed 
  :map-pos 
  :map-neg)

(defn calc-acc
  [vals pospred]
  (/ (double (reduce + (binary-relevance vals pospred))) 
     (count vals)))

(defn make-topic-report
  "Do a full report of this topic vs these labels"
  [topic labels]
  (let [pospred #(or (contains? (set (get labels :pos)) %1)
                     (contains? (set (get labels :seed)) %1))
        negpred #(contains? (set (get labels :neg)) %1)
        seedpred #(contains? (set (get labels :seed)) %1)
        noseed (filter #(not (seedpred %1)) topic)]
    (struct-map topic-report
      :acc (calc-acc topic pospred)
      :acc-noseed (calc-acc noseed pospred)
      :map-pos (mean-avg-prec topic pospred)
      :map-neg (mean-avg-prec topic negpred))))


(defn eval-topic
  "Evaluate topic-concept agreement for a single topic"
  [topics label topicname]
  (hash-map topicname
            (list (make-topic-report (take 50 (get topics topicname)) label))))

(defn eval-run 
  "Evaluate topic-concept agreement for a single run"
  [topics labels]
  (apply merge (map #(eval-topic topics (nth labels %1) (format "Topic %d" %1))
                    (range 6))))

;
; Aggregate stats for topic-report struct
;
(defn report-vars
  [reports]
  (struct-map topic-report
    :acc (variance (map :acc reports))
    :acc-noseed (variance (map :acc-noseed reports))
    :map-pos (variance (map :map-pos reports))
    :map-neg (variance (map :map-neg reports))))

(defn report-means
  [reports]
  (struct-map topic-report
    :acc (mean (map :acc reports))
    :acc-noseed (mean (map :acc-noseed reports))
    :map-pos (mean (map :map-pos reports))
    :map-neg (mean (map :map-neg reports))))
                                                     
(defn eval-method
  "Evaluate topic-concept agreement over mult seedruns"
  [runs labels]
  (apply (partial merge-with concat) (map #(eval-run %1 labels) runs)))

(defn word-hits
  "How many times is this word in the Top 50?"
  [topics word]
  (count (filter #(contains? (set %1) word) topics)))


(defn precisions
  "Precisions at each cutoff"
  [ranked positive]
  (cons 1.0 (map #(/ (float (overlap (take %1 ranked) positive)) %1)
                 (range 1 (count ranked)))))

(defn recalls
  "Recalls at each cutoff"
  [ranked positive]
  (cons 0.0 (map #(/ (float (overlap (take %1 ranked) positive)) (count positive))
                 (range 1 (count ranked)))))
                                                            
(let [seeds (parse-word-file seedfile seed-re seedword-re)
      
      labels (map #(read-label-file (format labelfile %1)) (range 6))                  

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
      alltopics (map #(parse-word-file %1 topic-re topicword-re) allfiles)

      std-eval (eval-method convertedstd labels)
      zl-eval (eval-method zlabeltopics labels)
      in-eval (eval-method incltopics labels)
      ex-eval (eval-method excltopics labels)
      all-eval (eval-method alltopics labels)

      runno 1
      ptn (list 
           (list "std" (nth convertedstd runno))
           (list "zl" (nth zlabeltopics runno))
           (list "incl" (nth incltopics runno))
           (list "excl" (nth excltopics runno))
           (list "all" (nth alltopics runno)))]
  (doseq [topic (range 6)]
    (let [poswords (union (:pos (nth labels topic)) 
                          (:seed (nth labels topic)))]
      (println (format "Concept %d - %d pos words" topic (count poswords))))))
  ;
  ; for a SINGLE run of each method, print prec-rec values
  ;   
  ;; (doseq [[pname ptopic] ptn]
  ;;   (doseq [topic (range 6)]
  ;;     (let [topicname (format "Topic %d" topic)
  ;;           nposwords (union (:pos (nth labels topic)) 
  ;;                           (:seed (nth labels topic)))
  ;;           words (get ptopic topicname)]
  ;;       (write-lines (format "%s-%d.prec" pname topic)
  ;;                    (map str (precisions words poswords)))       
  ;;       (write-lines (format "%s-%d.rec" pname topic)
  ;;                    (map str (recalls words poswords)))))))

  ;
  ; Output all accuracies to text files for posthoc by R
  ;
  ;; (doseq [topic (sort-by-re #"Topic (\d+)" (keys std-eval))]
  ;;   (spit (format "%s.rtable" topic)
  ;;         (with-out-str
  ;;           (doseq [report (get std-eval topic)]
  ;;             (println (format "%f %d" (:acc report) 0)))
  ;;           (doseq [report (get zl-eval topic)]
  ;;             (println (format "%f %d" (:acc report) 1)))
  ;;           (doseq [report (get in-eval topic)]
  ;;             (println (format "%f %d" (:acc report) 2)))
  ;;           (doseq [report (get ex-eval topic)]
  ;;             (println (format "%f %d" (:acc report) 3)))
  ;;           (doseq [report (get all-eval topic)]
  ;;             (println (format "%f %d" (:acc report) 4)))))))

  ;
  ; for a SINGLE run of each method, print prec-rec values
  ;   
  ;; (doseq [[pname ptopic] ptn]
  ;;   (doseq [topic (range 6)]
  ;;     (let [topicname (format "Topic %d" topic)
  ;;           poswords (union (:pos (nth labels topic)) 
  ;;                           (:seed (nth labels topic)))
  ;;           words (get ptopic topicname)]
  ;;       (write-lines (format "%s-%d.prec" pname topic)
  ;;                    (map str (precisions words poswords)))       
  ;;       (write-lines (format "%s-%d.rec" pname topic)
  ;;                    (map str (recalls words poswords)))))))

       


;
; Print out all positive annotated words
;  
  ;; (doseq [topic (range 6)]
  ;;   (let [topicname (format "Topic %d" topic)
  ;;         poswords (:pos (nth labels topic))]
  ;;     (println topicname)
  ;;     (println (apply str (interpose "\n" poswords)))
  ;;     (println))))
                                     
  ;                                   
  ; How ofen do approaches put each :pos word in Top 50?
  ;

  ;; (doseq [topic (range 6)]
  ;;   (let [topicname (format "Topic %d" topic)
  ;;         poswords (:pos (nth labels topic))]
  ;;     (println (format "\n\n%s\n" topicname))
  ;;     (doseq [posword poswords]
  ;;       (println posword)
  ;;       (println (format "\tStd = %d hits" 
  ;;                        (word-hits (map #(get %1 topicname) convertedstd) posword)))
  ;;       (println (format "\tZL = %d hits" 
  ;;                        (word-hits (map #(get %1 topicname) zlabeltopics) posword)))
  ;;       (println (format "\tIncl = %d hits" 
  ;;                        (word-hits (map #(get %1 topicname) incltopics) posword)))
  ;;       (println (format "\tExcl = %d hits" 
  ;;                        (word-hits (map #(get %1 topicname) excltopics) posword)))
  ;;       (println (format "\tAll = %d hits" 
  ;;                        (word-hits (map #(get %1 topicname) alltopics) posword)))
  ;;       ))))




;
; for each concept, count how often each :pos word is found by each method
;


;
; Aggregate accuracies, etc over 10 runs for diff methods
;

  ;; (doseq [topic (sort-by-re #"Topic (\d+)" (keys std-eval))]
  ;;   (println "\n")
  ;;   (println topic)    
  ;;   (println "Standard")    
  ;;   (println (report-means (get std-eval topic)))
  ;;   (println (report-vars (get std-eval topic)))

  ;;   (println "z-label")                   
  ;;   (println (report-means (get zl-eval topic)))
  ;;   (println (report-vars (get zl-eval topic)))

  ;;   (println "incl")            
  ;;   (println (report-means (get in-eval topic)))
  ;;   (println (report-vars (get in-eval topic)))

  ;;   (println "excl")            
  ;;   (println (report-means (get ex-eval topic)))
  ;;   (println (report-vars (get ex-eval topic)))

  ;;   (println "all")             
  ;;   (println (report-means (get all-eval topic)))
  ;;   (println (report-vars (get all-eval topic)))))

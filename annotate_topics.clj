(comment

  Annotate topic words - eg, are they positively labeled words?
)

(ns posthoc.annotate-topics
  (:use [posthoc.mean-avg-prec])
  (:use [posthoc.fileio.parse-topics])
  (:use [posthoc.util]))

(def topic-re #"(Topic \d+).*")
(def topicword-re #"(\S+) = [\d\.]+")

(def failure-re #"(Failure \d+).*")
(def failword-re #"(\S+)")

(def concept-re #"(Concept \d+).*")
(def conceptword-re #"(\S+)")

(def seed-re #"(Seed \d+).*")
(def seedword-re #"(\S+)")
    
(defn annotated-word
  "Return str of word along with annotations"
  [annotation word]
  (apply str word " - " (interpose ", " (get annotation word '("????")))))

(defn annotated-topic
  "Return str of annotated topic words" 
  [topicwords annotation]
  (to-lines (map #(annotated-word annotation %1) topicwords)))
		 
                                  
(let [topicfile "hdg.topics"
      alltopics (parse-word-file topicfile topic-re topicword-re)
      topics (select-keys alltopics (map #(format "Topic %d" %1) (range 6)))

      conceptfile "pos-labels.txt"
      concepts (parse-word-file conceptfile concept-re conceptword-re)

      failfile "neg-labels.txt"
      failures (parse-word-file failfile failure-re failword-re)

      seedfile "seedwords.txt"
      seeds (parse-word-file seedfile seed-re seedword-re)

      wordhashes (list concepts failures seeds)
      annotations (apply merge-with concat (map invert-vecmap wordhashes))]
  (doseq [topic (keys topics)]
    (println topic)
    (print (annotated-topic (get topics topic) annotations))))

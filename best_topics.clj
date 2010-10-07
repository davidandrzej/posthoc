(ns posthoc.best-topics
  (:use [posthoc.util]))

(defn get-best-topic
  "Find the topic containing the most seedwords"
  [topics seedwords]
  (apply max-key #(overlap (set seedwords) (set (get topics %1)))
         (keys topics)))

(defn convert-topic
  "Find topic with most seedword overlap, return hash newname-->topicwords"
  [topics seedwords newname]
  (hash-map newname (get topics (get-best-topic topics seedwords))))

(defn convert-all-standard
  "For each seed concept rename highest-overlap topics to Topic 0...5"
  [topics seedwords]
  (apply merge (for [idx (range 6)]
                 (convert-topic topics 
                                (get seedwords (format "Seed %d" idx))
                                (format "Topic %d" idx)))))

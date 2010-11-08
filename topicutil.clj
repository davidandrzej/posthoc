(comment

  Topic model functions/etc

)

(ns posthoc.topicutil)

; Represents info associated w/ a single corpus index 
(defrecord idx [sent topic word doc])

; Regular expressions for parsing out topic text files
(def topic-re #"(Topic \d+).*")
(def topicword-re #"(\S+) = [\d\.]+")

(def seed-re #"(Seed \d+).*")
(def seedword-re #"(\S+)")

(comment

  Topic model functions/etc

)

(ns posthoc.topicutil)

; Represents a single corpus index
(defstruct idx :sent :topic :word :doc)

; Regular expressions for parsing out topic text files
(def topic-re #"(Topic \d+).*")
(def topicword-re #"(\S+) = [\d\.]+")

(def seed-re #"(Seed \d+).*")
(def seedword-re #"(\S+)")

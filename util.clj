(comment

Very general convenience/utility functions

)

(ns posthoc.util
  (:use [clojure.set :only (intersection)])
  (:use [clojure.contrib.seq-utils :only (indexed)])
  (:use [clojure.contrib.string :only (blank?)]))

;
; Nice to have these not-X? functions
;
(defn not-nil?
  [q]
  (not (nil? q)))

(defn not-blank?
  [s]
  (not (blank? s)))

(defn not-zero?
  [val]
  (not (zero? val)))

;
; We often have hash-map where values are lists (listmaps)
;
(defn invert-entry
  "Helper function for invert-listmap"
  [lst k]
  (apply merge (for [item lst] (hash-map item (list k)))))

(defn invert-listmap
  "Given map x-->list of y, return map y-->list of x (s.t. y in x's list)"
  [listmap]
  (apply merge-with concat (map #(invert-entry (get listmap %1) %1)
                                (keys listmap))))

(defn values-listmap
  "Given map x-->list of y, return hash-set of all y"
  [listmap]
  (set (apply concat (vals listmap))))

(defn flip-singleton-hash
  "Given singleton hash key->val, return val->(key)"
  [h]
  (let [keyct (count (keys h))
        okval (assert (= keyct 1))
        k (first (keys h))]
    (hash-map (get h k) (list k))))

(defn construct-listmap
  "Given seq of singleton word-->label hashmaps, return label-->words listmap"
  [hashes]
  (apply merge-with concat (map flip-singleton-hash hashes)))
                                       
(defn str-listmap-keyval
  "Convert a single key-list pair to a newline-separated string"
  [listmap k]
  (apply str (interpose "\n" (cons k (get listmap k)))))

(defn str-listmap
  "Convert listmap key-list pairs to nice newline-separated string"
  ([listmap] ; if no keys supplied, do all keys
     (str-listmap listmap (keys listmap)))
  ([listmap ks] ; use supplied keys and ordering
     (apply str (interpose "\n\n"  (map (partial str-listmap-keyval listmap)
                                        ks)))))


;
; Regex helpers
;
(defn nth-match
  "Return the nth regex match"
  [re s n]
  (nth (re-matches re s) n))

(defn sort-by-re
  "Sort strings like Topic 0, Topic 2, etc by index"
  [re strings]
  (sort-by #(Integer/parseInt (nth-match re %1 1)) strings))

;
; Is there a standard API way to do this?
;
(defn take-pairs
  "Given a seq, return lazy seq of adjacent (non-overlapping) pairs"
  [vals]
  (if (empty? vals)
    '()
    (cons (apply list (take 2 vals))
	  (lazy-seq (take-pairs (drop 2 vals))))))

;
; Find value sequences in Java arrays
;      
(defn ngram-match
  "Does jarray[start:] match ngram?"
  [jarray ngram start]
  (every? identity (for [[idx,val] (indexed ngram)]
                     (== (aget jarray (+ start idx)) val))))
                    
(defn ngram-count
  "Count occurrences of ngram (int seq) in jarray (int[])" 
  [jarray ngram]
  (count (filter (partial ngram-match jarray ngram) 
                 (range (- (alength jarray) (- (count ngram) 1))))))

      
;
; Misc patterns that get used a lot in my code...
;
(defn to-lines
  [lst]
  (apply str (interpose "\n" lst)))

(defn overlap
  "Count number of overlapping elements"
  [s1 s2]
  (count (intersection (set s1) (set s2))))

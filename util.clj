(comment

General convenience/utility functions

)

(ns posthoc.util
  (:use [clojure.contrib.math :only (ceil)])
  (:use [clojure.set :only (intersection)])
  (:use [clojure.contrib.seq-utils :only (indexed)])
  (:use [clojure.contrib.string :only (blank?)]))

;
; Misc patterns that get used a lot...
;

(defn os-path-split
  "Mimic Python's os.path.split()[1]" 
  [f]
  (. (. f getCanonicalPath) substring
     (inc (-> f .getParentFile .getCanonicalPath .length))))

(defn to-spaced-line
  [lst]
  (apply str (interpose " " lst)))

(defn to-lines
  ([lst]
     (apply str (interpose "\n" lst)))
  ([lst & opt]
     (apply str (interpose "\n" (cons lst opt)))))

(defn overlap
  "Count number of overlapping elements"
  [s1 s2]
  (count (intersection (set s1) (set s2))))

(defn k-way-partition
  "Return a (more or less) even k-way partition of vals"
  [vals k]
  (partition-all (ceil (/ (count vals) k)) vals))

(defn str-map
  "To pretty print hash-map contents"
  [m]
  (to-lines (map #(format "%s: %s" (str %1) (str (m %1)))
                 (keys m))))
(defn print-map
  "Actually pretty-print a hash-map"
  [m]
  (println (str-map m)))
                                   
;
; Often have hash-map where keys=items vals=counts
;
(defn mfilter 
  "Do filter on a hash-map by values"
  [p m]
  (into (empty m) (filter #(-> %1 (second) (p)) m)))

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
; Fcns for working w/ hash-map where vals are vectors (vecmaps)
;
(defn invert-entry
  "Helper function for invert-vecmap"
  [v k]
  (reduce merge (for [item v] (hash-map item (vector k)))))

(defn invert-vecmap
  "Given map x-->vec of y, return map y-->vec of x (s.t. y in x's vec)"
  [vecmap]
  (apply merge-with concat (map #(invert-entry (get vecmap %1) %1)
                                (keys vecmap))))

(defn values-vecmap
  "Given map x-->vec of y, return hash-set of all y"
  [vecmap]
  (set (apply concat (vals vecmap))))

(defn flip-singleton-hash
  "Given singleton hash key->val, return val->(key)"
  [h]
  (let [keyct (count (keys h))
        okval (assert (= keyct 1))
        k (first (keys h))]
    (hash-map (get h k) (vector k))))

(defn construct-vecmap
  "Given seq of singleton word-->label hashmaps, return label-->words vecmap"
  [hashes]
  (apply merge-with concat (map flip-singleton-hash hashes)))
                                       
(defn str-vecmap-keyval
  "Convert a single key-vec pair to a newline-separated string"
  [vecmap k]
  (to-lines (cons k (get vecmap k))))

(defn str-vecmap
  "Convert vecmap key-vec pairs to nice newline-separated string"
  ([vecmap] ; if no keys supplied, do all keys
     (str-vecmap vecmap (keys vecmap)))
  ([vecmap ks] ; use supplied keys and ordering
     (apply str (interpose "\n\n"  (map (partial str-vecmap-keyval vecmap)
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

      

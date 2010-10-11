(comment

  Parse concept word labels

)

(ns posthoc.fileio.parse-labels
  (:use [clojure.contrib.duck-streams :only (read-lines)])
  (:use [clojure.contrib.string :only (split trim blank?)])
  (:use [posthoc.util]))

; 3 cases:
; word 
; word y
; word (SEEDWORD) y 
; 
(defn onehit [hits] :neg)
(defn twohit 
  "Double-check that it really matches"
  [hits] 
  (let [ispos (= (nth hits 1) "y")
        isseed (= (nth hits 1) "(SEEDWORD)")]
  (if ispos
    :pos
    (if isseed
      :seed      
      (assert nil)))))

(defn threehit
  "Double-check that it really matches"
  [hits]
  (let [testval (and (= (nth hits 1) "(SEEDWORD)")
                     (= (nth hits 2) "y"))]
    (if testval
      :seed
      (assert testval))))

; 'table' of label generating functions
(def labeltable [onehit twohit threehit])
     

(defn get-word-label
  "Parse a single labeling"
  [line]
  (let [sline (split #"\s+" line)
        labelfn (nth labeltable (- (count sline) 1))]    
    (hash-map (first sline) 
              (labelfn sline))))

(defn read-label-file
  "First 3 lines just say what concept is, etc"
  [labelfn]
  (let [lines (filter not-blank? (drop 3 (read-lines labelfn)))]    
    (construct-vecmap (map get-word-label lines))))

; TESTCODEE
;  
;(let [labelfn "labels/0.labelled.txt"
;      labels (read-label-file labelfn)]  
;  (println (reduce + (map count (vals labels)))))


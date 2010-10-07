(comment

  Parse and manipulate Stanford-style dependency parses from C&C
  http://svn.ask.it.usyd.edu.au/trac/candc/wiki/Download

)

(ns posthoc.dependency
  :use '[clojure.contrib.duck-streams :only (read-lines)])

; Used for extracting dependency entries from parsing script output
(def dep-re #"^\((\w+) (\w+)_(\d+) (\w+)_(\d+)\)")
(defstruct dependency 
  :dep-type 
  :gov-word :gov-idx
  :dep-word :dep-idx)


; Dependency graph representation structures
(defstruct dep-graph :nodes :edges)
(defstruct tok-node :word :idx)
(defstruct dep-edge :dep-type :governor :dependent)

;
; Reading the dependencies file
;
(defn not-nil? 
  "Could not find this in core API...?!"
  [val]
  (not (nil? val)))
  
(defn match-to-dep
  "Convert a regex match to a dependency struct"
  [depmatch]
  (let [[fullmatch dtype gword gidx dword didx] depmatch]
    (struct dependency dtype 
	    gword (Integer/parseInt gidx)
	    dword (Integer/parseInt didx))))

(defn get-dep-matches
  "Get match objects for dependencies in from text lines"
  [lines]
  (filter not-nil? (map (partial re-matches dep-re) lines)))

(defn parse-dep-file
  "Parse dependencies entries from file"
  [filename] 
  (map match-to-dep (get-dep-matches (read-lines filename))))

;
; Constructing the dependency graph
;
(defn add-edge
  "Add new edge to our adjacency lists " 
  [edges newedge]
  (let [gov-idx (:governor newedge)
	dep-idx (:dependent newedge)]
    (assoc edges 
      gov-idx (cons newedge (get edges gov-idx '()))
      dep-idx (cons newedge (get edges dep-idx '())))))
	   
(defn build-dep-graph
  "Construct dependency graph from list of dependency entries"
  [alldeps]
  (loop [deps alldeps
	 nodes (hash-map)  ; idx --> tok-node 
	 edges (hash-map)] ; idx --> adjacency list of dep-edge
    (let [current (first deps)
	  newdep (struct tok-node (:dep-word current) (:dep-idx current)) 
	  newgov (struct tok-node (:gov-word current) (:gov-idx current))
	  newnodes (assoc nodes (:idx newdep) newdep (:idx newgov) newgov)
	  newedge (struct dep-edge (:dep-type current) 
			  (:idx newgov) (:idx newdep))
	  newedges (add-edge edges newedge)]
      (if (zero? (count (rest deps)))
	(struct dep-graph newnodes newedges)
	(recur (rest deps)
	       newnodes
	       newedges)))))

;
; Manipulate the dependency graph
;
(defn dump-graph
  "Print out raw dump of nodes and edges"
  [graph]
  (let [nodes (:nodes graph)
	edges (:edges graph)]
    (println (interpose "\n" (map (partial get nodes) (keys nodes))))
    (println)
    (println (interpose "\n" (map #(format "%d: %s" %1 (str (get edges %1)))
				  (keys edges))))))

(defn get-dep-roots
  "Get idx of tokens which have no governor"
  [dep-graph]
  (let [edges (:edges dep-graph)]
    (filter #(and (== (count (get edges %1)) 1)
		  (== (:governor (first (get edges %1))) %1))	    
	    (keys edges))))

(defn get-dependents
  "Get idx of tokens which are dependents of this token"
  [dep-graph idx]
  (let [edges (get (:edges dep-graph) idx)]
    (map :dependent (filter #(== (:governor %1) idx) edges))))

(defn tok-to-str
  "Convert a token node to string"
  [tok]
  (format "%s_%d" (:word tok) (:idx tok)))


(defn to-nested-str
  "Convert dependency graph to nested string representation "
  [dep-graph idx]
  (let [tnode (get (:nodes dep-graph) idx)]
    (cons (tok-to-str (get (:nodes dep-graph) idx))
          (map (partial to-nested-str dep-graph)
               (get-dependents dep-graph idx)))))

;; (defn print-nested
;;   "Convert nested seq of strings to a flat list with tab indentation"
;;   [strseq]
;;   (let [len (count strseq)]
;;     (println (str (first strseq)))
;;     (if (empty? strseq)
;;       '(); base case
;;     (if (string? (first strseq))
;;       (cons (first strseq) (print-nested (rest strseq)))
;;       (conj (map (partial format "\t%s") (print-nested (first strseq)))
;; 	    (print-nested (rest strseq)))))))
  
;; (defn nested-rep
;;   "Output dependency graph with indentation"
;;   [dep-graph]
;;   (map (partial node-report dep-graph)
;;        (get-dep-roots dep-graph)))

(let [depfile "./example.dep"
      dep-entries (parse-dep-file depfile)
      dep-graph (build-dep-graph dep-entries)
      roots (get-dep-roots dep-graph)]
  (dump-graph dep-graph)
  (println (format "\nroot idx are: %s" (seq (get-dep-roots dep-graph))))
  (println)
  (doseq [root roots]
    (println (to-nested-str dep-graph root))))
;  (println)
;  (println (interpose "\n" (print-nested (first (nested-rep dep-graph))))))

  

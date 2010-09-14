(ns type-level-tagger
  {:doc "Implements State-of-the-art Unsupervised Part-of-speech Tagger 
        from \"Simple Type-Level Unsuperivsed POS Tagging\"
        by Yoong-Keok Lee, Aria Haghighi and Regina Barzilay
        (http://www.cs.berkeley.edu/~aria42/pubs/typetagging.pdf)"
  :author "Aria Haghighi (aria42@gmail.com)"}
  (:use [clojure.java.io :only [reader]]
        [clojure.contrib.duck-streams :only [with-out-writer]]
        [clojure.contrib.seq-utils :only [indexed]]
        [clojure.contrib.def :only [defvar]]))  
  
;; Counter: Map from object to value, cache total
(defrecord Counter [counts total])

(defn get-count 
  "retrieve count of k from counter, should not be negative"
  [counter k]                                              
  {:post [(not (neg? %))]}
  (get (:counts counter) k 0.0))  

(defn inc-count 
  "increment-count of k in counter by weight amount"
  [counter k weight]  
  (let [new-count (+ (get-count counter k) weight)]
    (Counter. (if (zero? new-count)
                    (dissoc (:counts counter) k)
                    (assoc (:counts counter) k new-count))
              (+ (:total counter) weight))))  
                
;; Probability Distribution
;; counter: counts of objects
;; lambda: smoothing constants
;; num-keys: number of possible keys, needed to normalize
(defrecord DirichletMultinomial [counter lambda num-keys])

(defn new-dirichlet [lambda num-keys]
  (DirichletMultinomial. (Counter. {} 0) lambda num-keys))

(defn log-prob 
  "log prob. from a DirichletMultinomial"  
  [distr key]                                                
  {:post [(> % Double/NEGATIVE_INFINITY),(neg? %)]}
  (let [{:keys [counter lambda num-keys]} distr]        
    (Math/log (/ (+ (get-count counter key) lambda) 
                 (+ (:total counter) (* lambda num-keys))))))

(defn observe 
 "make an observation to a DirichletMultinomial"
 [distr key weight]
 (let [{:keys [counter,total,lambda,num-keys]} distr]  
  (DirichletMultinomial. (inc-count counter key weight) lambda num-keys)))
    
; Word Information
; word: string of word
; count: # of usages
; feats: map of feature-type to feature-value
; contexts: counter of [before-word after-word] usages (for HMM)
(defrecord WordInfo [word count feats contexts])

(defn get-feats 
  "Features on a word type"
  [w]
  {:hasInitCap (boolean (re-matches #"[A-Z].*" w))    
   :hasPunc (boolean (re-matches #".*\W.*" w))
   :suffix  (let [suffix-length (min 3 (.length w))]
              (.substring #^String w (- (.length w) suffix-length)))})              

(defn new-word-info [word]
  (WordInfo. word 0 (get-feats word) (Counter. {} 0)))

(defn tally-usage [word-info before after]
  (-> word-info
      (update-in [:count] inc)
      (update-in [:contexts] inc-count [before after] 1)))      

(defn assoc-if-absent [m k f]
  (if (m k) m (assoc m k (f k))))

(defn tally-sent [vocab sent]
  (reduce
    (fn [res [before word after]]
      (-> res
          (assoc-if-absent word new-word-info)          
          (update-in [word] tally-usage before after)))
    vocab
    (partition 3 1 sent)))

(defn build-vocab [sents]
  (vals (reduce tally-sent {} sents)))            

;; Gibbs Sampling State - All distributions are DirichletMultinomial
;; type-assigns: map word string to tag state (integer)
;; tag-prior: prior distr over tag assignment
;; trans-distrs: map of tag => P(tag' | tag) distribution
;; emission-distrs: tag => P(word | tag) distribution, word=string representation
;; feat-distrs: tag => feat-type => P(feat-val | feat-type,tag) distribution
(defrecord State [type-assigns tag-prior trans-distrs emission-distrs feat-distrs])

;; Globals
(defvar *K* nil "number of tag states")
(defvar *vocab* nil "seq of word infos")
(defvar *outfile* nil "where to write each iteration word assignments")
(def +rand+ (java.util.Random. 0))

;; Updating Counts after word assignment
(defn obs-transitions 
  "if we set word to tag, we update the transition tag counts
  from all context usages of word by weight amount"
  [trans-distrs type-assigns word-info tag weight] 
  (reduce
    (fn [res [[before after] count]]
      (let [type-assigns (assoc type-assigns (:word word-info) tag)
            before-tag (type-assigns before)
            after-tag (type-assigns after)]          
         (-> res
                ;; Observe P(tag | tag-assign(before))
                (update-in [before-tag] observe tag (* count weight))
                ;; Observe P(tag-assign(after) | tag)
                (update-in [tag] observe after-tag (* count weight)))))
    trans-distrs      
    (-> word-info :contexts :counts)))

(defn obs-features [tag-feat-distrs word-info weight]
  (reduce
    (fn [res [k v]]
      (update-in res [k] observe v weight))
    tag-feat-distrs
    (:feats word-info)))
    
(defn obs-emissions 
  "if a word has been assigned to a tag, we increment num-keys by 1
   and add weight * num-occurences of the word to counts"
  [tag-emission-distr word-info weight]
  (-> tag-emission-distr
      (update-in [:num-keys] (if (> weight 0) inc dec))
      (observe (:word word-info) (* weight (:count word-info)))))
        
(defn update-state
  "add word assignment and associated counts"
  [state word-info tag add?]
  (let [assoc-fn (if add? assoc dissoc) weight (if add? 1 -1)]
   (State.
    (assoc-fn (:type-assigns state) (:word word-info) tag)
    (observe (:tag-prior state) tag weight)
    (obs-transitions (:trans-distrs state) (:type-assigns state) word-info tag weight)    
    (update-in (:emission-distrs state) [tag]
      obs-emissions word-info weight)
    (update-in (:feat-distrs state) [tag]
      obs-features word-info weight))))

(defn assign [state word-info tag]
  (update-state state word-info tag true))      
  
(defn unassign [state word-info]
  (update-state state word-info (-> state :type-assigns (get (:word word-info))) false))  
              
(defn sum
 ([f xs] (reduce + (map f xs)))
 ([xs] 
   (reduce + 0.0 xs)))

(defn make-map [f xs]
    (reduce
      (fn [res x] (assoc res x (f x)))
      {} xs))
      
(defn map-vals [f m]
  (reduce
    (fn [res [k v]] (assoc res k (f v)))
    {} m))      

(defn log-add 
 "log (sum xs) from seq of log-x"
 [log-xs]
 (let [max-log-x (apply max log-xs)]
   (+ max-log-x
      (Math/log (sum          
          (for [log-x log-xs 
                 :let [diff (- log-x max-log-x)]
                 :when (> diff -30)]
             (Math/exp diff)))))))

(defn log-normalize [log-xs]
 (let [log-sum (log-add log-xs)]
   (map (fn [log-x] (Math/exp (- log-x log-sum))) log-xs)))
   
(defn sample-from-scores [log-scores]
 (let [trg (.nextDouble +rand+)]
  (loop [so-far 0.0
         posts (indexed (log-normalize log-scores))]
   (if-let [[i p] (first posts)]
     (cond
       (< trg (+ so-far p)) i
       :default (recur (+ so-far p) (rest posts)))
     (throw (RuntimeException. "Impossible"))))))

(defn score-assign 
  "Log probability of assigning word to tag"
  [state word-info tag]
  (+ ;; Tag Prior
     (log-prob (:tag-prior state)  tag)
     ;; Feature Prob
     (sum
       (fn t1 [[k v]]
         (log-prob (get-in state [:feat-distrs tag k]) v))
       (:feats word-info))
     ;; Token Transition/Emission Prob
     ;; There's a subtely here in that we need to add one to the num-keys
     ;; for the emission distribution
     (let [type-assigns (-> state :type-assigns (assoc (:word word-info) tag))
           word-log-prob
             (-> (:emission-distrs state)
                 (get tag)
                 (update-in [:num-keys] inc)
                 (log-prob (:word word-info)))]           
        (sum
            (fn t2 [[[before after] count]]
              (let [before-tag (type-assigns before) after-tag (type-assigns after)]                    
                (* count
                   (+ word-log-prob
                      (-> state :trans-distrs (get before-tag) (log-prob tag))
                      (-> state :trans-distrs (get tag) (log-prob after-tag))))))
            (-> word-info :contexts :counts)))))

(defn gibbs-sample [state word-info]
  (let [state  (unassign state word-info)
        scores (map (partial score-assign state word-info) (range *K*))
        sample-tag (sample-from-scores scores)]
    (assign state word-info sample-tag)))
      
(defn gibbs-sample-iter [state]
  (time (reduce
          (fn [res word-info] (gibbs-sample res word-info))
          state
          *vocab*)))    

(defn init-state-helper [alpha beta]
  (let [num-distinct (fn [xs] (count (reduce conj (hash-set) xs)))
        num-feat-map ; map feature-type to num possible values
          (->> *vocab*
               (mapcat :feats)
               (group-by first) ; group by feature type
               (map-vals num-distinct))]
    (State.
      ; random word to tag assignment - also fix assignments to start/stop
      (let [rand-assign (make-map (fn [_] (.nextInt +rand+ *K*)) (map :word *vocab*))]
        (assoc rand-assign "#start#" :start "#stop#" :stop))
      ; tag prior
      (new-dirichlet alpha *K*)
      ; transition distributions: all tags have same prior on successors
      ; K+1 possible values for tags and :stop state
      ; Also need a transition distribution for :start state
      (make-map 
        (constantly (new-dirichlet beta (inc *K*)))
        (conj (range *K*) :start))          
      ; emission distributions: all tags have same prior
      (make-map
        (constantly (new-dirichlet beta 0))
        (range *K*))
      ; feat distributions
      (let [tag-feat-distrs (map-vals (partial new-dirichlet alpha) num-feat-map)]
       (make-map (constantly tag-feat-distrs) (range *K*))))))

(defn init-state [alpha beta]
  (reduce
    (fn [res word-info]
      (assign res word-info (-> res :type-assigns (get (:word word-info)))))
    (init-state-helper alpha beta)
    *vocab*))
   
    
(defn learn [num-iters alpha beta]
   (println "Learning on " num-iters " iterations.")      
   (println (format "Num Word Types: %d Num Tokens: %d" (count *vocab*) (sum :count *vocab*)))
   (loop [iter 1 state (init-state alpha beta)]      
    (with-out-writer *outfile*
      (doseq [[word tag] (:type-assigns state) :when (not ((hash-set :start :stop) tag))]
        (println (format "%s\t%s" word tag))))
    (println (format "Finished Iteration %d %s Wrote current assignment to %s" 
                iter (if (= iter 1) "(Random)" "") *outfile*))    
    (if (= iter num-iters) state      
        (recur (inc iter) (gibbs-sample-iter state)))))     

(defn -main 
  "Main entry run with infile outfile num-iters K alpha beta
   infile: file with one sentence per-line tokenized so split on space gives tokens (no start/stop)
   outfile: file to write word to tag mapping after each iteration
   num-iters: number of gibbs sampling iters to run
   K: number of tags to use
   alpha,beta: doubles representing smoothing (try 0.1 1)"
  [& args]
  (let [[infile outfile num-iters K alpha beta] args        
        sents-fn #(map
                   (fn [line] (concat ["#start#"] (seq (.split #^String line "\\s+")) ["#stop#"]))
                   (-> infile reader line-seq))]                          
    (binding [*K* (Integer/parseInt K) *vocab* (build-vocab (sents-fn)) *outfile* outfile]
       (learn (Integer/parseInt num-iters) (Double/parseDouble alpha) (Double/parseDouble beta)))))

(apply -main *command-line-args*)
(ns genga.ga
  (:use [genga.util :only [weighted-rand-nth]]))

(declare run-gen select-mates crossover mutate)

(def selection-types [:weight :tournament])
(def crossover-types [:uniform :point])
(def mutation-types  [:uniform :agent])

;; The agent creator must write and bind these functions
(def ^:dynamic *rand-agent-fn*)
(def ^:dynamic *fitness-fn*)

;; The agent creator must write and bind one of the following functions
;; If uniform mutation is used, rand-allele-fn must be bound
;; If agent-specific mutation is use, agent-mutation-fn must be bound
(def ^:dynamic *rand-allele-fn*)
(def ^:dynamic *agent-mutation-fn*)

;; These default settings will work in most cases and need not be rebound.
;; However, different settings may cause the algorithm to reach good solutions
;; dramatically faster depending on the nature of the agent and its genes.
(def ^:dynamic *population-count* 100)
(def ^:dynamic *selection-type* :weight)
(def ^:dynamic *crossover-type* :uniform)
(def ^:dynamic *mutation-type* :uniform)
(def ^:dynamic *agent-mutation-chance* 0.20)
(def ^:dynamic *allele-mutation-chance* 0.10) ; Only used with uniform mutation

;; Top-level fns
(defn run-ga 
  "Generates a random starting population. Returns a lazy infinite seq of 
  of increasingly fit generations."
  []
  (iterate run-gen (repeatedly *population-count* *rand-agent-fn*)))

(defn run-gen 
  "Given a generation of agents, returns the next by performing ranking, 
  mate selection, crossover, and mutation."
  [agents]
  (map (comp mutate crossover) (select-mates agents (map *fitness-fn* agents))))

;; Selections fns
(defn select-by-weight [agents scores]
  (weighted-rand-nth agents scores))

(defn select-by-tournament [agents scores]
  (let [pool (repeatedly 2 #(rand-nth (map vector agents scores)))]
    (first (apply max-key second pool))))

(defn select-agent 
  "Given a collection of agents and a collection of corresponding agent fitness
  scores, return a random agent with higher-score agents returned more often."
  [agents scores]
  (({:weight     select-by-weight
     :tournament select-by-tournament} *selection-type*)
   agents scores))

(defn select-mates 
  "Given a collection of agents and a collection of corresponding agent fitness
  scores, return pairs to mate with higher-score agents appearing more often."
  [agents scores]
  (letfn [(make-pair []
            (take 2 (distinct (repeatedly #(select-agent agents scores)))))]
    (take (count agents) (repeatedly make-pair))))

;; Crossover fns
(defn uniform-crossover [[a1 a2]]
  (map #(rand-nth [%1 %2]) a1 a2))

(defn point-crossover [[a1 a2]]
  (let [point (rand-int (count a1))]
    (concat (take point a1) (drop point a2))))

(defn crossover [pair]
  (({:point   point-crossover
     :uniform uniform-crossover} *crossover-type*)
   pair))

;; Mutation fns
(defn uniform-mutation [a]
  (map #(if (< (rand) *allele-mutation-chance*) (*rand-allele-fn*) %) a))

(defn mutate [a]
  (if (> (rand) *agent-mutation-chance*)
    a
    (({:uniform uniform-mutation
       :agent   *agent-mutation-fn*} *mutation-type*) a)))


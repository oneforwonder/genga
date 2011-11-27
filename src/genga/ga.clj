(ns genga.ga
  (:use (util :only [weighted-rand-nth])))

(declare run-gen select-mates reproduce mutate)

(def selection-types [:weight :tournament])
(def crossover-types [:uniform :one-point :two-point])
(def mutation-types [:uniform :point :agent :mixed])
(def ^:dynamic *selection-type* :weight)
(def ^:dynamic *crossover-type* :uniform)
(def ^:dynamic *mutation-type* :uniform)
(def ^:dynamic *allele-mutation-chance* 0.01)

(defn run-ga [n]
  (iterate run-gen (repeatedly n rand-agent)))

(defn run-gen [agents]
  (map (comp mutate crossover) (select-mates agents (map fitness agents))))

(defn select-by-weight [agents scores]
  (let [weights (map #(- % (* 0.8 (reduce min scores))) scores)]
    (weighted-rand-nth agents weights)))

(defn select-by-tournament [agents scores]
  (let [pool (take 4 (distinct (repeatedly #(rand-nth (map vector agents scores)))))]
    (first (reduce #(max-by second %) pool))))

(defn select-agent [agents scores]
  (({:weight     select-by-weight
     :tournament select-by-tournament} *selection-type*)
   agents scores))

(defn select-mates [agents scores]
  (letfn [(make-pair []
            (take 2 (distinct (repeatedly #(select-agent agents scores)))))]
    (take (count agents) (repeatedly (make-pair)))))
  
(defn uniform-crossover [[a1 a2]]
  (map #(rand-nth [%1 %2]) a1 a2 (range)))

(defn mutate [a]
  (map #(if (< (rand) *allele-mutation-chance*) (random-allele) %) a))
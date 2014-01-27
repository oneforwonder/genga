(ns genga.digit-agent)

;; This digit agent is a trivially simple example agent.
;; Each allele is a decimal digit [0-9] and the entire genome is a sequence of
;; 9 such digits. The agent's fitness is equal to those digits interpreted as
;; a decimal number.
;; Hence [0 0 0 0 0 0 0 0 0] is the worst possible agent with a score of 0.
;; While [9 9 9 9 9 9 9 9 9] is the best possible agent with a score of 999,999,999.

(def num-alleles 9)

(defn rand-allele []
  (rand-nth (range 10)))

(defn rand-agent []
  (repeatedly num-alleles rand-allele))

(defn fitness [a]
  (reduce + (map * (reverse a) (iterate #(* 10 %) 1))))


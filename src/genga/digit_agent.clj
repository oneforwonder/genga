(ns genga.digit-agent)

(def num-alleles 10)

(defn rand-allele []
  (rand-nth (range 10)))

(defn rand-agent []
  (repeatedly num-alleles rand-allele))

(defn fitness [a]
  (reduce + (map * (reverse a) (iterate #(* 10 %) 1))))


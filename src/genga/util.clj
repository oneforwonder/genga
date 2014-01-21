(ns genga.util)

(defn weighted-rand-nth
  "Take two corresponding seqs of weights and xs and return a random x with the
  likelihood of weight-of-x/sum-of-weights. Example: [:a :b :c], [4 6 10] would
  yield :a 20%, :b 30%, and :c 50% of the time."
  [xs weights]
  (let [r (rand (reduce + weights))]
    (ffirst (filter #(< r (second %)) (map vector xs (reductions + weights))))))

(defn avg [coll]
  (/ (reduce + coll) (count coll)))

(defn indexed [coll]
  (map vector (iterate inc 0) coll))

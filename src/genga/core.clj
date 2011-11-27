(ns genga.core
  (:use (ga :only [run-ga])
        (util :only [avg indexed])))

(defn use-ga []
  (binding [*selection-type* :tournament
            *crossover-type* :one-point]
    (doseq [[i gen] (indexed (take 50 (run-ga 20)))]
      (let [scores (map fitness gen)]
        (println "Generation" i)
        (println "Average: " (float (avg scores)))
        (println "Best: " (apply max scores) "\n")))))
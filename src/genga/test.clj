(ns genga.test
  (:use [genga.ga])
  (:require [genga.util]
            [genga.digit-agent]))

(defn -main []
  (binding [*rand-agent-fn*    genga.digit-agent/rand-agent
            *fitness-fn*       genga.digit-agent/fitness
            *rand-allele-fn*   genga.digit-agent/rand-allele
            *population-count* 10
            *selection-type*   :tournament
            *crossover-type*   :point]
    (doseq [[i gen] (genga.util/indexed (take 51 (run-ga)))]
      (let [scores (map genga.digit-agent/fitness gen)]
        (when (= (mod i 10) 0)
          (println "Generation" i)
          (println "Average: " (genga.util/avg scores))
          (println "Best: " (apply max scores) "\n"))))))


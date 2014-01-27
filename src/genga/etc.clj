;; NOTE: The code and comments relating to a score->weight fn are stored here,
;; 1) so as not to clutter ga.clj, and 2) because I am not sure I like the 
;; solution given. 
;; For now, I would recommend the problem of clustered scores be solved by 
;; using tournament selection.

;; When agents are selected for inclusion in mates, agents with higher fitness
;; scores are preferred. One method for doing this is to select agents in
;; direct proportion to their fitness score, e.g. an agent with a score of 400
;; would be selected exactly twice as often as an agent with a score of 200.
;; This method works does not work well, however, when the scores are clustered
;; together, e.g. lowest score is 200 and highest score is 220. 
;; In this case, we may want to use a function to transform our scores into a
;; range more suitable for use as weights. An example is shown below of 
;; subtracting 80% of the lowest score from each score to give its weight.
;;  Given the example above, score 200 becomes weight 40 and score 220 becomes
;;  weight 60.
(def ^:dynamic *score->weight-fn* (fn [w mw] (identity w)))
;(def ^:dynamic *score->weight-fn* (fn [w mw] (- w (* 0.8 mw))))

(defn select-by-weight [agents scores]
  (let [min-score (apply min scores)
        scaled-ws (map #(*weight-scale-fn* % min-weight) scores)]
    (weighted-rand-nth agents scaled-ws)))

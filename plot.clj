(comment

  Plots using Incanter

)

(ns posthoc.plot
  (:use [incanter.core])
  (:use [incanter.charts])
  (:use [incanter.stats]))

(defn plot-weights
  "Plot bar chart (eg multinomial probabilities)"
  [mdist]
  (view (bar-chart
         (range (count mdist))
         mdist)))

(defn plot-freq
  "Plot histogram"
  [events]
  (view (histogram events)))

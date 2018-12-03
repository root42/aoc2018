(ns aoc2018.core
  (:gen-class))

(defn read-input
  [input-file]
  (let [input (slurp input-file)]
    (map #(Integer. %) (clojure.string/split-lines input))) 
  )

(defn calibrate
  "Advent of Code 2018 - Part 1:
  Sum up all integers in the file
  "
  [frequencies]
  (reduce + frequencies)
  )

(defn duplicate
  "Advent of Code 2018 - Part 2:
  Find the first duplicate frequency"
  [input]
  (let [intermediates (reductions + (cycle input))
        result (reduce
                (fn [seen
                     x]
                  (if (contains? seen x)
                    (reduced x)
                    (conj seen x)
                    )
                  )
                #{} intermediates
                )
        ]
    (if (set? result)
      nil
      result
      )
    )
  )

(defn -main
  "Advent of Code 2018."
  [& args]
  (let [input (read-input "input.txt")]
    (println "1. Calibration result: " (calibrate input))
    (println "2. First duplicate: " (duplicate input))
    )
  )

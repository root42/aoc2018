(ns aoc2018.core
  (:gen-class)
  )

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

(defn strip [coll chars]
  (apply str (remove #((set chars) %) coll)))

(defn split-lines-and-strip-whitespaces
  [input]
  (map #(strip % "# ") (clojure.string/split-lines input))
  )

(defn read-input-squares
  [input-file]
  (let [input (slurp input-file)
        lines (split-lines-and-strip-whitespaces input)
        ]
    (map #(clojure.string/split % #"[@,:x]") lines)
    )
  )

(defn to-claims
  [input]
  (->> input
       (map (fn [s] (map #(Integer. %) s)))
       (map #(list
              (nth % 0)
              (nth % 1)
              (nth % 2)
              (+ (nth % 1) (nth % 3))
              (+ (nth % 2) (nth % 4))
              )
            )
       (map #(zipmap [:id :left :top :right :bottom] %))
   )
  )

(defn locations [input-square]
  (for [row (range (:top input-square) (:bottom input-square))
        col (range (:left input-square) (:right input-square))]
    [row col]))

(defn add-claim
  [fabric claim]
  (reduce
   (fn [fabric location]
     (update fabric location conj (:id claim))
     )
   fabric
   (locations claim)
   )
  )

(defn overlapping?
  [location-claims]
  (> (count location-claims) 1)
  )

(defn overlap
  [input]
    (->> input
         (to-claims)
         (reduce add-claim {})
         (vals)
         (filter overlapping?)
         (count)
     )
  )

(defn non-overlap
  [input]
  (let [claims (to-claims input)
        all-claims (map :id claims)
        ]
    (remove 
     (->> claims
          (reduce add-claim {})
          (vals)
          (filter overlapping?)
          (reduce into #{})
          )
     all-claims
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
  (let [input (read-input-squares "input-day-3.txt")]
    (println "3.1. overlapping square inches: " (overlap input))
    (println "3.2. non-overlapping IDs: " (non-overlap input))
    )
  )

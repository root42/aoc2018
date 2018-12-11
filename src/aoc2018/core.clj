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
  [fabric]
  (->> fabric
       (vals)
       (filter overlapping?)
       )
  )

(defn overlapping-inches
  [fabric]
    (->> fabric
         (overlap)
         (count)
     )
  )

(defn non-overlapping-ids
  [fabric claims]
  (let [all-claims (map :id claims)
        ]
    (remove 
     (->> fabric
          (overlap)
          (reduce into #{})
          )
     all-claims
     )
    )
  )

(defn read-input-guards
  [input-file]
  (->> input-file
       slurp
       clojure.string/split-lines
       sort
       )
  )

(defn parse-minute
  [token]
  (-> token
       (clojure.string/split #":")
       (last)
       (Integer.)
       )
  )

(defn lines->guards
  [lines]
  (:naplog
   (reduce (fn [acc line]
             (let [tokens (clojure.string/split line #" ")]
               (case (nth tokens 2) ; parse one of "Guard", "falls" or "wakes"
                 "Guard" (assoc acc :guard-id (nth tokens 3)) ; take guard id
                 "falls" (assoc acc :start (parse-minute (nth tokens 1)))
                 "wakes" (update acc :log conj
                                 (-> (select-keys acc [:guard-id :start])
                                     (assoc :end (parse-minute (nth tokens 1))
                                            )
                                     )
                                 )
                 )
               )
             )
           {:naplog []}
           lines
           )
   )
  )

(defn guards->minutes
  [guards]
  nil
  )

(defn minutes->guardid-minute
  [minutes]
  nil
  )

(defn asleep
  [input]
  (->> input
       (map lines->guards)
       (reduce guards->minutes)
       (reduce minutes->guardid-minute)     
       )
  )

(defn -main
  "Advent of Code 2018."
  [& args]
  ;; (let [input (read-input "input.txt")]
  ;;   (println "1. Calibration result: " (calibrate input))
  ;;   (println "2. First duplicate: " (duplicate input))
  ;;   )
  ;; (let [input (read-input-squares "input-day-3.txt")
  ;;       claims (to-claims input)
  ;;       fabric (reduce add-claim {} claims)
  ;;       ]
  ;;   (println "3.1. overlapping square inches: " (overlapping-inches fabric))
  ;;   (println "3.2. non-overlapping IDs: " (non-overlapping-ids fabric claims))
  ;;   )
  (let [input (read-input-guards "input-day-4.txt")
        ]
    (println "4.1. ID of guard, minute mostly asleep: " (asleep input))
    )
  )

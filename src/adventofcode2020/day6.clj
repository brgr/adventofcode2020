(ns adventofcode2020.day6)

(def input (-> (slurp "resources/day6.input")
               (clojure.string/split #"\n\n")))

(def part1 (apply + (->> (map set input)
                         (map #(disj % \newline))
                         (map count))))

(defn count-answers [answers]
  (apply clojure.set/intersection (map set answers)))

(defn part2 (apply + (->> input
                          (map clojure.string/split-lines)
                          (map count-answers)
                          (map count))))

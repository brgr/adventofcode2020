(ns adventofcode2020.day5)

(def input
  (->> (slurp "resources/day5.input")
       clojure.string/split-lines
       (map #(split-at 7 %))))

(defn binary-walk [lower upper s i]
  (if (empty? s)
    lower
    (let [direction (case (first s) \B :down \R :down \F :up \L :up)]
      (case direction
        :up (recur lower (- upper i) (rest s) (/ i 2))
        :down (recur (+ lower i) upper (rest s) (/ i 2))))))

(def row-column (fn [[a b]]
                 [(binary-walk 0 127 a 64) (binary-walk 0 7 b 4)]))

(def ids (vec (->> input
               (map row-column)
               (map (fn [[a b]] (+ (* a 8) b))))))

(def part1 (apply max ids))

; ------

(def part2
  (sort (vec (clojure.set/difference (set (range part1)) (set ids)))))

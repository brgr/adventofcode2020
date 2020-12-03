(ns adventofcode2020.day3)

(def input (->> (slurp "resources/day3.input") clojure.string/split-lines))

(defn field [x y]
  (let [width (count (first input))
        line (get input y)]
    (get line (mod x width))))

(defn how-many-trees [right down]
  (loop [x right
         y down
         count-trees 0]
    (let [count-trees (if (= (field x y) \#) (inc count-trees) count-trees)]
      (if (> (inc y) (count input))
        count-trees
        (recur (+ x right) (+ y down) count-trees)))))

(def part1 (how-many-trees 3 1))

(def part2
  (apply * [(how-many-trees 1 1)
            (how-many-trees 3 1)
            (how-many-trees 5 1)
            (how-many-trees 7 1)
            (how-many-trees 1 2)]))
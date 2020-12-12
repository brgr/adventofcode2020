(ns adventofcode2020.day12)

(def input (slurp "resources/day12.input"))

(defn parse-input [input]
  (->> input
       clojure.string/split-lines
       (map (partial split-at 1))
       (map (fn [[d n]] [(first d) (Integer/parseInt (apply str n))]))))

(def directions [\N \E \S \W])

(defn turn-left [facing degrees]
  (let [times (/ degrees 90)
        face-pos ((into {} (map #(vec [(get directions %) %]) (range 4))) facing)]
    (get directions (mod (- face-pos times) 4))))

(defn turn-right [facing degrees]
  (let [times (/ degrees 90)
        face-pos ((into {} (map #(vec [(get directions %) %]) (range 4))) facing)]
    (get directions (mod (+ face-pos times) 4))))

(def start {:facing   \E
            :position [0 0]})

(defn get-next [{:keys [facing position] :as p} instr]
  (let [[x y] position]
    (case (first instr)
      \N {:facing facing, :position [x (+ y (second instr))]}
      \S {:facing facing, :position [x (- y (second instr))]}
      \E {:facing facing, :position [(+ x (second instr)) y]}
      \W {:facing facing, :position [(- x (second instr)) y]}
      \L {:facing (turn-left facing (second instr)), :position position}
      \R {:facing (turn-right facing (second instr)), :position position}
      \F (get-next p [facing (second instr)]))))

(def part1
  (-> (reduce get-next start (parse-input input))
      :position
      (map #(Math/abs ^Integer %))))


; ---

(defn turn-left-once [[x y]]
  [(- y) x])

(defn turn-right-once [[x y]]
  [y (- x)])

(nth (iterate turn-right-once [10 5]) 1)


(def start {:position [0 0]
            :waypoint [10 1]})

(defn get-next [{:keys [position waypoint]} instr]
  (let [[x y] waypoint]
    (println instr position waypoint)
    (case (first instr)
      \N {:position position, :waypoint [x (+ y (second instr))]}
      \S {:position position, :waypoint [x (- y (second instr))]}
      \E {:position position, :waypoint [(+ x (second instr)) y]}
      \W {:position position, :waypoint [(- x (second instr)) y]}
      \L {:position position, :waypoint (nth (iterate turn-left-once waypoint) (/ (second instr) 90))}
      \R {:position position, :waypoint (nth (iterate turn-right-once waypoint) (/ (second instr) 90))}
      \F {:position (map + (map (partial * (second instr)) waypoint) position), :waypoint waypoint})))

(-> (reduce get-next start (parse-input input))
    :position)
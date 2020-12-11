(ns adventofcode2020.day10)

(def input (->> (slurp "resources/day10.input")))

(def test1-adapters "16\n10\n15\n5\n1\n11\n7\n19\n6\n12\n4")
(def test2-adapters "28\n33\n18\n42\n31\n14\n46\n20\n48\n47\n24\n23\n49\n45\n19\n38\n39\n11\n1\n32\n25\n35\n8\n17\n7\n9\n4\n2\n34\n10\n3")

(def adapters (->> input
                   clojure.string/split-lines
                   (map #(Integer/parseInt %))))

(def built-in-joltage-adapter (+ 3 (apply max adapters)))

(defn with-next-adapters [all-adapters]
  (let [f (fn [adapter]
            (let [allowed-adapters [(+ adapter 1) (+ adapter 2) (+ adapter 3)]]
              [adapter (filter #(.contains allowed-adapters %) all-adapters)]))]
    (into {} (map f all-adapters))))

(def adapter-map
  (with-next-adapters (conj adapters 0 built-in-joltage-adapter)))

(defn adapter-path [adapter-map current-path]
  (let [last-adapter (last current-path)
        next-adapters (get adapter-map last-adapter)]
    (if (empty? next-adapters)
      current-path
      (recur adapter-map (conj current-path
                               (apply min next-adapters))))))

(defn count-diffs [l]
  (loop [l l
         counts {:1 0, :2 0, :3 0}]
    (if (= 1 (count l))
      counts
      (let [diff (- (second l) (first l))
            counts (update-in counts [(keyword (str diff))] inc)]
        (recur (rest l) counts)))))

(def part1
  (count-diffs (adapter-path adapter-map [0])))

; ------

(def adapters
  (into (sorted-set) (conj adapters 0 built-in-joltage-adapter)))

(defn how-many-paths-lead-to-this-adapter [adapters adapter]
  (->> (range (- adapter 3) (inc adapter))
       (keep adapters)
       (reduce +)))

(defn f [adapters next-adapter]
  (assoc adapters next-adapter (how-many-paths-lead-to-this-adapter adapters next-adapter)))

(def part2
  (let [ways (reduce f {(first adapters) 1} (rest adapters))]
    (ways (last adapters))))

; To understand this, have a look at the first few numbers and what is calculated there:
; 0 (+ 1)
; 1 (+ 1)
; 2 (+ 1 1) -- from 0 and 1
; 5 (+ 2) -- from 2
; 6 (+ 2) -- from 5
; 7 (+ 2 2) -- from 5 and 6
; 8 (+ 2 2 4)
; 9 (+ 2 4 8)
; 12 (+ 14)
; 15 (+ 14)
;
; There is 1 path to get to adapter 0 (this is the trivial case).
; We can reach 1 via 0 (but not more), thus there is still only one total path.
; We can reach 2 via 1, but also via 0 (both are in reach). Therefore, we add the paths that we can take up to those
;  two = 1 + 1 = 2.
; We can reach 5 only via 2, thus it is the same paths as for 2.
; Same for 6.
; We can reach 7 both via 5, and via 6. We add the paths leading to those together.
; And so on...
;
; We thus have for example:
; 0 1 2 5 6 7 ...
; 0 1 2 5   7 ...
; 0   2 5 6 7 ...
; 0   2 5   7 ...
; ...
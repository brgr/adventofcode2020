(ns adventofcode2020.day11
  (:require [clojure.math.combinatorics]))

(def input (slurp "resources/day11.input"))
;(def input "L.LL.LL.LL\nLLLLLLL.LL\nL.L.L..L..\nLLLL.LL.LL\nL.LL.LL.LL\nL.LLLLL.LL\n..L.L.....\nLLLLLLLLLL\nL.LLLLLL.L\nL.LLLLL.LL")

(defn create-map [input]
  (let [split-lines (clojure.string/split-lines input)]
    {:layout split-lines
     :width  (count (first split-lines))
     :height (count split-lines)}))

(defn get-at [seats x y]
  (-> (nth (:layout seats) y)
      (nth x)))

(defn get-adjacent-seats [{:keys [layout width height] :as seats} x y]
  (let [start-x (if (> x 0) (- x 1) 0)
        end-x (if (< x (- width 1)) (+ x 1) (- width 1))
        start-y (if (> y 0) (- y 1) 0)
        end-y (if (< y (- height 1)) (+ y 1) (- height 1))
        seat-positions (-> (clojure.math.combinatorics/cartesian-product (range start-x (+ end-x 1))
                                                                         (range start-y (+ end-y 1)))
                           (set)
                           (disj [x y]))]
    (map (fn [[x y]] (get-at seats x y)) seat-positions)))

(defn count-occupied-adjacent-seats [seats x y]
  (->> (get-adjacent-seats seats x y)
       (filter #(= \# %))
       (count)))

(defn get-new-seats [seats]
  {:layout (for [y (range (:height seats))]
             (for [x (range (:width seats))]
               (let [seat (get-at seats x y)
                     occupied-adjacent-seats (count-occupied-adjacent-seats seats x y)]
                 (cond
                   (and (= seat \L) (= occupied-adjacent-seats 0)) \#
                   (and (= seat \#) (>= occupied-adjacent-seats 4)) \L
                   :else seat))))
   :width  (:width seats)
   :height (:height seats)})

(defn get-seats-that-wont-change [seats]
  (loop [last-seats seats
         seats (get-new-seats seats)]
    (if (= seats last-seats)
      seats
      (recur seats (get-new-seats seats)))))

(def seats (create-map input))
(def part1
  (->> (:layout (get-seats-that-wont-change seats))
       (flatten)
       (filter #{\#})
       (count)))

; ---

; for part 2, see for inspiration:
; https://topaz.github.io/paste/#XQAAAQBoBAAAAAAAAAAUGQimf64ubUKcDECWDcLPSVJJc7ZtOWb1QPXaq2efGeI74YoICSCqSCrRK7HTI6se3LxzV98v6Tv7HdjbUM7W9ZBI+OeSh6Irhdr/2/UqJJwOVbdXcMPiISzlGxT4+hkT1iPnxaZM5Hj9k9o3mj+Jg63Nl8bEDLoDavIof4HXrHMViolgPz+rELsnoObo1VzaDTkU+uALY5dOsl0z336WUT6OEuPe+HI2brHM5+YATgNy3W87uoTEAG5jhXKrHNku5far6XLFhwWPONnu1Q0br1L3O0LWuJgU5b1+LSV2eKSSutw5TCMaOaV76v/yhS5u2dlm2eA6+r1THhxETz67cZ1QM6k31IqEzqs/hjJqlnXaIiSveqKahoiEi0H4/KgB4f7dyDZvpPueXF8RSd9gbApHyR8Q2tyifw1J0Tef9cnHcOlYrL5uq6BOVyw1c3n0xTpZZHOTXHzSEPN3kVoY7jL4NpIzijSCUnvn8In0YQr2ft5uqcj7SaAU1lPzuZkL0IiQz9k8YdTWDIkUK3PpvHyxwkktfTKFa5Zmsj0g4GWNwMB4JhoU07WCwzQpsRNjhR6Eo7YPmGkENUvfyxEg9zz6KMXdflZLPCiBQbtUHuNVZpk2Z3unc+iS4U1Q8XwbYPr0EIDWM/GBF15gH4gh4h4GK1fOu8Nhe9GH/3ekAbSvVJr/9gLD8rQrUYParFaXCkhmxlKCvo/Zp//HKQPX
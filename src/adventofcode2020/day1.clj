(ns adventofcode2020.day1
  (:require [clojure.math.combinatorics :as math]))

(def input (-> (slurp "resources/day1.input")
               (clojure.string/split #"\n")))

(def part1
  (->> input
       (map #(Integer/parseInt %))
       (repeat 2)
       (apply math/cartesian-product)
       (filter (fn [[a b]] (= 2020 (+ a b))))
       first
       (apply *)))

(def part2
  (->> input
       (map #(Integer/parseInt %))
       (repeat 3)
       (apply math/cartesian-product)
       (filter (fn [[a b c]] (= 2020 (+ a b c))))
       first
       (apply *)))
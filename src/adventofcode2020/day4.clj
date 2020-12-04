(ns adventofcode2020.day4)

(def unparsed-input (-> (slurp "resources/day4.input")
                        (clojure.string/split #"\n\n")))

(defn parse-passport [passport-text]
  (into {} (->> passport-text
                (map #(clojure.string/split % #":"))
                (map (fn [[k v]] [(keyword k) v])))))

(def input (->> unparsed-input
                (map #(clojure.string/split % #"\n|\ "))
                (map parse-passport)))

(defn has-correct-fields? [passport]
  (and (contains? passport :iyr)
       (contains? passport :pid)
       (contains? passport :ecl)
       (contains? passport :hgt)
       (contains? passport :byr)
       (contains? passport :hcl)
       (contains? passport :eyr)))

(def part1 (->> input
                (filter has-correct-fields?)
                (count)))

; ------

(def valid-byr (fn [year]
                 (and (not (empty? year))
                      (let [y (Integer/parseInt year)]
                        (and (>= y 1920) (<= y 2002))))))

(def valid-iyr (fn [year]
                 (and (not (empty? year))
                      (let [y (Integer/parseInt year)]
                        (and (>= y 2010) (<= y 2020))))))

(def valid-eyr (fn [year]
                 (and (not (empty? year))
                      (let [y (Integer/parseInt year)]
                        (and (>= y 2020) (<= y 2030))))))

(def valid-hgt (fn [height]
                 (and (not (nil? height))
                      (or (clojure.string/includes? height "in")
                          (clojure.string/includes? height "cm"))
                      (let [split-up (clojure.string/split height #"c|i")
                            h (Integer/parseInt (first split-up))]
                        (case (second split-up)
                          "n" (and (>= h 59) (<= h 76))
                          "m" (and (>= h 150) (<= h 193))
                          false)))))

(def valid-hcl (fn [hair]
                 (and (not (nil? hair))
                      (not (nil? (re-matches #"\#[\da-f][\da-f][\da-f][\da-f][\da-f][\da-f]" hair))))))

(def valid-pid (fn [hair]
                 (and (not (nil? hair))
                      (not (nil? (re-matches #"\d\d\d\d\d\d\d\d\d" hair))))))

(def valid-ecl (fn [hair]
                 (and (not (nil? hair))
                      (not (nil? (re-matches #"amb|blu|brn|gry|grn|hzl|oth" hair))))))

(def part2 (filter #(and (has-correct-fields? %)
                         (valid-byr (:byr %))
                         (valid-iyr (:iyr %))
                         (valid-eyr (:eyr %))
                         (valid-hgt (:hgt %))
                         (valid-hcl (:hcl %))
                         (valid-pid (:pid %))
                         (valid-ecl (:ecl %))) input))
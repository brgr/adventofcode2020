(ns adventofcode2020.day7)

(def input (->> (slurp "resources/day7.input") clojure.string/split-lines))

(defn get-bag-count [bag-count-string]
  (let [number (re-find #"\d+" bag-count-string)
        bag (re-find #"[a-z]+\ ?[a-z]+" bag-count-string)]
    {:count (Integer/parseInt number)
     :bag   bag}))

(defn get-bags [bags-list]
  (if (= "no other" (first bags-list))
    nil
    (map get-bag-count bags-list)))

(defn split-up [line]
  (let [[bag & contains] (->> (clojure.string/split line #"\ bags\ contain\ |\, |\ bags|\.")
                              (filter not-empty))]
    [bag (map :bag (get-bags contains))]))

(def bags (->> input
               (map split-up)
               (into {})))

(defn contains-shiny-gold? [bag all-bag-list]
  (let [bags (get all-bag-list bag)]
    (cond
      (empty? bags) false
      (.contains bags "shiny gold") true
      :else (some true? (map #(contains-shiny-gold? % all-bag-list) bags)))))


(def part1 (->> (map #(contains-shiny-gold? % bags) (keys bags))
                (filter true?)
                count))


; --- part 2

(def example1 (clojure.string/split-lines "\n\nlight red bags contain 1 bright white bag, 2 muted yellow bags.\ndark orange bags contain 3 bright white bags, 4 muted yellow bags.\nbright white bags contain 1 shiny gold bag.\nmuted yellow bags contain 2 shiny gold bags, 9 faded blue bags.\nshiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.\ndark olive bags contain 3 faded blue bags, 4 dotted black bags.\nvibrant plum bags contain 5 faded blue bags, 6 dotted black bags.\nfaded blue bags contain no other bags.\ndotted black bags contain no other bags.\n"))

(defn split-up [line]
  (let [[bag & contains] (->> (clojure.string/split line #"\ bags\ contain\ |\, |\ bags|\.")
                              (filter not-empty))]
    [bag (get-bags contains)]))

(def bags (->> input
               (map split-up)
               (into {})))

(defn get-total-count [all-bags bag]
  (let [bags (get all-bags bag)]
    (if (empty? bags)
      0
      (let [count-of-these (map :count bags)
            count-recursive (map
                              (fn [{:keys [count bag]}] (* count (get-total-count all-bags bag)))
                              bags)]
        (apply + (concat count-of-these count-recursive))))))

(def part2 (get-total-count bags "shiny gold"))

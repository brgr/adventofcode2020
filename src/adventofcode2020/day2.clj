(ns adventofcode2020.day2)

(def input (-> (slurp "resources/day2.input")
               (clojure.string/split #"\n")))

(defn parse-policy [policy]
  (let [[min-max character] (clojure.string/split policy #" ")
        [min max] (clojure.string/split min-max #"\-")]
    {:character      (-> character .getBytes first char)
     :min-occurrence (Integer/parseInt min)
     :max-occurrence (Integer/parseInt max)}))

(defn parse [[policy password]]
  {:policy   (parse-policy policy)
   :password password})

(defn check-valid [{:keys [policy password]}]
  (let [count (get (frequencies password) (:character policy) 0)]
    (and (>= count (:min-occurrence policy))
         (<= count (:max-occurrence policy)))))


(def part1 (count (->> input
                       (map #(clojure.string/split % #":\ "))
                       (map parse)
                       (filter check-valid))))

; -----

(defn parse-policy [policy]
  (let [[min-max character] (clojure.string/split policy #" ")
        [min max] (clojure.string/split min-max #"\-")]
    {:character         (-> character .getBytes first char)
     :first-occurrence  (- (Integer/parseInt min) 1)
     :second-occurrence (- (Integer/parseInt max) 1)}))

(defn parse [[policy password]]
  {:policy   (parse-policy policy)
   :password password})

(defn check-valid [{:keys [policy password]}]
  (let [cond1 (= (get password (:first-occurrence policy)) (:character policy))
        cond2 (= (get password (:second-occurrence policy)) (:character policy))]
    (or (and cond1 (not cond2))
        (and (not cond1) cond2))))

(def part2 (->> input
                (map #(clojure.string/split % #":\ "))
                (map parse)
                (filter check-valid)
                count))

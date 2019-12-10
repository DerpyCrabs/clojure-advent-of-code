(ns clojure-advent-of-code.day10
  (:require [clojure.java.io :as io]
            [clojure.math.numeric-tower :as math]))

(defn asteroid-map []
  (let [contents (slurp (io/resource "input10.txt"))]
    (->> contents
         clojure.string/split-lines)))

(defn map->asteroids [ast-map]
  (apply concat (map (fn [[ast-str y]] (loop [x 0 asteroids []]
                                   (if (= x (count ast-str))
                                     asteroids
                                     (if (= \# (nth ast-str x))
                                       (recur (inc x) (conj asteroids [x y]))
                                       (recur (inc x) asteroids)))))
               (map vector ast-map (range (count ast-map))))))

(defn distance [[sx sy] [ex ey]]
  (math/sqrt (+ (math/expt (- ex sx) 2) (math/expt (- ey sy) 2))))

(defn obstructs-sight? [A B C]
  (= (math/round (* 100000 (distance A B))) (math/round (* 100000 (+ (distance A C) (distance B C))))))

(defn in-direct-sight? [start end asteroids]
  ((complement some) #(obstructs-sight? start end %)
                     (filter #(not (or (= start %) (= end %))) asteroids)))

(defn in-direct-sight [ast asteroids]
                    (filter #(in-direct-sight? ast % asteroids)
                            (filter #(not= ast %) asteroids)))

(defn solution []
  (let [asteroids (map->asteroids (asteroid-map))]
    (last (sort-by
           second
           (map
            #(vector % (count (in-direct-sight % asteroids)))
            asteroids)))))

(defn angle [[bx by] [ax ay]]
  (let [angle (Math/toDegrees (Math/atan2 (- ax bx) (-(- ay by))))]
  (if (neg? angle)
    (- 360 (Math/abs angle))
    angle)))

(defn next-target [base asteroids]
  (first (first (sort-by second
                          (map #(vector % (angle base %)) asteroids)))))

(defn without [elem coll]
  (remove #(= elem %) coll))

(defn solution2 []
  (let [base [19 11]
        asteroids (without base (map->asteroids (asteroid-map)))]
    (loop [n 0
           ast nil
           remaining-targets (in-direct-sight base asteroids)
           asteroids asteroids]
      (cond
        (= n 200) (+ (* 100 (first ast)) (second ast))

        (empty? remaining-targets) (recur n ast (in-direct-sight base asteroids) asteroids)

        :else (let [next-asteroid (next-target base remaining-targets)
                    remaining-targets (without next-asteroid remaining-targets)
                    asteroids (without next-asteroid asteroids)]
                (recur (inc n) next-asteroid remaining-targets asteroids))))))
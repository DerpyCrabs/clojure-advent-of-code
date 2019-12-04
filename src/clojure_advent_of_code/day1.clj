(ns clojure-advent-of-code.day1
  (:require [clojure.java.io :as io]))

(defn get-numbers []
  (let [contents (slurp (io/resource "input1.txt"))]
    (->> contents
         clojure.string/split-lines
         (map read-string))))

(defn compute-fuel [mass] (- (Math/floor (/ mass 3)) 2))

(defn compute-fuel2 [mass]
  (let [fuel (compute-fuel mass)]
    (loop [total-fuel fuel remaining-fuel fuel]
      (if (pos? (compute-fuel remaining-fuel))
        (recur (+ total-fuel (compute-fuel remaining-fuel)) (compute-fuel remaining-fuel))
        total-fuel))))

(defn solution [] (->> (get-numbers)
                       (map compute-fuel)
                       (reduce +)))

(defn solution2 [] (->> (get-numbers)
                        (map compute-fuel2)
                        (reduce +)))


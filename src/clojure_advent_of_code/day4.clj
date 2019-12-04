(ns clojure-advent-of-code.day4)

(def input [171309 643603])

(defn has-same-adjacent-digits [pass]
  (loop [n 0]
    (if (= n 5)
      false
      (if (= (nth pass n) (nth pass (inc n)))
        true
        (recur (inc n))))))

(defn digits-doesnt-decrease [pass]
  (apply <= (map #((comp read-string str) %) pass)))

(defn solution []
  (loop [i (first input) count 0]
    (if (= i (second input))
      count
      (if (and (has-same-adjacent-digits (str i)) (digits-doesnt-decrease (str i)))
        (recur (inc i) (inc count))
        (recur (inc i) count)))))

(defn has-only-two-adjacent-digits [pass]
  (loop [n 0 last-digit nil]
    (if (= n 5)
      false
      (if (and (= (nth pass n) (nth pass (inc n))) (not= (nth pass (+ n 2) nil) (nth pass n)) (not= last-digit (nth pass n)))
        true
        (recur (inc n) (nth pass n))))))

(defn solution2 []
  (loop [i (first input) count 0]
    (if (= i (second input))
      count
      (if (and (has-only-two-adjacent-digits (str i)) (digits-doesnt-decrease (str i)))
        (recur (inc i) (inc count))
        (recur (inc i) count)))))
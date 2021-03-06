(ns clojure-advent-of-code.day3
  (:require [clojure.java.io :as io]))

(defn read-path [path] (let [moves (read-string (subs path 1))]
                         [(keyword (str (first path))) moves]))

(defn get-wires []
  (let [contents
        (slurp (io/resource "input3.txt"))]
    (->> contents
         clojure.string/split-lines
         (map #(clojure.string/split % #","))
         (map #(map read-path %)))))

(defn path-move [[[dir dist] & path]]
  (if (= dist 1)
    path
    (cons [dir (dec dist)] path)))

(defn point-path-move [[x y] [[dir dist] & path]]
  (let [new-point (case dir
                    :U [x (+ y dist)]
                    :D [x (- y dist)]
                    :R [(+ x dist) y]
                    :L [(- x dist) y])]
    [new-point path]))

(defn point-path-move-1dist [[x y] [[dir] :as path]]
  (let [new-point (case dir
                    :U [x (inc y)]
                    :D [x (dec y)]
                    :R [(inc x) y]
                    :L [(dec x) y])]
    [new-point (path-move path)]))

(defn point-line-intersect? [[x y] [[sx sy] [ex ey]]]
  (or (and (<= sx x ex) (<= sy y ey)) (and (>= sx x ex) (>= sy y ey))))

(defn abs [n] (max n (- n)))

(defn point-path-intersect? [point path]
  (loop [pos [0 0] path path steps 0]
    (if (empty? path)
      false
      (let [[new-pos new-path] (point-path-move pos path)]
        (if (point-line-intersect? point [pos new-pos])
          (+ steps (abs (- (first point) (first pos))) (abs (- (second point) (second pos))))
          (recur new-pos new-path (+ steps (second (first path)))))))))

(defn intersections []
  (let [[wire1 wire2] (get-wires)]
    (loop [pos [0 0] path wire1 intersections []]
      (if (empty? path)
        intersections
        (let [[new-pos new-path] (point-path-move-1dist pos path)]
          (if (point-path-intersect? new-pos wire2)
            (recur new-pos new-path (conj intersections new-pos))
            (recur new-pos new-path intersections)))))))


(defn manhattan-distance [[x y]] (+ (abs x) (abs y)))

(defn solution [] (->> (intersections)
                       (map manhattan-distance)
                       (reduce min)))

(defn intersections2 []
  (let [[wire1 wire2] (get-wires)]
    (loop [pos [0 0] path wire1 intersections [] steps 0]
      (if (empty? path)
        intersections
        (let [[new-pos new-path] (point-path-move-1dist pos path)
              intersect (point-path-intersect? new-pos wire2)]
          (if intersect
            (recur new-pos new-path (conj intersections (+ (inc steps) intersect))  (inc steps))
            (recur new-pos new-path intersections (inc steps)))))
      )))

(defn solution2 [] (reduce min (intersections2)))
(ns clojure-advent-of-code.day6
  (:require [clojure.java.io :as io]))

(defn get-map-data []
  (let [contents (slurp (io/resource "input6.txt"))]
    (->> contents
         clojure.string/split-lines
         (map #(clojure.string/split % #"\)")))))

(defn construct-tree [data object]
  (let [orbiters (filter #(= (first %) object) data)]
    (vector object
            (reduce #(conj %1 [(first %2) (second %2)]) []
                    (map #(construct-tree data %) (map second orbiters))))))

(defn get-orbits [tree level]
  (+ level (reduce #(+ %1 (get-orbits %2 (inc level))) 0 (second tree))))

(defn solution []
  (let [map-data (get-map-data)]
    (-> map-data
        (construct-tree "COM")
        (get-orbits 0))))

(defn find-object [tree object]
  (if (= object (first tree))
    [object]
    (let [path (remove nil? (map #(find-object %1 object) (second tree)))]
      (when (and ((complement empty?) path) (some? path))
        (cons (first tree) (first path))))))

(defn path-between-objects [tree obj1 obj2]
  (let [obj1-path (find-object tree obj1)
        obj2-path (find-object tree obj2)]
    (->> (map vector obj1-path obj2-path)
         (drop-while #(= (first %) (second %)))
         (map #(concat (reverse (first %)) (second %))))))

(defn solution2 []
  (- (count (path-between-objects (construct-tree (get-map-data) "COM") "YOU" "SAN")) 2))
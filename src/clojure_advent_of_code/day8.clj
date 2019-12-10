(ns clojure-advent-of-code.day8
  (:require [clojure.java.io :as io]))

(def height 6)
(def width 25)
(defn get-layers []
  (let [contents (slurp (io/resource "input8.txt"))
        layer-size (* height width)]
    (->> contents
         (partition layer-size))))

(defn keywordize-keys [m]
  (reduce-kv #(assoc %1 (keyword (str %2)) %3) {} m))

(defn solution []
  (let [layers (get-layers)
        frequencies (map keywordize-keys (map frequencies layers))
        layer (first (sort-by :0 frequencies))]
    (* (:1 layer) (:2 layer))))

(defn transpose [m]
  (apply mapv vector m))

(defn pixel-value [values]
  (some #(not= \2 %) values))

(defn solution2 []
  (let [layers (get-layers)
        pixels (clojure.string/replace
                (clojure.string/join (map pixel-value (transpose layers)))
                #"(2|0)" " ")
        image (partition width pixels)]
    (doseq [line image] (println line))))
(ns clojure-advent-of-code.day2
  (:require [clojure.java.io :as io]))

(defn get-code []
  (let [contents (slurp (io/resource "input2.txt"))]
    (->> contents
         (#(clojure.string/split % #","))
         (map read-string))))

(defn interpreter [code]
  (loop [instruction-pointer 0 state code]
    (case (nth state instruction-pointer)
      1 (let [op1 (nth state (nth state (inc instruction-pointer)))
              op2 (nth state (nth state (inc (inc instruction-pointer))))
              res (nth state (+ 3 instruction-pointer))]
          (recur (+ instruction-pointer 4) (assoc state res (+ op1 op2))))
      2 (let [op1 (nth state (nth state (inc instruction-pointer)))
              op2 (nth state (nth state (inc (inc instruction-pointer))))
              res (nth state (+ 3 instruction-pointer))]
          (recur (+ instruction-pointer 4) (assoc state res (* op1 op2))))
      99 state)))

(defn input [code noun verb]
  (assoc (assoc code 2 verb) 1 noun))

(defn output [code] (nth code 0))

(defn solution []
  (let [code (vec (get-code))
        program-alarm (input code 12 2)]
    (output (interpreter program-alarm))))

(defmacro swallow-exceptions [& body]
    `(try ~@body (catch Exception e#)))

(defn solution2 []
  (loop [noun 0 verb 0]
    (let [code (input (vec (get-code)) noun verb)]
      (if (= (output (swallow-exceptions (interpreter code))) 19690720)
        (+ (* 100 noun) verb)
        (if (= verb 99)
          (recur (inc noun) 0)
          (recur noun (inc verb)))))))
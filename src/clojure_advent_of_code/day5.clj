(ns clojure-advent-of-code.day5
  (:require [clojure.java.io :as io]))

(defn get-code []
  (let [contents (slurp (io/resource "input5.txt"))]
    (->> contents
         (#(clojure.string/split % #","))
         (map read-string))))

(defn parse-opcode [opcode]
  [(rem opcode 100) (rem (quot opcode 100) 10) (rem (quot opcode 1000) 10) (quot opcode 10000)])

(defn interpreter [code input]
  (loop [instruction-pointer 0 state code output [] input input]
    (let [[op m1 m2 m3] (parse-opcode (nth state instruction-pointer))
          get-arg #(if (= %2 1) %1 (nth state %1))
          first-arg #(get-arg (nth state (+ instruction-pointer 1)) %)
          second-arg #(get-arg (nth state (+ instruction-pointer 2)) %)
          third-arg #(get-arg (nth state (+ instruction-pointer 3)) %)]
      (case op
        ;; sum op1 op2 out
        1 (recur (+ instruction-pointer 4)
                 (assoc state (third-arg 1)
                        (+ (first-arg m1) (second-arg m2)))
                 output input)
        ;; mul op1 op2 out
        2 (recur (+ instruction-pointer 4)
                 (assoc state (third-arg 1)
                        (* (first-arg m1) (second-arg m2)))
                 output input)
        ;; input out
        3 (recur (+ instruction-pointer 2)
                 (assoc state (first-arg 1)
                        (first input))
                 output (rest input))
        ;; output in
        4 (recur (+ instruction-pointer 2)
                 state
                 (conj output (first-arg m1))
                 input)
        ;; jump-if-true pred addr
        5 (if-not (zero? (first-arg m1))
            (recur (second-arg m2) state output input)
            (recur (+ instruction-pointer 3) state output input))
        ;; jump-if-false pred addr
        6 (if (zero? (first-arg m1))
            (recur (second-arg m2) state output input)
            (recur (+ instruction-pointer 3) state output input))
        ;; less-than op1 op2 out
        7 (if (< (first-arg m1) (second-arg m2))
            (recur (+ instruction-pointer 4)
                   (assoc state (third-arg 1) 1)
                   output input)
            (recur (+ instruction-pointer 4)
                   (assoc state (third-arg 1) 0)
                   output input))
        ;; equals op1 op2 out
        8 (if (= (first-arg m1) (second-arg m2))
            (recur (+ instruction-pointer 4)
                   (assoc state (third-arg 1) 1)
                   output input)
            (recur (+ instruction-pointer 4)
                   (assoc state (third-arg 1) 0)
                   output input))
        ;; halt
        99 [state output]))))

(defmacro swallow-exceptions [& body]
    `(try ~@body (catch Exception e#)))

(defn solution []
  (second (interpreter (vec (get-code)) [1])))

(defn solution2 []
  (second (interpreter (vec (get-code)) [5])))
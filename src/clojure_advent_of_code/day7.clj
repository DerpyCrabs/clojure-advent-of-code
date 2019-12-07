(ns clojure-advent-of-code.day7
  (:require [clojure.java.io :as io])
  (:require [clojure-advent-of-code.day5 :as day5])
  (:require [clojure.core.async :as async :refer [go go-loop >! >!! <! <!! chan]]))

(defn get-code []
  (let [contents (slurp (io/resource "input7.txt"))]
    (->> contents
         (#(clojure.string/split % #","))
         (map read-string))))

;;; from https://stackoverflow.com/questions/26076077/clojure-list-all-permutations-of-a-list
(defn permutations [colls]
  (if (= 1 (count colls))
    (list colls)
    (for [head colls
          tail (permutations (disj (set colls) head))]
      (cons head tail))))

(defn phases [] (permutations (range 5)))

(defn solution []
  (loop [[[t1 t2 t3 t4 t5] & rest :as phases] (phases) highest-signal 0]
    (if (empty? phases)
      highest-signal
      (let [thruster #(first (second (day5/interpreter (vec (get-code)) [%1 %2])))
            o1 (thruster t1 0)
            o2 (thruster t2 o1)
            o3 (thruster t3 o2)
            o4 (thruster t4 o3)
            o5 (thruster t5 o4)]
        (if (> o5 highest-signal)
          (recur rest o5)
          (recur rest highest-signal))))))

(defn parse-opcode [opcode]
  [(rem opcode 100) (rem (quot opcode 100) 10) (rem (quot opcode 1000) 10) (quot opcode 10000)])

(defn interpreter [code input output]
  (go-loop [instruction-pointer 0 state code]
        (let [[op m1 m2 m3] (parse-opcode (nth state instruction-pointer))
              get-arg #(if (= %2 1) %1 (nth state %1))
              first-arg #(get-arg (nth state (+ instruction-pointer 1)) %)
              second-arg #(get-arg (nth state (+ instruction-pointer 2)) %)
              third-arg #(get-arg (nth state (+ instruction-pointer 3)) %)]
          (case op
        ;; sum op1 op2 out
            1 (recur (+ instruction-pointer 4)
                     (assoc state (third-arg 1)
                            (+ (first-arg m1) (second-arg m2))))
        ;; mul op1 op2 out
            2 (recur (+ instruction-pointer 4)
                     (assoc state (third-arg 1)
                            (* (first-arg m1) (second-arg m2))))
        ;; input out
            3 (recur (+ instruction-pointer 2)
                     (assoc state (first-arg 1)
                            (<! input)))
        ;; output in
            4 (do
                (>! output (first-arg m1))
                (recur (+ instruction-pointer 2)
                       state))
        ;; jump-if-true pred addr
            5 (if-not (zero? (first-arg m1))
                (recur (second-arg m2) state)
                (recur (+ instruction-pointer 3) state))
        ;; jump-if-false pred addr
            6 (if (zero? (first-arg m1))
                (recur (second-arg m2) state)
                (recur (+ instruction-pointer 3) state))
        ;; less-than op1 op2 out
            7 (if (< (first-arg m1) (second-arg m2))
                (recur (+ instruction-pointer 4)
                       (assoc state (third-arg 1) 1))
                (recur (+ instruction-pointer 4)
                       (assoc state (third-arg 1) 0)))
        ;; equals op1 op2 out
            8 (if (= (first-arg m1) (second-arg m2))
                (recur (+ instruction-pointer 4)
                       (assoc state (third-arg 1) 1))
                (recur (+ instruction-pointer 4)
                       (assoc state (third-arg 1) 0)))
        ;; halt
            99 state))))

(defn thruster [in out phase]
    (interpreter (vec (get-code)) in out))

(defn phases2 [] (permutations (range 5 10)))

(defn solution2 []
  (loop [[[t1 t2 t3 t4 t5] & rest :as phases] (phases2) highest-signal 0]
    (if (empty? phases)
      highest-signal
      (let [c1 (chan 1)
            c2 (chan 1)
            c3 (chan 1)
            c4 (chan 1)
            c5 (chan 1)
            o1 (thruster c1 c2 t1)
            o2 (thruster c2 c3 t2)
            o3 (thruster c3 c4 t3)
            o4 (thruster c4 c5 t4)
            o5 (thruster c5 c1 t5)
            _ (go
                (>! c1 t1)
                (>! c2 t2)
                (>! c3 t3)
                (>! c4 t4)
                (>! c5 t5)
                (>! c1 0))
            output (do
                     (<!! o1)
                     (<!! o2)
                     (<!! o3)
                     (<!! o4)
                     (<!! o5)
                     (<!! c1))]
        (if (> output highest-signal)
          (recur rest output)
          (recur rest highest-signal))))))

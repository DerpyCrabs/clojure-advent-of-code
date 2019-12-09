(ns clojure-advent-of-code.day9
  (:require [clojure.java.io :as io])
  (:require [clojure.core.async :as async :refer [go go-loop >! >!! <! <!! chan]]))

(defn get-code []
  (let [contents (slurp (io/resource "input9.txt"))]
    (->> contents
         (#(clojure.string/split % #","))
         (map read-string))))

(defn parse-opcode [opcode]
  [(rem opcode 100) (rem (quot opcode 100) 10) (rem (quot opcode 1000) 10) (quot opcode 10000)])

(defn interpreter [code input output]
  (go-loop
   [instruction-pointer 0 state (into (hash-map) (map vector (range (count code)) code)) base 0]
    (let [[op m1 m2 m3] (parse-opcode (get state instruction-pointer))
          get-arg #(if (= %2 1)
                     %1
                     (if (= %2 2)
                       (get state (+ base %1) 0)
                       (get state %1 0)))
          first-arg (get-arg (get state (+ instruction-pointer 1) 0) m1)
          second-arg (get-arg (get state (+ instruction-pointer 2) 0) m2)

          set-first-arg
          #(assoc state
                  (if (= m1 2)
                    (+ base (get state (+ instruction-pointer 1)))
                    (get state (+ instruction-pointer 1))) %)

          set-third-arg
          #(assoc state
                  (if (= m3 2)
                    (+ base (get state (+ instruction-pointer 3)))
                    (get state (+ instruction-pointer 3))) %)]
      (case op
        ;; sum op1 op2 out
        1 (recur (+ instruction-pointer 4)
                 (set-third-arg
                  (+ first-arg second-arg)) base)
        ;; mul op1 op2 out
        2 (recur (+ instruction-pointer 4)
                 (set-third-arg
                  (* first-arg second-arg)) base)
        ;; input out
        3 (recur (+ instruction-pointer 2)
                 (set-first-arg
                  (<! input)) base)
        ;; output in
        4 (do
            (>! output first-arg)
            (recur (+ instruction-pointer 2)
                   state base))
        ;; jump-if-true pred addr
        5 (if-not (zero? first-arg)
            (recur second-arg state base)
            (recur (+ instruction-pointer 3) state base))
        ;; jump-if-false pred addr
        6 (if (zero? first-arg)
            (recur second-arg state base)
            (recur (+ instruction-pointer 3) state base))
        ;; less-than op1 op2 out
        7 (if (< first-arg second-arg)
            (recur (+ instruction-pointer 4)
                   (set-third-arg 1) base)
            (recur (+ instruction-pointer 4)
                   (set-third-arg 0) base))
        ;; equals op1 op2 out
        8 (if (= first-arg second-arg)
            (recur (+ instruction-pointer 4)
                   (set-third-arg 1) base)
            (recur (+ instruction-pointer 4)
                   (set-third-arg 0) base))
        ;; adjust-base inc-base
        9 (recur (+ instruction-pointer 2) state (+ base first-arg))
        ;; halt
        99 state))))

(defn solution []
  (let [code (vec (get-code))
        in (chan 1)
        out (chan 1)
        _ (interpreter code in out)
        _ (go (>! in 1))]
    (<!! out)))

(defn solution2 []
  (let [code (vec (get-code))
        in (async/to-chan [2])
        out (chan 1)
        _ (interpreter code in out)]
    (<!! out)))
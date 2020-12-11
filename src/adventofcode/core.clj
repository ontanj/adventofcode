(ns adventofcode.core
  (:require [clojure.string :as str]))

(def line-split
  #(str/split %1 #"\n"))

(defn in? 
  "true if coll contains elm"
  [coll elm]
  (some #(= elm %) coll))

; task 1
(defn find-sum
  [coll sum]
  (when (not (empty? coll)) 
    (let [val (first coll)
          neg (- sum val)]
      (if (in? (rest coll) neg)
        [val neg]
        (recur (rest coll) sum)))))

(defn verify-find-sum
  [values coll sum]
  (and (= sum (apply + values))
       (every? identity (map (partial in? coll) values))))

(defn apply-task-1
  [subtask]
  (let [sum 2020
        data (map read-string (line-split (slurp "inputs/input1")))
        results (subtask data sum)]
    (when (verify-find-sum results data sum)
      (apply * results))))

(defn t1-1
  []
  (apply-task-1 find-sum))

(defn find-three-sum
  ([coll sum]
   (when (not (empty? coll))
     (let [val (first coll)
           neg (- sum val)]
       (if-let [results (find-sum (rest coll) neg)]
         (cons val results)
         (recur (rest coll) sum)))))
  ([val1 coll sum]
   (when-let [vals (find-sum coll (- sum val1))]
     vals)))

(defn t1-2
  []
  (apply-task-1 find-three-sum))

; task 2
(defn password-checker
  [acc inp]
  (let [[count letter-col passw] (str/split inp #" ")
        [low up] (map read-string (str/split count #"-"))
        letter (first letter-col)
        occurances (reduce #(if (= letter %2) (inc %1) %1) 0 passw)]
    (if (and (<= occurances up) (>= occurances low)) (inc acc) acc)))

(defn t2-1
  []
  (let [data (line-split (slurp "inputs/input2"))]
    (reduce password-checker 0 data)))

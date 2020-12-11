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

(defn t1-1
  []
  (let [sum 2020
        data (map read-string (line-split (slurp "inputs/input1")))
        results (find-sum data sum)]
    (when (verify-find-sum results data sum)
      (apply * results))))

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
  (let [sum 2020
        data (map read-string (line-split (slurp "inputs/input1")))
        results (find-three-sum data sum)]
    (when (verify-find-sum results data sum)
      (apply * results))))

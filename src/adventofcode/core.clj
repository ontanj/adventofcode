(ns adventofcode.core
  (:require [clojure.string :as str]))

(def line-split
  #(str/split %1 #"\n"))

(def d-line-split
  #(str/split %1 #"\n\n"))

(defn in? 
  "true if coll contains elm"
  [coll elm]
  (some #(= elm %) coll))

(defn incl-between
  [v1 v2 v3]
  (and (<= v1 v2) (<= v2 v3)))

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
(defn password-checker-occurances
  [indices letter passw acc]
  (let [[low up] indices
        occurances (reduce #(if (= letter %2) (inc %1) %1) 0 passw)]
    (if (and (<= occurances up) (>= occurances low)) (inc acc) acc)))

(defn password-checker
  [checker acc inp]
  (let [[count letter-col passw] (str/split inp #" ")
        indices (map read-string (str/split count #"-"))
        letter (first letter-col)]
    (checker indices letter passw acc)))

(defn apply-password-checker
  [checker]
  (let [data (line-split (slurp "inputs/input2"))]
    (reduce (partial password-checker checker) 0 data)))

(def t2-1
  (partial apply-password-checker password-checker-occurances))

(defn password-checker-exact
  [indices letter passw acc]
  (if (= 1 (reduce #(if (= letter (nth passw (dec %2))) (inc %1) %1) 0 indices))
    (inc acc)
    acc))

(def t2-2
  (partial apply-password-checker password-checker-exact))

; task 3
(defn tree-encounter?
  [line pos]
  (= \# (nth line pos)))

(defn tree-counter
  [acc [line pos]]
  (if (tree-encounter? line pos) (inc acc) acc))

(defn run-hill
  [right down]
  (let [data (line-split (slurp "inputs/input3"))
        size (count (first data))
        positions (map #(mod %1 size) (iterate (partial + (/ right down)) 0))
        rows (keep-indexed #(when (= 0 (mod %1 down)) %2) (map vector data positions))]
    (reduce tree-counter 0 rows)))

(def t3-1
  (partial run-hill 3 1))

(def t3-2
  (let [try [[1 1] [3 1] [5 1] [7 1] [1 2]]]
    (apply * (map #(apply run-hill %1) try))))

; task 4
(defn passports
  []
  (str/split (slurp "inputs/input4") #"\n\n"))

(def mandatory-fields
  ["byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"])

(defn validate-passport
  [passport-string]
  (let [size (count mandatory-fields)]
    (= size (reduce #(if (.contains passport-string %2) (inc %1) %1)
                  0 mandatory-fields))))

(defn validate-passports
  [passports validator]
  (reduce #(if %2 (inc %1) %1) 0 (map validator passports)))

(defn t4-1
  []
  (validate-passports (passports) validate-passport))

(defn fetch-field
  [field passport]
  (second (re-find (re-pattern (str field ":(.*?)(?:$|[\n ])")) passport)))

(defn validate-height
  [height-val]
  (when-let [[_ height-string unit] (re-find #"^(\d+)(cm|in)$" height-val)]
    (let [height (Integer. height-string)]
      (if (= "cm" unit)
        (incl-between 150 height 193)
        (incl-between 59 height 76)))))

(def validators
  {"byr" #(incl-between 1920 (read-string %) 2020)
   "iyr" #(incl-between 2010 (read-string %) 2020)
   "eyr" #(incl-between 2020 (read-string %) 2030)
   "hgt" validate-height
   "hcl" #(re-find #"^#[0-9a-f]{6}$" %)
   "ecl" #(in? ["amb" "blu" "brn" "gry" "grn" "hzl" "oth"] %)
   "pid" #(re-find #"^\d{9}$" %)})

(defn validate-strict
  [passport-string]
  (let [size (count validators)]
    (= size (reduce (fn [acc [key validator]]
                      (if-let [_ (some->> passport-string
                                          (fetch-field key)
                                          validator)]
                        (inc acc)
                        acc))
                    0 validators))))

(defn t4-2
  []
  (validate-passports (passports) validate-strict))

; task 5
(defn seats
  []
  (line-split (slurp "inputs/input5")))

(defn seat-to-id
  [seat]
  (->
   seat
   (str/replace #"[RB]" "1")
   (str/replace #"[LF]" "0")
   (Integer/parseInt 2)))

(defn t5-1
  []
  (reduce max 0 (map seat-to-id (seats))))

(defn detect-missing
  [vals]
  (if (= (inc (first vals)) (second vals))
    (recur (rest vals))
    (inc (first vals))))

(defn t5-2
  []
  (detect-missing (sort (map seat-to-id (seats)))))

; task 6
(defn declaration-forms
  []
  (d-line-split (str/trim (slurp "inputs/input6"))))

(defn count-unique-letters-except-newline
  [string]
  (->>
   string
   set
   (remove #(= \newline %1))
   count))

(defn t6-1
  []
  (reduce #(+ %1 (count-unique-letters-except-newline %2)) 0 (declaration-forms)))

; count occurances of value (char or string) in string
(defn count-occurances
  [string value]
  (-> value
      str
      re-pattern
      (re-seq string)
      count))

(defn count-lines
  [string]
  (inc (count-occurances string \newline)))

(defn count-by-all
  ([string] (count-by-all 0 (count-lines string) (str/replace string "\n" "")))
  ([counter group-size string]
   (if (not (empty? string))
     (let [letter (first string)
           rest-string (str/replace string (str letter) "")]
       (if (= group-size (count-occurances string letter))
         (recur (inc counter) group-size rest-string)
         (recur counter group-size rest-string)))
     counter)))

(defn t6-2
  []
  (reduce + 0 (map count-by-all (declaration-forms))))

; task 7
(defn bag-rule-to-map
  [rule]
  (let [container (second (re-find #"(.*?) bags contain" rule))
        content (map (fn [[_ nbr type]] [(Integer.  nbr) type]) (re-seq #"(\d+?) (.*?) bag" rule))]
    [container content]))

(defn bag-map
  []
  (into {} (map bag-rule-to-map (line-split (slurp "inputs/input7")))))

(defn find-bag-containers
  [bag-map bag]
  (map first (filter #(in? (map second (second %)) bag) bag-map)))

(defn remove-bag
  [bag-map bag]
  (into {} (remove #(= bag (first %)) bag-map)))

(defn bag-containers
  [counter bag-map bags]
  (if (empty? bags)
    counter
    (let [containers (find-bag-containers bag-map (first bags))]
      (recur (+ counter (count containers))
             (reduce remove-bag bag-map containers)
             (concat (rest bags) containers)))))

(defn t7-1
  []
  (bag-containers 0 (bag-map) ["shiny gold"]))

(defn update-bag-count
  [multiplier bags [counter type]]
  (let [contrib (* multiplier counter)]
    (update bags type
            #(if % (+ % contrib) contrib))))

(defn bag-contents
  [counter bag-map old-bags]
  (let [[bag q] (first old-bags)
        new-count (+ q counter)
        new-bags (reduce (partial update-bag-count q)
                         (remove-bag old-bags bag)
                         (get bag-map bag))]
    (if (empty? new-bags)
      new-count
      (recur new-count
             bag-map
             new-bags))))

(defn t7-2
  []
  (dec (bag-contents 0 (bag-map) {"shiny gold" 1})))

; task 8
(defn interpret-instruction
  [instruction]
  (let [[_ inst val] (re-find #"^(\w{3}) \+?(.*)$" instruction)]
          [inst (Integer. val)]))

(defn instructions
  []
  (map interpret-instruction
       (line-split (slurp "inputs/input8"))))

(defn evaluate-instructions
  [instructions line acc visited]
  (if (contains? visited line)
    acc
    (let [[inst val] (nth instructions line)
          visited (conj visited line)]
      (condp = inst
        "acc" (recur instructions (inc line) (+ val acc) visited)
        "jmp" (recur instructions (+ val line) acc visited)
        "nop" (recur instructions (inc line) acc visited)))))

(defn t8-1
  []
  (evaluate-instructions (instructions) 0 0 #{}))

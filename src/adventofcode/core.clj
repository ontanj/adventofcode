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
  (-> string
      (count-occurances \newline)
      inc))

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
    [false acc]
    (if (= line (count instructions))
      [true acc]
      (let [[inst val] (nth instructions line)
            visited (conj visited line)]
        (condp = inst
          "acc" (recur instructions (inc line) (+ val acc) visited)
          "jmp" (recur instructions (+ val line) acc visited)
          "nop" (recur instructions (inc line) acc visited))))))

(defn init-program
  [instructions]
  (evaluate-instructions instructions 0 0 #{}))

(defn t8-1
  []
  (second (init-program (instructions))))

(defn try-change-instructions
  [instructions line]
  (if (= line (count instructions))
    nil
    (let [[inst val] (nth instructions line)]
      (condp = inst
        "acc" (recur instructions (inc line))
        "jmp" (let [[pred res] (init-program
                                (assoc (into [] instructions)
                                       line ["nop" val]))]
                (if pred res (recur instructions (inc line))))
        "nop" (let [[pred res] (init-program
                                (assoc (into [] instructions)
                                       line ["jmp" val]))]
                (if pred res (recur instructions (inc line))))))))

(defn t8-2
  []
  (try-change-instructions (instructions) 0))

; task 9
(defn value-stream
  []
  (map #(Integer. %) (line-split (slurp "inputs/input9"))))

(defn validate-sum
  [preamble val]
  (if (empty? preamble)
    false
    (let [wanted (- val (first preamble))]
      (if (in? (rest preamble) wanted)
        true
        (recur (rest preamble) val)))))

(defn validate-all
  [preamble input]
  (if (validate-sum preamble (first input))
    (recur (conj (into [] (rest preamble)) (first input)) (rest input))
    (first input)))

(defn t9-1
  [pre-size]
  (let [stream (value-stream)]
    (validate-all (take pre-size stream) (drop pre-size stream))))

(defn verify-sum
  [input acc-sum desired low high]
  (let [current (first input)
        acc-sum (+ current acc-sum)
        low (min current low)
        high (max current high)]
    (cond
      (> acc-sum desired) nil
      (= acc-sum desired) [low high]
      :else (recur (rest input) acc-sum desired low high))))

(defn find-sum
  [input desired]
  (let [val (first input)]
    (if-let [interval (verify-sum (rest input) (first input) desired val val)]
      interval
      (recur (rest input) desired))))

(defn t9-2
  [pre-size]
  (apply + (find-sum (value-stream) (t9-1 pre-size))))

; task 10
(defn adapters
  []
  (sort (map #(Integer/parseInt %) (line-split (slurp "inputs/input10")))))

(defn count-differences
  [[one-count three-count last-val] new-val]
  (condp = (- new-val last-val)
      1 [(inc one-count) three-count new-val]
      3 [one-count (inc three-count) new-val]
      :else [one-count three-count new-val]))

(defn adapter-differences
  []
  (let [[one-count three-count _] (reduce count-differences [0 0 0] (adapters))]
    [one-count (inc three-count)]))

(defn t10-1
  []
  (apply * (adapter-differences)))

(defn update-adapter
  [prev current]
  (assoc prev current (reduce #((fnil + 0) (get prev %2) %1) 0 (range (- current 3) current))))

(defn count-adapter-possibilities
  [adapters]
  (reduce update-adapter {0 1} adapters))

(defn t10-2
  []
  (let [adapters (adapters)
        built-in (+ 3 (last adapters))
        adapters-built (conj (into [] adapters) built-in)]
    (get (count-adapter-possibilities adapters-built) built-in)))

; task 11
(defn seats
  []
  (slurp "inputs/input11"))

(defn seat-at
  [seats pos]
  (nth seats pos))

(defn right-most
  [pos width height]
  (= (mod (+ 2 pos) width) 0))

(defn left-most
  [pos width height]
  (= (mod pos width) 0))

(defn up-most
  [pos width height]
  (< pos width))

(defn down-most
  [pos width height]
  (>= pos (* width (dec height))))

(def remove-non-exist
  [(fn [poss pos width height]
     (if (right-most pos width height)
       (reduce dissoc poss [2 4 7]) poss)) ; remove right
   (fn [poss pos width height]
     (if (left-most pos width height)
       (reduce dissoc poss [0 3 5]) poss)) ; remove left
   (fn [poss pos width height]
     (if (up-most pos width height)
       (reduce dissoc poss [0 1 2]) poss)) ; remove above
   (fn [poss pos width height]
     (if (down-most pos width height)
       (reduce dissoc poss [5 6 7]) poss))]) ; remove below

(def directions
  (list #(- %1 %2 1)
        #(- %1 %2)
        #(- %1 %2 -1)
        (fn [pos _] (dec pos))
        (fn [pos _] (inc pos))
        #(+ %1 %2 -1)
        #(+ %1 %2)
        #(+ %1 %2 1)))


(defn neighboring-positions
  [pos width height]
  (reduce #(%2 %1 pos width height)
          (into {}
                (map vector
                     (range 8)
                     (map #(%1 pos width) directions)))
          remove-non-exist))

(defn count-adjacent-people
  [seats pos width height]
  (reduce + (map #(if (= \# (seat-at seats (second %))) 1 0)
                 (neighboring-positions pos width height))))

(defn update-seat
  [seats pos width height count-func nbr]
  (condp = (seat-at seats pos)
    \# (if (<= nbr (count-func seats pos width height)) \L \#)
    \L (if (= 0 (count-func seats pos width height)) \# \L)
    \. \.
    \newline \newline))

(defn update-all-seats
  [seats width height count-func nbr]
  (reduce #(str %1 (update-seat seats %2 width height count-func nbr)) "" (range (count seats))))

(defn update-til-stable
  [seats width height count-func nbr]
  (let [new-seats (update-all-seats seats width height count-func nbr)]
    (if (= new-seats seats)
      seats
      (recur new-seats width height count-func nbr))))

(defn count-stable-seats
  [count-func nbr]
  (let [seats (seats)
        width (count (re-find #".*?\n" seats))
        height (/ (count seats) width)]
    (count (re-seq #"#" (update-til-stable seats width height count-func nbr)))))

(defn t11-1
  []
  (count-stable-seats count-adjacent-people 4))

(defn neighboring-directions
  [pos width height]
  (reduce #(%2 %1 pos width height)
          (into {}
                (map vector
                     (range 8)
                     directions))
          remove-non-exist))

(def borders
  (list right-most left-most up-most down-most))

(defn border-seat
  [pos width height]
  (some #(% pos width height) borders))

(defn is-outside
  [pos width height]
  (or (< pos 0) (>= pos (* width height)) (= 0 (mod (inc pos) width))))

(defn search-occupied
  [seats pos width height direction]
  (let [next-pos (direction pos width)]
    (cond
      (is-outside next-pos width height) 0
      (= (seat-at seats next-pos) \L) 0
      (= (seat-at seats next-pos) \#) 1
      :else (recur seats next-pos width height direction))))

(defn count-adjacent-people-far
  [seats pos width height]
  (reduce + (map #(search-occupied seats pos width height (second %)) (neighboring-directions pos width height))))

(defn t11-2
  []
  (count-stable-seats count-adjacent-people-far 5))

; task 12
(defn nav-instructions
  []
  (-> "inputs/input12" slurp line-split))

(defn circulate-right
  ([direction _] (circulate-right direction))
  ([direction]
   (case direction
     "N" "E" "E" "S" "S" "W" "W" "N")))

(defn ride-instruction
  [status instruction [n s e w l r f :as mov]]
  (let [[_ action value-string] (re-find #"^(\w)(\d*)$" instruction)
        value (Integer. value-string)]
    (case action
      "N" (n status value)
      "S" (s status value)
      "E" (e status value)
      "W" (w status value)
      "L" (l status value)
      "R" (r status value)
      "F" (f status value mov))))

(def boat-movements
  (list
   #(update %1 :y + %2)
   #(update %1 :y - %2)
   #(update %1 :x + %2)
   #(update %1 :x - %2)
   (fn [status value] (update status :dir
                              #(reduce circulate-right
                                       % (range (* 3 (/ value 90))))))
   (fn [status value] (update status :dir
                              #(reduce circulate-right
                                       % (range (/ value 90)))))
   #(ride-instruction %1 (str (:dir %1) %2) %3)))

(defn ride-boat
  [status instructions op-set]
  (reduce #(ride-instruction %1 %2 op-set) status instructions))

(defn t12-1
  []
  (let [status (ride-boat {:x 0 :y 0 :dir "E"} (nav-instructions) boat-movements)]
    (+ (Math/abs (:x status)) (Math/abs (:y status)))))

(defn rotate-wp
  [{wp-x :wp-x wp-y :wp-y x :x y :y :as status} steps]
  (let [new-status (-> status
                       (assoc :wp-x (- wp-y))
                       (assoc :wp-y (+ wp-x)))]
    (if (= steps 1)
      new-status
      (recur new-status (dec steps)))))

(defn forward-wp
  [status value movements]
  (let [x-move (* value (:wp-x status))
        y-move (* value (:wp-y status))]
    (-> status
        (update :x + x-move)
        (update :y + y-move))))

(def wp-movements
  (list
   #(update %1 :wp-y + %2)
   #(update %1 :wp-y - %2)
   #(update %1 :wp-x + %2)
   #(update %1 :wp-x - %2)
   #(rotate-wp %1 (/ %2 90))
   #(rotate-wp %1 (* 3 (/ %2 90)))
   forward-wp))

(defn t12-2
  []
  (let [status (ride-boat {:x 0 :y 0 :dir "E" :wp-x 10 :wp-y 1} (nav-instructions) wp-movements)]
    (+ (Math/abs (:x status)) (Math/abs (:y status)))))

; task 13
(defn bus-times
  []
  (let [[depart-time bus-times-with-x] (line-split (slurp "inputs/input13"))
        depart-int (Integer. depart-time)
        bus-times (map #(Integer. %) (re-seq #"\d+" bus-times-with-x))]
    (list depart-int bus-times)))

(defn check-divisor
  [value divisors]
  (cond 
    (empty? divisors) nil
    (= 0 (mod value (first divisors))) (first divisors)
    :else (recur value (rest divisors))))

(defn search-divisor
  [start-value divisors]
  (if-let [div (check-divisor start-value divisors)]
    [start-value div]
    (recur (inc start-value) divisors)))

(defn t13-1
  []
  (let [[depart times] (bus-times)
        [found-depart found-bus] (search-divisor depart times)]
    (* found-bus (- found-depart depart))))

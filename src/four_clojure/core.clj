(ns four-clojure.core)

;; From https://gist.github.com/thegeez/8352754
;; 1. Common test utility
(require '[clojure.walk :as walk])

(defmacro to-test [name & source]
  `(defn ~(symbol (str name "-test")) [] ~@(for [case (filter list? source)]
                                             `(assert ~(walk/postwalk-replace {'__ name} case)))))

;; 2. for each problem I write:
;; (def pNN
  ;; (fn ...solution here...))

;; 3. type (to-test solution-fn-name  )
;; 4. copy paste the problem test cases from 4clojure into the to-test
;; form (the text will include "test not run")
;; example problem 21:

;; (to-test pNN
;;          (= (__ 2 [1 2 3 4 5]) '(3 4 5 1 2))
;;          test not run	
;;          (= (__ -2 [1 2 3 4 5]) '(4 5 1 2 3))
;;          test not run	
;;          (= (__ 6 [1 2 3 4 5]) '(2 3 4 5 1))
;;          test not run	
;;          (= (__ 1 '(:a :b :c)) '(:b :c :a))
;;          test not run	
;;          (= (__ -4 '(:a :b :c)) '(:c :a :b))
;;          )

;; 5. run (pNN-test) to run all the created assertions of the test
;; case
;; 6. edit pNN and test again
;; 7. (pNN-test) returns nil for succes!
;; 8. copy the (fn ...) body from pNN into 4clojure

;; #(first (reverse %)) is more idiomatic/clear and also still linear
;; don't get fancy/verbose when it doesn't serve
(def p19
  (fn [coll]
    (loop [[first & rest] coll]
      (if (empty? rest)
        first
        (recur rest)))))

(to-test p19
         (= (__ [1 2 3 4 5]) 5)
         (= (__ '(5 4 3)) 3)
         (= (__ ["b" "c" "d"]) "d")
         )

(def p26
  (fn [x]
    (letfn
        [(next-fib [[n-1 n-2]]
           [(+ n-1 n-2) n-1])]
      (take x (map first (iterate next-fib [1 0]))))))

(to-test p26
         (= (__ 3) '(1 1 2))
         (= (__ 6) '(1 1 2 3 5 8))
         (= (__ 8) '(1 1 2 3 5 8 13 21))
         )

(defn fibonacci [x]
  (letfn
      [(next-fib [[n-1 n-2]]
         [(+ n-1 n-2) n-1])]
    (take x (map first (iterate next-fib [1 0])))))

(defn flatten2 [arg]
  (letfn
      [(flattener [node]
         (cond
           (sequential? node) (mapcat flattener node)
           :else (list node)))]
    (flattener arg))
  )

;; Write a function which reverses the interleave process into x number of subsequences.
(def p43
  (fn [seq-in num-partitions]
    (let [vector-vector (into [] (repeat num-partitions []))]
      (map sequence (first (reduce (fn [[vec-vec counter] x]
                (let [index (mod counter num-partitions)]
                  [(assoc vec-vec index (conj (get vec-vec index) x)) (inc counter)]))
              (vector vector-vector 0)
              seq-in))))))

(def p43-1 [[1 2 3 4 5 6] 2])
(to-test p43
         (= (__ [1 2 3 4 5 6] 2) '((1 3 5) (2 4 6))))
(p43 [1 2 3 4 5 6] 2)

(def p44
  (fn [shift s]
    (let [length (count s)
          drop-length (if (pos? shift) shift (+ length (mod length shift)))]
      (take length (drop drop-length (cycle s))))))

(defn rotate [shift s]
  (let [length (count s)
        drop-length (if (pos? shift) shift (+ length (mod length shift)))]
    (concat (drop drop-length s) (take drop-length s))))

;; Write a function which takes a sequence consisting of items with different types and splits them up into
;; a set of homogeneous sub-sequences. The internal order of each sub-sequence should be maintained, but the
;; sub-sequences themselves can be returned in any order (this is why 'set' is used in the test cases).
(def p50
  (fn [v]
    (->> v
         (reduce (fn [type->vals x]
                   (update-in type->vals
                              (list (type x))
                              #((fnil conj []) % x)))
                 {})
         (vals))))
;; lol just use group-by
(def p50-2
  (fn [v]
    (vals (group-by type v))))

;; Given a vector of integers, find the longest consecutive sub-sequence of increasing numbers.
;; If two sub-sequences have the same length, use the one that occurs first. An increasing
;; sub-sequence must have a length of 2 or greater to qualify.
(def p53
  (fn [v]
    (:longest-subsequence (reduce (fn [{:keys [last-subsequence longest-subsequence] :as m} value]
                                    (let [current-subsequence (if (> value (get m :last-value value))
                                                                (conj last-subsequence value)
                                                                [value])
                                          larger-subsequence (max-key count current-subsequence longest-subsequence)]
                                      {:last-value value
                                       :last-subsequence current-subsequence
                                       :longest-subsequence (if (<= 2 (count larger-subsequence)) larger-subsequence [])}))
                                  {:current-subsequence [] :longest-subsequence []}
                                  v))))

(def p54
  (fn [n coll]
    (first (reduce (fn [[output next-partition] value]
                     (if (>= (inc (count next-partition)) n)
                       [(conj output (conj next-partition value)) []]
                       [output (conj next-partition value)]))
                   [[] []]
                   coll))))

(def p55
  (fn [coll]
    (reduce (fn [val->freq value]
              (update val->freq value (fnil inc 0)))
            {}
            coll)))

(def p56
  (fn [coll]
    (first (reduce (fn [[output seen :as acc] value]
                     (if (contains? seen value)
                       acc
                       [(conj output value) (conj seen value)]))
                   [[] #{}]
                   coll))))

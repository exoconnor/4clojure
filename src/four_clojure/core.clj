(ns four-clojure.core)

;; Write a function which reverses the interleave process into x number of subsequences.
(def p43
  (fn [coll n]
    (first (reduce (fn [[output counter] x]
                     (let [index (mod counter n)]
                       [(assoc output index (conj (get output index) x)) (inc counter)]))
                   [(into [] (repeat n [])) 0]
                   coll))))
;; I did not understand the true power of apply
(def p43-2 #(apply map list (partition %2 %1)))

;; Write a function which can rotate a sequence in either direction.
(def p44
  (fn [shift s]
     (let [length (count s)
           drop-length (if (pos? shift)
                         shift
                         (+ length (mod shift length)))]
       (take length (drop drop-length (cycle s))))))
;; (concat (drop drop-length s) (take drop-length s))
;; also works (although need to rename drop length var)
;; I like the explicit taking of length but neither is obv.
;; better to me

;; Write a higher-order function which flips the order of the arguments of an input function.
(def p46 #(fn [& args] (apply % (reverse args))))

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
(def p50-2 (fn [v] (vals (group-by type v))))

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

;; Write a function which returns a sequence of lists of x items each. Lists of less than x items should not be returned.
(def p54
  (fn [n coll]
    (first (reduce (fn [[output next-partition] value]
                     (if (>= (inc (count next-partition)) n)
                       [(conj output (conj next-partition value)) []]
                       [output (conj next-partition value)]))
                   [[] []]
                   coll))))

;; Write a function which returns a map containing the number of occurences of each distinct item in a sequence.
(def p55
  (fn [coll]
    (reduce (fn [val->freq value]
              (update val->freq value (fnil inc 0)))
            {}
            coll)))
;; 4Clojure uses version < 1.7, in which case you have to use update-in

;; Write a function which removes the duplicates from a sequence. Order of the items must be maintained.
(def p56
  (fn [coll]
    (first (reduce (fn [[output seen :as acc] value]
                     (if (contains? seen value)
                       acc
                       [(conj output value) (conj seen value)]))
                   [[] #{}]
                   coll))))

;; Write a function which allows you to create function compositions.
;; The parameter list should take a variable number of functions, and create a function that applies them from right-to-left.
(def p58
  (fn [& funcs]
    (let [right-to-left (reverse funcs)]
      (fn [& args]
        (reduce (fn [acc func] (func acc)) (apply (first right-to-left) args) (rest right-to-left))))))

;; Take a set of functions and return a new function that takes a variable number of arguments and
;; returns a sequence containing the result of applying each function left-to-right to the argument list.
(def p59
  (fn [& funcs]
    (fn [& args] (map #(apply % args) funcs))))

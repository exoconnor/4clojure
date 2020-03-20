(ns four-clojure.core)

;; Write a function which reverses the interleave process into x number of subsequences.
(def p43
  (fn [xs n]
    (first (reduce (fn [[output counter] x]
                     (let [index (mod counter n)]
                       [(assoc output index (conj (get output index) x)) (inc counter)]))
                   [(into [] (repeat n [])) 0]
                   xs))))
;; I did not understand the true power of apply
(def p43-2 #(apply map list (partition %2 %1)))

;; Write a function which can rotate a sequence in either direction.
(def p44
  (fn [n xs]
     (let [length (count xs)
           drop-length (if (pos? n) n (+ length (mod n length)))]
       (take length (drop drop-length (cycle xs))))))
;; (concat (drop drop-length xs) (take drop-length xs))
;; also works (although need to rename drop length var)
;; I like the explicit taking of length but neither is obv.
;; better to me

;; Write a higher-order function which flips the order of the arguments of an input function.
(def p46 #(fn [& args] (apply % (reverse args))))

;; Write a function which takes a sequence consisting of items with different types and splits them up into
;; a set of homogeneous sub-sequences. The internal order of each sub-sequence should be maintained, but the
;; sub-sequences themselves can be returned in any order (this is why 'set' is used in the test cases).
(def p50
  (fn [xs]
    (->> xs
         (reduce (fn [type->vals x]
                   (update-in type->vals
                              (list (type x))
                              #((fnil conj []) % x)))
                 {})
         (vals))))
;; lol just use group-by
(def p50-2 (fn [xs] (vals (group-by type xs))))

;; Given a vector of integers, find the longest consecutive sub-sequence of increasing numbers.
;; If two sub-sequences have the same length, use the one that occurs first. An increasing
;; sub-sequence must have a length of 2 or greater to qualify.
(def p53
  (fn [xs]
    (:longest-subsequence (reduce (fn [{:keys [last-subsequence longest-subsequence] :as m} x]
                                    (let [current-subsequence (if (< (get m :last-value x) x)
                                                                (conj last-subsequence x)
                                                                [x])
                                          larger-subsequence (max-key count current-subsequence longest-subsequence)]
                                      {:last-value x
                                       :last-subsequence current-subsequence
                                       :longest-subsequence (if (<= 2 (count larger-subsequence)) larger-subsequence [])}))
                                  {:current-subsequence [] :longest-subsequence []}
                                  xs))))

;; Write a function which returns a sequence of lists of x items each. Lists of less than x items should not be returned.
(def p54
  (fn [n xs]
    (first (reduce (fn [[output next-partition] x]
                     (if (<= n (inc (count next-partition)))
                       [(conj output (conj next-partition x)) []]
                       [output (conj next-partition x)]))
                   [[] []]
                   xs))))

;; Write a function which returns a map containing the number of occurences of each distinct item in a sequence.
(def p55
  (fn [xs]
    (reduce (fn [x->freq x]
              (update x->freq x (fnil inc 0)))
            {}
            xs)))
;; 4Clojure uses version < 1.7, in which case you have to use update-in

;; Write a function which removes the duplicates from a sequence. Order of the items must be maintained.
(def p56
  (fn [xs]
    (first (reduce (fn [[output seen :as output+seen] x]
                     (if (contains? seen x)
                       output+seen
                       [(conj output x) (conj seen x)]))
                   [[] #{}]
                   xs))))

;; Write a function which allows you to create function compositions.
;; The parameter list should take a variable number of functions, and create a function that applies them from right-to-left.
(def p58
  (fn [& fs]
    (let [right-to-left (reverse fs)]
      (fn [& args]
        (reduce (fn [acc func] (func acc)) (apply (first right-to-left) args) (rest right-to-left))))))

;; Take a set of functions and return a new function that takes a variable number of arguments and
;; returns a sequence containing the result of applying each function left-to-right to the argument list.
(def p59
  (fn [& fs]
    (fn [& args] (map #(apply % args) fs))))

;; Write a function which behaves like reduce, but returns each intermediate value of the reduction.
;; Your function must accept either two or three arguments, and the return sequence must be lazy.
(def p60
  (fn reductions2
    ([f xs] (reductions2 f (first xs) (rest xs)))
    ([f init xs]
     (let [reducer
           (fn reducer [xs acc]
             (if (empty? xs)
               (cons acc nil)
               (cons acc (lazy-seq (reducer (rest xs) (f acc (first xs)))))))]
       (reducer xs init)))))

;; Given a side-effect free function f and an initial value x write a function which returns an infinite lazy sequence of
;; x, (f x), (f (f x)), (f (f (f x))), etc.
(def p62
  (fn iter [f x] (cons x (lazy-seq (iter f (f x))))))

;; Given a function f and a sequence s, write a function which returns a map.
;; The keys should be the values of f applied to each item in s.
;; The value at each key should be a vector of corresponding items in the order they appear in s.
(def p63
  (fn [f xs]
    (reduce
     (fn [m x] (update-in m (list (f x)) #((fnil conj []) % x)))
     {}
     xs)))


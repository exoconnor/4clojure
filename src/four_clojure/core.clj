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

;; Clojure has many sequence types, which act in subtly different ways. The core
;; functions typically convert them into a uniform "sequence" type and work with
;; them that way, but it can be important to understand the behavioral and
;; performance differences so that you know which kind is appropriate for your
;; application.

;; Write a function which takes a collection and returns one of :map, :set, :list,
;; or :vector - describing the type of collection it was given. You won't be
;; allowed to inspect their class or use the built-in predicates like list? - the
;; point is to poke at them and understand their behavior.
;; NOTE - can't use class, type, Class, vector?, sequential?, list?, seq?, map?, set?, instance?, getClass
(def p65
  (fn [coll]
    (let [a (gensym)
          b (gensym)
          test-case (conj (empty coll) [a b] [a a] [a a])
          test-size (count test-case)]
      (cond
        (= 1 test-size) :map
        (= 2 test-size) :set
        (= (first test-case) [a a]) :list
        :else :vector))))

;; Given two integers, write a function which returns the greatest common divisor.
(def p66
  (fn [a b]
    (let [start (int (/ (max a b) 2))
          divisor? (fn [n d] (zero? (mod n d)))]
      (some #(and (and (divisor? a %) (divisor? b %)) %) (range start 0 -1)))))
;; Euclidean Algorithm is clearly better but now's not the time for a math tangent

(defn primes
  "Lazy prime generator based on https://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf"
  ([] (let [wheel2357
            (cycle '(2 4 2 4 6 2 6 4 2 4 6 6 2 6 4 2 6 4 6 8 4 2 4 2 4 8 6 4 6 2 4 6 2 6 6 4 2 4 6 2 6 4 2 4 2 10 2 10))
            candidates (reductions + 11 wheel2357)
            primeq (java.util.PriorityQueue. 32 (comparator (fn [a b] (< (first a) (first b)))))]
        (concat '(2 3 5 7) (primes candidates primeq))))
  ([candidates primeq]
   (let [c (first candidates)
         cs (drop 1 candidates)
         ;; Update primeq via java mutation while determining if c is prime
         c-prime? (reduce (fn [p? p-iter]
                            (.add primeq (drop-while #(<= % c) p-iter))
                            (and p? (not= (first p-iter) c)))
                          true
                          (take-while #(and % (<= (first %) c))
                                      (repeatedly (fn []
                                                    (let [p (.peek primeq)]
                                                      (cond
                                                        (nil? p) nil
                                                        (< c (first p)) nil
                                                        :else (.poll primeq)))))))]
     (if c-prime?
       ;; c is prime, create iterator and place it at next unmarked composite c^2, seq on
       (do
         (.add primeq (iterate #(+ % (* 2 c)) (* c c)))
         (cons c (lazy-seq (primes cs primeq))))
       (recur cs primeq)))))

;; Repeat prime form for copy pasting into 4clojure
(def p67
  (fn [n]
    (let [primes (fn primes
                   ([] (let [wheel2357
                             (cycle '(2 4 2 4 6 2 6 4 2 4 6 6 2 6 4 2 6 4 6 8 4 2 4 2 4 8 6 4 6 2 4 6 2 6 6 4 2 4 6 2 6 4 2 4 2 10 2 10))
                             candidates (reductions + 11 wheel2357)
                             primeq (java.util.PriorityQueue. 32 (comparator (fn [a b] (< (first a) (first b)))))]
                         (concat '(2 3 5 7) (primes candidates primeq))))
                   ([candidates primeq]
                    (let [c (first candidates)
                          cs (drop 1 candidates)
                          ;; Update primeq via java mutation while determining if c is prime
                          c-prime? (reduce (fn [p? p-iter]
                                             (.add primeq (drop-while #(<= % c) p-iter))
                                             (and p? (not= (first p-iter) c)))
                                           true
                                           (take-while #(and % (<= (first %) c))
                                                       (repeatedly (fn []
                                                                     (let [p (.peek primeq)]
                                                                       (cond
                                                                         (nil? p) nil
                                                                         (< c (first p)) nil
                                                                         :else (.poll primeq)))))))]
                      (if c-prime?
                        ;; c is prime, create iterator and place it at next unmarked composite c^2, seq on
                        (do
                          (.add primeq (iterate #(+ % (* 2 c)) (* c c)))
                          (cons c (lazy-seq (primes cs primeq))))
                        (recur cs primeq)))))]
      (take n (primes)))))

;; Write a function which takes a function f and a variable number of maps. Your
;; function should return a map that consists of the rest of the maps conj-ed
;; onto the first. If a key occurs in more than one map, the mapping(s) from the
;; latter (left-to-right) should be combined with the mapping in the result by
;; calling (f val-in-result val-in-latter)
(def p69
  (fn [f & ms]
    (reduce (fn merge-entry [result [k v-latter]]
              (let [v-result (get result k)]
                (if (nil? v-result)
                  (assoc result k v-latter)
                  (assoc result k (f v-result v-latter)))))
            {}
            (mapcat seq ms))))

;; Write a function that splits a sentence up into a sorted list of words.
;; Capitalization should not affect sort order and punctuation should be
;; ignored.
(def p70
  (fn [s]
    (sort-by clojure.string/lower-case
             (re-seq #"[a-zA-Z]+" s))))

;; Analyze a tictactoe board
(def p73
  (fn [[r1 r2 r3 :as board]]
    (some (fn [[a b c]]
            (when (and (= a b c) (not= a :e)) a))
          (concat board
                  (map vector r1 r2 r3)
                  (list (map-indexed #(nth %2 %1) board))
                  (list (map-indexed #(nth %2 %1) (reverse board)))))))

;; Given a string of comma separated integers, write a function which returns a
;; new comma separated string that only contains the numbers which are perfect
;; squares.
(def p74
  (fn [s]
    (->> s
         (re-seq #"\d+")
         (filter (fn [digit-string]
                   (let [n (Integer/parseInt digit-string)
                         root (int (Math/sqrt n))]
                     (= n (* root root)))))
         (clojure.string/join ","))))

;; Euler's Totient Function
(def p75
  (fn euler-totient [x]
    (let [gcd (fn gcd [a b]
                (if (zero? b)
                  (Math/abs a)
                  (recur b (rem a b))))]
      (reduce (fn [count a]
                (if (= 1 (gcd a x))
                  (inc count)
                  count))
              1
              (range 2 x)))))

;; Write a function which finds all the anagrams in a vector of words. A word x
;; is an anagram of word y if all the letters in x can be rearranged in a
;; different order to form y. Your function should return a set of sets, where
;; each sub-set is a group of words which are anagrams of each other. Each
;; sub-set should have at least two words. Words without any anagrams should not
;; be included in the result.
(def p77
  (fn [ss]
    (->> ss
         (group-by #(frequencies %))
         (vals)
         (reduce (fn [outset words]
                   (if (< 1 (count words))
                     (conj outset (set words))
                     outset))
                 #{}))))

;; Reimplement trampoline
;; "Takes a function f and variable parameters, calls f with any
;; parameters that were supplied. If f returns a function, call with no
;; args, repeat until f not a function."
(def p78
  (fn [f & args]
    (let [bounce
          (fn bounce [b]
            (if (fn? b) (recur (b)) b))]
          (bounce (apply f args)))))

;; Write a function which calculates the sum of the minimal path through a
;; triangle. The triangle is represented as a collection of vectors. The path
;; should start at the top of the triangle and move to an adjacent number on the
;; next row until the bottom of the triangle is reached.
(def p79
  (fn [triangle]
    (apply min
           (reduce (fn [path-sums row]
                     (map (fn [from to] (+ (apply min from) to))
                          (partition 2 1 (concat [(first path-sums)]
                                                 path-sums
                                                 [(last path-sums)]))
                          row))
                   triangle))))

;; A number is "perfect" if the sum of its divisors equal the number itself. 6
;; is a perfect number because 1+2+3=6. Write a function which returns true for
;; perfect numbers and false otherwise.
(def p80
  (fn [n]
    (->> (inc (int (/ n 2)))
         (range 1)
         (filter #(zero? (mod n %)))
         (reduce +)
         (= n))))

;; Write a function which returns the intersection of two sets. The intersection
;; is the sub-set of items that each set has in common.
(def p81
  (fn [s1 s2]
    (reduce (fn [intersect el]
              (if (contains? s2 el)
                (conj intersect el)
                intersect))
            #{}
            s1)))
;; (comp set filter) works and is pretty dope -> use one set as a function, the other as
;; a sequence for the filter function, then cast back to set.

;; A word chain consists of a set of words ordered so that each word differs by
;; only one letter from the words directly before and after it. The one letter
;; difference can be either an insertion, a deletion, or a substitution. Here is
;; an example word chain: cat -> cot -> coat -> oat -> hat -> hot -> hog -> dog
;; Write a function which takes a sequence of words, and returns true if they
;; can be arranged into one continous word chain, and false if they cannot.
(def p82
  (fn [strings]
    ;; Brute force/recursive search? Levenshtein distance 1 sequence
    "DR STUB"))

;; Write a function which takes a variable number of booleans. Your function
;; should return true if some of the parameters are true, but not all of the
;; parameters are true. Otherwise your function should return false.
(def p83
  (fn [& xs]
    (let [truthiness (reduce (fn [c x] (if x (inc c) c)) 0 xs)]
      (and (< 0 truthiness) (not= truthiness (count xs))))))

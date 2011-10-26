(defn prime? [x]
  (loop [e (Math/sqrt x) i 2]
    (cond
     (> i e) true
     (zero? (rem x i)) false
     :else (recur e (inc i)))))

(defn sum-of-all-primes-below [n]
  (apply + (filter prime? (range 2 n))))

(println (sum-of-all-primes-below 2000000))
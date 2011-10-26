(defn prime? [x]
  (loop [e (Math/sqrt x) i 2]
    (cond
     (> i e) true
     (zero? (rem x i)) false
     :else (recur e (inc i)))))

(defn prime-factors [n]
  ((fn [x i acc]
     (cond
      (= x 1) (reverse acc)
      (and (prime? i) (zero? (mod x i))) (recur (/ x i) i (cons i acc))
      :else (recur x (inc i) acc))) n 2 '()))

(defn max-prime-factor [n]
  (apply max (prime-factors n)))

(println (max-prime-factor 600851475143))
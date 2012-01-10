(defn gcd [a b]
  (if (= b 0) a (recur b (rem a b))))

(defn lcm [a b]
  (/ (* a b) (gcd a b)))

(defn solve []
  (reduce lcm (range 1 21)))

(println (solve))
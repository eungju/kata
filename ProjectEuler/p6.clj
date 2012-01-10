(defn square [x]
  (* x x))

(defn sum-of-squares [coll]
  (reduce + (map square coll)))

(defn square-of-sums [coll]
  (square (reduce + coll)))

(defn solve []
  (let [coll (range 1 101)]
    (Math/abs (- (sum-of-squares coll) (square-of-sums coll)))))

(println (solve))
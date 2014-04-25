(defn look-and-say [input]
  ((fn [input acc]
     (if (empty? input)
       (reverse acc)
       (let [[x & xs] input [i n & as] acc]
         (if (= x i)
           (recur xs (list* x (inc n) as))
           (recur xs (list* x 1 acc))))))
   (rest input) [(first input) 1]))

(println (look-and-say [1]))
(println (look-and-say [1 1]))
(println (look-and-say [2 1]))
(println (look-and-say [1 2 1 1]))
(println (look-and-say [1 1 1 2 2 1]))

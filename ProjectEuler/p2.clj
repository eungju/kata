(defn fib []
  (let [n (atom (list 1 -1))]
    (fn [] (first (reset! n (take 2 (cons (apply + @n) @n)))))))

(defn fib-seq-while [p]
  ((fn [i acc]
     (let [y (i)]
       (if (p y)
         (recur i (cons y acc))
         (reverse acc)))) (fib) '()))

(println
 (apply + (filter even? (fib-seq-while #(<= %1 4000000)))))

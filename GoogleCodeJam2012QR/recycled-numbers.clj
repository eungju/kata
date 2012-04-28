#!/usr/bin/env clj

(defn rotate [s n]
  (str (subs s n) (subs s 0 n)))

(assert (= (rotate "123" 1) "231"))

(defn recycled-numbers [n]
  (let [s (str n)]
    (set (map #(Integer/parseInt (rotate s %1)) (range 1 (.length s))))))

(assert (= (recycled-numbers 123) #{231 312}))
(assert (= (recycled-numbers 120) #{201 12}))
(assert (= (recycled-numbers 1111) #{1111}))
(assert (= (recycled-numbers 1212) #{2121 1212}))

(defn solve [a b]
  (reduce + (map (fn [n] (count (filter #(and (< n %1) (<= %1 b)) (recycled-numbers n))))
                 (range a (inc b)))))

(assert (= (solve 1 9) 0))
(assert (= (solve 10 40) 3))
(assert (= (solve 100 500) 156))
(assert (= (solve 1111 2222) 287))

(dotimes [i (Integer/parseInt (read-line))]
  (let [tokens (clojure.string/split (read-line) #"\s+")
        a (Integer/parseInt (first tokens))
        b (Integer/parseInt (second tokens))]
    (println (format "Case #%d: %d" (+ i 1) (solve a b)))))

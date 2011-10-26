(defn palindrome? [s]
  (= (seq s) (reverse s)))

(println (apply max (filter #(palindrome? (str %1)) (flatten (map (fn [a] (map #(* a %1) (range 100 1000))) (range 100 1000))))))
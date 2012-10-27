(ns round2
  (:use clojure.test)
  (:use clojure.java.io)
  (:require clojure.string)
  (:require clojure.set))

(defn char>int [c] (Integer/parseInt (.toString c)))

(defn string>int [s] (Integer/parseInt s))

(defn inside? [w h [x y]]
  (and (>= x 0) (>= y 0) (< x w) (< y h)))

(defn read-game-spec [r]
  (let [[g n m nc cs cn b s] (re-matches #"^(\d+) (\d+) (\d+) (\d+) (\d+) B(\d+)(?:/S(\d+))?$" (.readLine r))]
    [(let [h (string>int n)
           w (string>int m)]
       (fn [[x y]]
         (inside? w h [x y])))
     (set (repeatedly (string>int nc) #(map string>int (clojure.string/split (.readLine r) #"\s+"))))
     (let [set-cost (string>int cs)
           next-cost (string>int cn)]
       (fn [s n] (+ (* set-cost s) (* next-cost n))))
     (let [cases (set (map char>int b))]
       (fn [live-neighbors]
         (contains? cases live-neighbors)))
     (let [cases (set (if (nil? s) '() (map char>int s)))]
       (fn [live-neighbors]
         (contains? cases live-neighbors)))]))

(let [[boundary seeds cost birth? survival?] (read-game-spec (reader (java.io.StringReader. "5 5 4 1 2 B3/S23\n2 3\n3 2\n3 4\n4 2")))]
  (is (= false (boundary [5 5])))
  (is (= #{[2 3] [3 2] [3 4] [4 2]} seeds))
  (is (= 16 (cost 2 7)))
  (is (= true (birth? 3)))
  (is (= false (birth? 2)))
  (is (= true (survival? 2)))
  (is (= true (survival? 3)))
  (is (= false (survival? 4))))

(defn neighbors [boundary [x y]]
  (set (for [dx [-1 0 1]
             dy [-1 0 1]
             :let [neighbor (vector (+ x dx) (+ y dy))]
             :when (and (not= [x y] neighbor) (boundary neighbor))]
         neighbor)))

(let [boundary #(inside? 5 5 %1)]
  (is (= #{[0 0] [1 0] [2 0] [0 1] [2 1] [0 2] [1 2] [2 2]} (neighbors boundary [1 1])))
  (is (= #{[1 0] [1 1] [0 1]} (neighbors boundary [0 0])))
  (is (= #{[3 4] [3 3] [4 3]} (neighbors boundary [4 4]))))

(defn birth-subjects [boundary survival-subjects]
  (clojure.set/difference
   ((fn [todo acc]
      (if (empty? todo) acc
          (recur (rest todo) (clojure.set/union acc (neighbors boundary (first todo))))))
    survival-subjects #{})
   survival-subjects))

(let [boundary #(inside? 5 5 %1)]
  (is (= #{[1 0] [1 1] [0 1] [3 4] [3 3] [4 3]} (birth-subjects boundary [[0 0] [4 4]]))))

(defn collapse [[boundary seeds cost birth? survival?]]
  ((fn [lifes acc]
     (if (empty? lifes) acc
         (recur (rest lifes) (conj acc [:set (first lifes)])))) seeds []))

(defn actions-cost [actions]
  ())

(defn print-actions [actions]
  (println (count actions))
  (doseq [action actions]
    (if (= :set (first action))
      (println "SET" (nth (second action) 0) (nth (second action) 1))
      (println "NEXT"))))

(with-open [r (reader (first *command-line-args*))]
  (let [t (string>int (.readLine r))]
    (dotimes [i t]
      (let [game (read-game-spec r)]
        (print-actions (collapse game))
        ;(println i)
        ;(let [[boundary seeds cost birth? survival?] game] (birth-subjects boundary seeds))
        ))))

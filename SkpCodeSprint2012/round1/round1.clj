#!/usr/bin/env clj

(use 'clojure.java.io)

(defn parse-edge [line]
  (map #(Integer/parseInt %1) (.split line ",")))

(deftype Direction [^int to ^int distance])

(def graph (object-array 1000000))

(defn load-graph [edges]
  (doseq [p (partition-by first edges)]
  	(aset graph (first (first p)) (map #(Direction. (second %1) (nth %1 2)) p))
  	(println (first (first p)))))

(with-open [rdr (reader (first *command-line-args*))]
	(load-graph (map parse-edge (line-seq rdr)))
	(doseq [node graph]
		(println (count node))))

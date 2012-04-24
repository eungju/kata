#!/usr/bin/env clj

(use '[clojure.set])

(def alphabet (set (map char (range 97 (+ 97 26)))))

(defn train [knowledge example]
  (merge knowledge (zipmap (first example) (second example))))

(defn infer [knowledge]
  (merge knowledge (zipmap (difference alphabet (set (keys knowledge)))
          (difference alphabet (set (vals knowledge))))))

(defn translate [knowledge googlerese]
  (apply str (map #(knowledge %1) googlerese)))

(def knowledge (infer (reduce train {} [["yeq" "aoz"]
                           ["ejp mysljylc kd kxveddknmc re jsicpdrysi" "our language is impossible to understand"]
                           ["rbcpc ypc rtcsra dkh wyfrepkym veddknkmkrkcd" "there are twenty six factorial possibilities"]
                           ["de kr kd eoya kw aej tysr re ujdr lkgc jv" "so it is okay if you want to just give up"]])))

(dotimes [i (Integer/parseInt (read-line))]
  (println (format "Case #%d: %s" (+ i 1) (translate knowledge (read-line)))))
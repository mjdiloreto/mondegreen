(ns mondegreen.word-counts
  (require [clojure.string :as string]))

(def raw-word-counts (slurp "resources/count_1w.txt"))

(def word-counts
  (->> raw-word-counts
       (string/split-lines)
       (mapcat #(string/split % #"\t"))
       (take-nth 2)
       (map string/upper-case)))

(defn most-common-words
  [n]
  (set (take n word-counts)))


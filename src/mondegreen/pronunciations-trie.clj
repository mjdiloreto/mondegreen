(ns mondegreen.pronunciations-trie)

;; To answer questions about the sequences of phonemes, we construct a trie and map over it with the appropriate substitution function.
(defn word->nested-map
  [phones word]
  (if (empty? phones)
    {:word word}
    {:word nil
     (first phones) (word->nested-map (rest phones) word)}))

;; TODO move to util. https://clojuredocs.org/clojure.core/merge-with#example-5b80843ae4b00ac801ed9e74
(defn deep-merge-with
  "Like merge-with, but merges maps recursively, applying the given fn
  only when there's a non-map at a particular level.
  (deep-merge-with + {:a {:b {:c 1 :d {:x 1 :y 2}} :e 3} :f 4}
                     {:a {:b {:c 2 :d {:z 9} :z 3} :e 100}})
  -> {:a {:b {:z 3, :c 3, :d {:z 9, :x 1, :y 2}}, :e 103}, :f 4}"
  [f & maps]
  (apply
   (fn m [& maps]
     (if (every? map? maps)
       (apply merge-with m maps)
       (apply f maps)))
   maps))

(defn merge-non-nil
  [a b]
  (if (nil? a) b
      (if (nil? b) a
          (into a b))))

(def pronunciations-trie
  (apply
   deep-merge-with
   merge-non-nil
   (map (fn [[k v]] (word->nested-map k v)) pronunciation->word)))

(defn pronun-valid?
  [pronunciation]
  (loop [remaining pronunciation
         trie pronunciations-trie]
    (cond (empty? remaining) trie
          (nil? trie) trie
          :else (recur (rest remaining) (trie (first remaining))))))

(comment
  (pronunciations-trie "DH")
  (pronun-valid? '("DH" "IY"))
  (pronun-valid? '("DH" "IY")))


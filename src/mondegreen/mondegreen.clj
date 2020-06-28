(ns mondegreen.mondegreen
  (:require [clojure.string :as string]
            [clojure.set :refer [union]]
            [clojure.math.combinatorics :as combo]))

(def word-counts
  (->> (slurp "resources/count_1w.txt")
       (string/split-lines)
       (mapcat #(string/split % #"\t"))
       (take-nth 2)
       (map string/upper-case)))

(defn most-common-words
  [n]
  (set (take n word-counts)))

(def maps-parts
  (let [dictionary-chars #"^[A-Z'\-\.\_]+" ; All of these characters valid characters an English word can contain.
        symbol->phone (fn [sym] (re-find dictionary-chars sym))
        syms (->>
              (slurp "resources/cmudict-0.7b.phones")
              (string/split-lines)
              (mapcat #(string/split % #"\t"))
              (take-nth 2)
              (set))
        ]
    (->> (slurp "resources/cmudict-0.7b")
         (string/split-lines)
         (filter (complement #(.startsWith % ";")))
         (map #(string/split % #"\s"))
         (filter (complement empty?))
         (map #(map symbol->phone %))
         (filter (fn [[spelling _tab & pronunciation]]
                    (every? (partial contains? syms) pronunciation)))
         (map (fn [[spelling _tab & pronunciation]]
                [{spelling [pronunciation]} {pronunciation [spelling]}]))
         ))) ; create both maps at once, spelling->pronunciation, pronunciation->spelling

(def word->pronunciations
  (reduce (partial merge-with into) (map first maps-parts)))

(def pronunciation->word
  (reduce (partial merge-with into) (map second maps-parts)))

(comment
  "Takes into account conjunctions and hyphenations"
  (word->pronunciations "THIS")        ;  [("DH" "IH" "S") ("DH" "IH" "S")]
  (word->pronunciations "THIS'LL")     ;  [("DH" "IH" "S" "AH" "L") ("DH" "IH" "S" "AH" "L")]

  "These are common substitutions for mondegreens, A few key substitutions."
  (word->pronunciations "THEME")       ;  [("TH" "IY" "M")]
  (word->pronunciations "AWNING")      ;  [("AA" "N" "IH" "NG")]
  (word->pronunciations "THE")         ;  [("DH" "AH") ("DH" "AH") ("DH" "IY")]
  (word->pronunciations "MORNING")     ;  [("M" "AO" "R" "N" "IH" "NG")]

  (word->pronunciations "THIS")        ;  [("DH" "IH" "S") ("DH" "IH" "S")]
  (word->pronunciations "GUY")         ;  [("G" "AY")]
  (word->pronunciations "THE")         ;  [("DH" "AH") ("DH" "AH") ("DH" "IY")] ; when multiple pronunciations are equal, the only difference was syllable stress.
  (word->pronunciations "SKY")         ;  [("S" "K" "AY")]

  (word->pronunciations "ICE")         ;  [("AY" "S")]
  (word->pronunciations "CREAM")       ;  [("K" "R" "IY" "M")]
  (word->pronunciations "ICE-CREAM")   ;  [("AY" "S" "K" "R" "IY" "M")]
  (word->pronunciations "I")           ;  [("AY")]
  (word->pronunciations "SCREAM")      ;  [("S" "K" "R" "IY" "M")]
  )

(def pronunciations-trie
  (letfn [(word->nested-map [phones word]
            "To answer questions about the sequences of phonemes, we construct a trie and map over it with the appropriate substitution function."
            (if (empty? phones)
              {:word word}
              {:word nil
               (first phones) (word->nested-map (rest phones) word)}))

          (deep-merge-with ; TODO move to util. https://clojuredocs.org/clojure.core/merge-with#example-5b80843ae4b00ac801ed9e74
            [f & maps]
            (apply
             (fn m [& maps]
               (if (every? map? maps)
                 (apply merge-with m maps)
                 (apply f maps)))
             maps))

          (merge-non-nil
            [a b]
            (if (nil? a) b
                (if (nil? b) a
                    (into a b))))]
    (apply
     deep-merge-with
     merge-non-nil
     (map (fn [[k v]] (word->nested-map k v)) pronunciation->word))))

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

(defn parse-sentence
  [sentence]
  (map word->pronunciations
       (->
        sentence
        (string/upper-case)
        (string/split #"\s"))))

(comment
  "This example /should/ be within the reach of the equivalence class-based solution. Only substitutions are Z->S and AA->AO."
  (parse-sentence "please not while I'm eating")
  ([("P" "L" "IY" "Z")]
   [("N" "AA" "T")]
   [("W" "AY" "L") ("HH" "W" "AY" "L")]
   [("AY" "M") ("AH" "M")]
   [("IY" "T" "IH" "NG")])

  (parse-sentence "plea snot why lie meeting")
  ([("P" "L" "IY")]
   [("S" "N" "AO" "T")]
   [("W" "AY") ("HH" "W" "AY")]
   [("L" "AY")]
   [("M" "IY" "T" "IH" "NG")]))

(defn sentence->pronunciations
  "Given a sentence represented by lists of words, represented each by lists of pronunciations, return all ways a sentence could be pronounced."
  [sentence]
  (map flatten (reduce combo/cartesian-product sentence)))

(comment
  (sentence-pronunciations (parse-sentence "I AM"))
  (("AY" "AE" "M")
   ("AY" "EY" "EH" "M"))

  (sentence-pronunciations (parse-sentence "NOT WHILE I'M EATING"))
  (("N" "AA" "T" "W" "AY" "L" "AY" "M" "IY" "T" "IH" "NG")
   ("N" "AA" "T" "W" "AY" "L" "AH" "M" "IY" "T" "IH" "NG")
   ("N" "AA" "T" "HH" "W" "AY" "L" "AY" "M" "IY" "T" "IH" "NG")
   ("N" "AA" "T" "HH" "W" "AY" "L" "AH" "M" "IY" "T" "IH" "NG")))

(def equivalence-classes
  "It would be better to base these substitutions on some kind of linguistic data. I bet it exists."
  [#{"AA" "AO" "AW" "AH" "UH"}
   #{"AY" "AE"}
   #{"B"}
   #{"CH"}
   #{"D"}
   #{"DH" "TH"}
   #{"EH" "IH"}
   #{"EY"} ; Maybe can be paired up?
   #{"ER" "R"}
   #{"F"}
   #{"G" "K"}
   #{"HH"} ; This would be a good candidate for deletion. Maybe also goes with "EH"?
   #{"IY"} ; Maybe goes with "IH", but probably not "EH"... Maybe these shouldn't be sets?
   #{"JH" "ZH"}
   #{"L"}
   #{"M"}
   #{"N"}
   #{"NG"} ; Can go with N+G when splitting one phoneme into multiple is allowed.
   #{"OW"} ; All diphthongs can probably go with their constituent monophthongs. e.g. AA+UW
   #{"OY"}
   #{"P"}
   #{"SH"} ; Maybe belongs with "S"
   #{"T"} ; Can this go with "D"? Is that too jarring.
   #{"V"}
   #{"W"}
   #{"Y"}
   #{"S" "Z"}])

(defn valid-replacements
  "Which phonemenes can be used to represent the given one?"
  [phone]
  (apply vector (apply union (filter #(% phone) equivalence-classes))))

(defn pronunciation->sentence
  "Can this series of phonemes be parsed as a collection of English words? Find one answer containing no words from the set of words in the original sentence."
  [pronunciation original-sentence]
  (letfn [(word? [phones]
            (and (not (original-sentence phones))
                 (:word (pronun-valid? phones))))
          (search-substitutions
            [prev-phones next-phones]
            (reduce (fn [acc x] (or acc (find-pronuns (conj prev-phones x) (rest next-phones))))
                    nil
                    (valid-replacements (first next-phones))))
          (find-pronuns [prev-phones next-phones]
            (let [answer (word? prev-phones)]
              (cond
                (empty? next-phones) (when answer (list answer))
                answer (if-let [rest-of-sentence (find-pronuns [] next-phones)]
                         (cons answer rest-of-sentence)
                         (search-substitutions prev-phones next-phones))
                :else (search-substitutions prev-phones next-phones))))]
    (find-pronuns [] pronunciation)))

;; From scratch a second time to get more ideas.
(defn search-shortest-words-first
  [pronunciation original-sentence]
  (letfn [(word? [phones]
            (and (not (original-sentence phones))
                 (:word (pronun-valid? phones))))]
    (loop [current-phones (vector (first pronunciation)) remaining-phones (rest pronunciation)]
      (let [word (word? current-phones)]
        (cond (and word (empty? remaining-phones)) (list word) ; return the last word, or nil if none found
              (empty? remaining-phones) nil ; there is no solution here
              word (if-let [rest-of-sentence (search-shortest-words-first remaining-phones original-sentence)]
                     (cons word rest-of-sentence)
                     (recur (conj current-phones (first remaining-phones))
                            (rest remaining-phones)))
              :else (recur (conj current-phones (first remaining-phones))
                           (rest remaining-phones)))))))

(defn mondegreen
  [sentence]
  (let [parsed (parse-sentence sentence)
        original-sentence (set (mapcat identity parsed))
        pronunciations (sentence->pronunciations parsed)]
    (pronunciation->sentence (first pronunciations) original-sentence)))

(comment
  (mondegreen "please not while I'm eating")
  ; example of dirty substitutions I've seen in videos that people will want to work.
  (mondegreen "alpha kenny body")
  (mondegreen "I'll fuck any body")
  (mondegreen "I scream")
  (mondegreen "The sky")
  (mondegreen "Baby duckling"))

(defn mondegreen2
  [sentence]
  (let [parsed (parse-sentence sentence)
        original-sentence (set (mapcat identity parsed))
        pronunciations (sentence->pronunciations parsed)
        selected-pronunciation (first pronunciations)
        partitions (combo/partitions selected-pronunciation)]
    (search-shortest-words-first selected-pronunciation original-sentence)))
(comment
  (mondegreen2 "please not while I'm eating")
  (mondegreen2 "I scream")
  (mondegreen2 "The sky")
  (mondegreen2 "Baby duckling"))

;; Ask someone if (list (list (list (first coll)))) is actually an issue. (I realize `(((~(first coll))))) works, but is it better?)
;; I don't imagine functions dealing with more deeply nested structures than this without an
;; intervening layer of abstraction, but maybe I'm wrong and doing things this way dooms
;; me to eternal suffering in hell '\_(o-o)_/'


(defn reverse-order-partitions
  "Partition the seq in all ways that maintain order."
  [coll]
  (if (empty? coll)
    '()
    (loop [c (rest coll) acc (list (list (list (first coll))))]
      (if (empty? c)
        acc
        (recur (rest c)
               (mapcat (fn [part] (list (list* (cons (first c) (first part)) (rest part))  ; TODO modify this to test for validity in my app.
                                        (list* (list (first c)) part)))
                       acc))))))

(comment
  (reverse-order-partitions [1])
  (combo/partitions [1])
  (reverse-order-partitions [1 2])
  (combo/partitions [1 2])
  (reverse-order-partitions [1 2 3])
  (combo/partitions [1 2 3])
  (reverse-order-partitions [1 2 3 4])
  (combo/partitions [1 2 3 4]))

(defn reverse-order-partitions2
  [coll]
  (reduce (map (fn [acc x] (map #(cons x %))))
          '()))




(ns mondegreen.mondegreen
  (:require [clojure.math.combinatorics :as combo]
            [clojure.set :refer [union]]
            [clojure.string :as string]))

(comment
  "
An alternative name for this project could be \"oronyms\" https://en.wikipedia.org/wiki/Juncture#Oronyms

1.[x] Parse the dictionary to provide word -> phonetic mapping
2. Create a scoring function, that given 2 phonemes gives a score reflecting how close they sound.
2.1 alternatively just create a map of phoneme -> [phoneme...] that tells whether a certain phoneme can replace another.

3? Create a reverse mapping
 list(phoneme), map(phoneme -> list(phoneme)) -> word
such that words match a list of phonemes given the list of acceptable replacements of phonemes.
This would require a direct reverse mapping of [1].
")

(defn parse-sentence
  "Given the spelling of an English sentence, list all valid pronunciations of every word in the sentence."
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
  (mondegreen "Baby duckling")
  )

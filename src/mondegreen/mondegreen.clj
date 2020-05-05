(ns mondegreen.mondegreen
  (:require [clojure.string :as string]
            [clojure.math.combinatorics :as combo]))

(comment
  "
An alternative name for this project could be \"oronyms\" https://en.wikipedia.org/wiki/Juncture#Oronyms

1.[x] Parse the dictionary to provide word -> phonetic mapping
2. Create a scoring function, that given 2 phonemes gives a score reflecting how close they sound.
2.1 alternatively just create a map of phoneme -> [phoneme...] that tells whether a certain phoneme can replace another.

3? Craete a reverse mapping
 list(phoneme), map(phoneme -> list(phoneme)) -> word
such that words match a list of phonemes given the list of acceptable replacements of phonemes.
This would require a direct reverse mapping of [1].

Algorithm:
Input <- A sentence, the answer to the mondegreen.
input -> flat phoneme list, no distinctions between words (represents prnunciation of all phonemes with no cadence)


Fill in the list with words that match the list
")

;; Dictionary files
(def raw-dict (slurp "resources/cmudict-0.7b"))
(def raw-phones (slurp "resources/cmudict-0.7b.phones"))
(def raw-symbols (slurp "resources/cmudict-0.7b.symbols"))

(comment
  "Make an equivalent threaded program for this"
  (set (take-nth 2 ((partial mapcat #(string/split % #"\t")) (string/split-lines raw-phones)))))

(def syms
  (->>
   raw-phones
   (string/split-lines)
   (mapcat #(string/split % #"\t"))
   (take-nth 2)
   (set)))

;; All of these characters are found in the dictionary.
(def dictionary-chars #"^[A-Z'\-\.\_]+")

(defn symbol->phone
  "Converts a symbol found in the dictionary to just it's phonetic part
  (i.e. no stress)"
  [sym]
  (re-find dictionary-chars sym))

(defn word-ops
  ; create both maps at once, spelling->pronunciation, pronunciation->spelling
  [[spelling pronunciation]]
  (let [spell (symbol->phone (first spelling))
        pronun (map symbol->phone pronunciation)]
    (if (every? (partial contains? syms) pronun)
      [{spell [pronun]} {pronun [spell]}])))  ; can be many pronunciations of a word, and multiple words for a pronunciation.

(defn line-ops
  [line]
  (->> line
       (filter (complement #(.startsWith % ";")))  ; remove comments
       (map #(filter not-empty (string/split % #"\s")))
       (map (partial split-at 1))                  ; break into word/phones
       (map word-ops)))

(def maps-parts
  (->>
   raw-dict
   (string/split-lines)
   line-ops))

(def word->pronunciations
  (reduce (partial merge-with into) (map first maps-parts)))

(def pronunciation->word
  (reduce (partial merge-with into) (map second maps-parts)))

(comment
  "Takes into account conjunctions and hyphenations"
  (word->pronunciations "THIS")
  (word->pronunciations "THIS'LL")
  (word->pronunciations "MORNING")
  (word->pronunciations "MORNING'S")
  (word->pronunciations "ICE-CREAM"))

(comment
  "These are common substitutions for mondegreens, A few key substitutions."
  (word->pronunciations "THEME")
  (word->pronunciations "AWNING")
  (word->pronunciations "THE")
  (word->pronunciations "MORNING")

  (word->pronunciations "THIS")
  (word->pronunciations "GUY")
  (word->pronunciations "THE")
  (word->pronunciations "SKY")

  (word->pronunciations "ICE")
  (word->pronunciations "CREAM")
  (word->pronunciations "ICE-CREAM")
  (word->pronunciations "I")
  (word->pronunciations "SCREAM"))

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

(comment
  (deep-merge-with (fn [a b] (if (nil? a) b a))
                   (word->nested-map '("A" "B" "C" "D") "ABCD")
                   (word->nested-map '("A" "B") "AB")
                   (word->nested-map '("A" "B" "C") "ABC")))

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
  (pronunciation->word '("DH" "IY"))
  (pronun-valid? '("DH" "IY"))
  (pronun-valid? '("DH" "IY")))

(defn parse-sentence
  [sentence]
  (map word->pronunciations
       (->
        sentence
        (clojure.string/upper-case)
        (clojure.string/split #"\s"))))

(comment
  ([("P" "L" "IY" "Z")] [("N" "AA" "T")] [("W" "AY" "L") ("HH" "W" "AY" "L")] [("AY" "M") ("AH" "M")] [("IY" "T" "IH" "NG")])
  (parse-sentence "please not while I'm eating")
  ([("P" "L" "IY")] [("S" "N" "AO" "T")] [("W" "AY") ("HH" "W" "AY")] [("L" "AY")] [("M" "IY" "T" "IH" "NG")])
  (parse-sentence "plea snot why lie meeting"))

(def equivalence-classes
  "It would be better to base these substitutions on some kind of linguistic data. I bet it exists."
  [
   #{"AA" "AO" "AW" "AH" "UH"}
   #{"AY"}
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
   #{"S" "Z"}
   ])

(def valid-replacements
  "Which phonemes can replace others and still sound reasonable? TODO add support for multiple phonemes or deletion.
  This might be more tersely described by sets, since (valid-replacement p1 p2) implies (valid-replacement p2 p1)."
  {
   "AA" ["AA"]
   "AE" ["AE"]
   "AH" ["AH"]
   "AO" ["AO"]
   "AW" ["AW"]
   "AY" ["AY"]
   "B" ["B"]
   "CH" ["CH"]
   "D" ["D"]
   "DH" ["DH"]
   "EH" ["EH"]
   "ER" ["ER"]
   "EY" ["EY"]
   "F" ["F"]
   "G" ["G"]
   "HH" ["HH"]
   "IH" ["IH"]
   "IY" ["IY"]
   "JH" ["JH"]
   "K" ["K"]
   "L" ["L"]
   "M" ["M"]
   "N" ["N"]
   "NG" ["NG"]
   "OW" ["OW"]
   "OY" ["OY"]
   "P" ["P"]
   "R" ["R"]
   "S" ["S"]
   "SH" ["SH"]
   "T" ["T"]
   "TH" ["TH"]
   "UH" ["UH"]
   "UW" ["UW"]
   "V" ["V"]
   "W" ["W"]
   "Y" ["Y"]
   "Z" ["Z"]
   "ZH" ["ZH"]})

(comment "
  IDEA 1: Convert the English Phonetic Mouth Chart to a function describing the 'distance' between phonemes.
  https://github.com/open-source-ideas/open-source-ideas/issues/58 <- This image.

  The vowel chart can be described as a graph.

  pros: Can generate new mondegreens from first principles.
  cons: Might be difficult to tune the mapping function(s)
        Might not be able to account for single phonemes that substitute for multiple (e.g. AO+R -> AA)
          This could potentially be fixed by researching phonetic substitutions common in American/English dialects.
          Obviously this wouldn't then be from 'first principles', but it would match the real-world linguistic phonemena.
")

(comment "
  IDEA 2:
  Find many examples of mondegreen/oronyms, and discover which phonemes are swapped for others.

  pros: Will easily be able to tell which substitutions of phonemes are valid (even when a single phoneme maps to multiple!)
        If a dataset exists, might be easier to implement.
  cons: Will only be able to replicate the types of mondegreens that other people have already found. Might not produce great variation.
")

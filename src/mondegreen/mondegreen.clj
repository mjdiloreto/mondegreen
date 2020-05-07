(ns mondegreen.mondegreen
  (:require [clojure.string :as string]
            [clojure.math.combinatorics :as combo]))

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

;; Dictionary files
(def raw-dict (slurp "resources/cmudict-0.7b"))
(def raw-phones (slurp "resources/cmudict-0.7b.phones"))
(def raw-symbols (slurp "resources/cmudict-0.7b.symbols"))

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
  (word->pronunciations "THIS")        ;  [("DH" "IH" "S") ("DH" "IH" "S")]
  (word->pronunciations "THIS'LL")     ;  [("DH" "IH" "S" "AH" "L") ("DH" "IH" "S" "AH" "L")]
  )

(comment
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

(defn parse-sentence
  [sentence]
  (map word->pronunciations
       (->
        sentence
        (clojure.string/upper-case)
        (clojure.string/split #"\s"))))

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

(defn pronunciation->sentence
  "Can this series of phonemes be parsed as a collection of English words? Find one answer containing no words from the set of words in the original sentence."
  [pronunciation original-sentence]
  (letfn [(word? [phones]
            (and (not (original-sentence phones))
                 (:word (pronun-valid? phones))))
          (something [prev-phones next-phones]
            (let [answer (word? prev-phones)]
              (cond
                (empty? next-phones) answer  ; The final phonemes must consitute a word.
                answer (cons answer (something (vector (first next-phones)) (rest next-phones)))
                :else (something (conj prev-phones (first next-phones))
                                 (rest next-phones)))))]
    (something (vector (first pronunciation)) (rest pronunciation))))

(defn mondegreen
  [sentence]
  (let [parsed (parse-sentence sentence)
        original-sentence (set (mapcat identity parsed))
        pronunciations (sentence->pronunciations parsed)]
    (pronunciation->sentence (first pronunciations) original-sentence)))

(comment
  (mondegreen "please not while I'm eating")
  )

(def equivalence-classes
  "It would be better to base these substitutions on some kind of linguistic data. I bet it exists."
  [#{"AA" "AO" "AW" "AH" "UH"}
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
   #{"S" "Z"}])

(def valid-replacements
  "Which phonemes can replace others and still sound reasonable? TODO add support for multiple phonemes or deletion."
  {"AA" ["AA"]
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

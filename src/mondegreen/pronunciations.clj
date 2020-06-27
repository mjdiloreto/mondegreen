(ns mondegreen.pronunciations
  (require [clojure.string :as string]))

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


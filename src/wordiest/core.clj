(ns wordiest.core
  (:gen-class)
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str]))

(defn letter-value [c]
  "Wordiest letter values."
  (let [letters {\d 2 \n 2 \u 2 \l 2
                 \g 3 \c 3 \p 3 \h 3
                 \m 4 \b 4 \f 4 \w 4 \y 4
                 \k 5
                 \v 6
                 \x 8
                 \j 10 \q 10 \z 10}]
    (get letters c 1)))

(defn to-tile [s]
  "Converts user input into tiles."
  (let [s (.toLowerCase s)
        letter (get s 0)
        tile {:letter letter
              :letter-val (letter-value letter)}]
    (case (count s)
      1 tile
      3 (let [mul (Character/getNumericValue (get s 1))]
          (case (get s 2)
            \l (assoc tile :letter-mul mul)
            \w (assoc tile :word-mul mul)
            nil))
      nil)))

(defn tiles-can-spell [tiles word]
  "Return the tiles to spell the given word, otherwise nil."
  {:pre [(>= (count tiles) (count word))]}
  (loop [letters (frequencies word)
         used []]
    (if (seq letters)
      (let [e (first letters)
            letter-tiles (take (val e)
                               (filter #(= (key e) (get % :letter)) tiles))]
        (when (= (count letter-tiles) (val e))
          (recur (rest letters) (concat used letter-tiles))))
      used)))

(defn score [tiles]
  "Calculates the score for given tiles."
  (let [word-mul (reduce * (map #(get % :word-mul 1) tiles))
        letters (reduce + (map #(* (:letter-val %)
                                   (get % :letter-mul 1)) tiles))]
    (* letters word-mul)))

(defn sort-tiles [tiles]
  (sort-by
    (juxt :word-mul :letter-mul) #(compare %2 %1)
    tiles))

(defn diff [s1 s2]
  (sort-tiles
    (mapcat
      (fn [[x n]] (repeat n x))
      (apply merge-with - (map frequencies [s1 s2])))))

(defn get-all-words [dictionary tiles]
  (sort-by :score >
           (for [word dictionary
                 :let [match (tiles-can-spell tiles word)]
                 :when (seq match)]
             {:word word :tiles match :score (score match)})))

(defn solution [pair]
  (reduce + (map :score pair)))

(defn match-str [m]
  (format "%s (%s)" (:word m) (:score m)))

(defn print-solution [pair]
  (let [[a b] pair]
    (printf "%s: %s + %s\n" (solution pair) (match-str a) (match-str b))
    (flush)))

(defn get-dictionary []
  "Reads word list into a vector."
  (with-open [fp (io/reader (io/resource "TWL06.txt"))]
    (reduce conj []
            (for [line (line-seq fp)
                  :let [line (.toLowerCase line)]
                  :when (not (str/starts-with? "#" line))]
              line))))

(defn parse-letters [letters]
  "Converts letters to tiles, sorts by tile strength, and validates input."
  (let [tiles (sort-tiles (map to-tile letters))]
    (when (= (count (filter some? tiles)) 14))
      tiles))

(defn solve [tiles]
  (let [matches (get-all-words (get-dictionary) tiles)]
    (apply max-key solution
           (for [match (take 50 matches)
                 :let [remaining (diff tiles (:tiles match))
                       second-word (first (get-all-words
                                            (map :word matches)
                                            remaining))]
                 :when second-word]
             [match second-word]))))

(defn -main [& args]
  (def tiles (parse-letters args))
  ; TODO: Turn these testcases into tests.
  ; (def tiles (parse-letters (str/split "c a i o o g w5w e u r2l r i2l u2w l" #"\s+")))
  ; (def tiles (parse-letters (str/split "e4w v e t5l n q i d2l f u i w s3w t" #"\s+")))
  ; (def tiles (parse-letters (str/split "n e2l z t s a s n l t3l e e e f" #"\s+")))

  (if (nil? tiles)
    (println "Invalid tiles")
    (print-solution (solve tiles))))

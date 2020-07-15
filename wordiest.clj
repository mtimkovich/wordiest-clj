#!/usr/bin/env clj

(defn letter-value [c]
  (let [letters {\d 2 \n 2 \u 2 \l 2
                 \g 3 \c 3\p 3 \h 3
                 \m 4 \b 4 \f 4 \w 4 \y 4
                 \k 5
                 \v 6
                 \x 8
                 \j 10 \q 10 \z 10}]
    (get letters c 1)))

(defn to-tile [s]
  "Converts user input into tiles."
  (let [letter (get s 0)
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
  (loop [letters (frequencies word)
         used []]
    (if (seq letters)
      (let [e (first letters)
            letter-tiles (take (val e)
                               (filter #(= (key e) (get % :letter)) tiles))]
        (if (not= (count letter-tiles) (val e))
          nil
          (recur (rest letters) (concat used letter-tiles))))
      used)))


(defn score [tiles]
  (let [word-mul (reduce * (map #(get % :word-mul 1) tiles))
        letters (reduce + (map #(* (:letter-val %)
                                   (get % :letter-mul 1))
                               tiles))]
    (* letters word-mul)))

(defn diff [s1 s2]
  "https://stackoverflow.com/questions/23199295/how-to-diff-substract-two-lists-in-clojure/23200627#23200627"
  (mapcat
    (fn [[x n]] (repeat n x))
    (apply merge-with - (map frequencies [s1 s2]))))

(defn get-all-words [dictionary tiles]
  (sort-by #(:score %) >
           (for [word dictionary
                 :let [match (tiles-can-spell tiles word)]
                 :when (seq match)]
             {:word word :tiles match :score (score match)})))

(defn solution [a b]
  (+ (:score a) (:score b)))

(defn print-solution [a b]
  (printf "%s: %s (%s) + %s (%s)\n" (solution a b)
          (:word a) (:score a)
          (:word b) (:score b)))

(defn get-dictionary [file]
  (with-open [fp (clojure.java.io/reader file)]
    (reduce conj [] (map #(.toLowerCase %) (line-seq fp)))))

(def dictionary (get-dictionary "TWL06.txt"))

(def letters *command-line-args*)
(def tiles (sort-by
             (juxt :word-mul :letter-mul) #(compare %2 %1)
             (map to-tile letters)))

(when (or (not= (count tiles) 14) (some nil? tiles))
  (println "Invalid tiles"))

(def matches (get-all-words dictionary tiles))

(doseq [match (take 1 matches)
      :let [remaining (diff tiles (:tiles match))
            second-word (first (get-all-words dictionary remaining))]
      :when second-word]
  (print-solution match second-word))

; (def first-word (first matches))
; (def second-word (first (get-all-words dictionary
;                                        (diff tiles (:tiles first-word)))))

; (print-solution first-word second-word)

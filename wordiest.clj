(def letters '("f" "o" "o2w" "o" "p" "w" "c4l" "n2w" "a" "x2l" "r3l"
               "a" "b" "i"))

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

(defn dictionary [file]
  (with-open [fp (clojure.java.io/reader file)]
    (reduce conj [] (map #(.toLowerCase %) (line-seq fp)))))

(def tiles (sort-by
             (juxt :word-mul :letter-mul) #(compare %2 %1)
             (map to-tile letters)))

(def matches (for [word (dictionary "TWL06.txt")
                   :let [match (tiles-can-spell tiles word)]
                   :when (seq match)]
               {:word word :tiles match :score (score match)}))

(def sorted-matches (sort-by #(:score %) > matches))
; (println (:word (first sorted-matches)))
(doseq [match (take 10 sorted-matches)]
  (println (:word match)))

; TODO: Calculate remaining tiles to create the second word.
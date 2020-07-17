(ns wordiest.core-test
  (:require [clojure.string :as str]
            [clojure.test :refer :all]
            [wordiest.core :refer :all]))

(defn solve-str [s]
  (solve
    (parse-letters
      (str/split s #"\s+"))))

(deftest multipler-priority
  (testing "Proper tile multiplier priority"
    (let [ret (solve-str "c a i o o g w5w e u r2l r i2l u2w l")]
      (is (>= (solution ret) 100)))))

(deftest second-word-test
  (testing "Second word is not empty"
    (let [ret (solve-str "e4w v e t5l n q i d2l f u i w s3w t")]
      (is (= (count ret) 2)))))

(deftest close-words
  (testing "Best solution when words are close in score"
    (let [ret (solve-str "n e2l z t s a s n l t3l e e e f")]
      (is (>= (solution ret) 32)))))

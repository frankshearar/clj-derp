(ns clj-derp.core-test-grammars
  (:use clojure.test
        clj-derp.core))

(defn nested-list-to-int [l]
  (reduce (fn [acc n] (+ (* acc 10) n)) 0 (flatten l)))

(deftest testing-nested-list-to-int
  (is (= 0 (nested-list-to-int '())))
  (is (= 0 (nested-list-to-int [])))
  (is (= 0 (nested-list-to-int [0 []])))
  (is (= 9 (nested-list-to-int [9 []])))
  (is (= 89 (nested-list-to-int [8 [9 []]])))
  (is (= 789 (nested-list-to-int [7 [8 [9 []]]]))))

(def digit
  "Parse a single (base 10) numeric character."
  (red
   (lit* (map char (range 48 57)))
   (fn [c] (- (int c) (int (char \0))))))
(def integer
  "Parse an integer."
  (red (star digit) nested-list-to-int))

(deftest test-parsers
  (is (= #{0} (parse digit "0")))
  (is (= #{123} (parse integer "123"))))
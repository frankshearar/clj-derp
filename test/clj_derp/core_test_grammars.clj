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

(defn lit-cat [tokens]
  (apply cat (map #(lit %) tokens)))

(def sheep
  (red (lit-cat [\s \h \e \e \p]) flatten))

(deftest test-sheep
  (is (= #{'(\s \h \e \e \p)} (parse sheep "sheep"))))

(def parser-counts
  ^{:doc "A map containing the number of times parsers (the keys) have run (the values)."}
  (atom {}))
(defn parser-count [counted-parser]
  "Return the number of times a parser has run. Assumes that counted-parser came from a call to counted"
  (get @parser-counts counted-parser 0))
;; (defn reset-parser-counts []
;;   (swap! parser-counts (fn [_] {})))

(deftest counting-sheep
  (binding [*pre-parse-callback* (fn [parser]
                                   (pr ".")
                                   (swap! parser-counts #(assoc % parser (inc (get % parser 0)))))]
    (do
      (is (= 0 (parser-count sheep)))
      (parse sheep "goat")
      (is (= 0 (parser-count sheep)))
      (parse sheep "sheep")
      (is (= 1 (parser-count sheep)))
      (prn "!!!")
      (parse sheep "sheep")
      (prn "!!!")
      (is (= 3 (parser-count sheep)))
      (prn (vals @parser-counts)))))
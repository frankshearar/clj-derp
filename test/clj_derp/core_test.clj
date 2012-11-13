(ns clj-derp.core-test
  (:use clojure.test
        clj-derp.core))

(deftest making-parsers
  (testing "Making parsers"
    (is (= (empty-p) (alt)))
    (is (= (lit "it") (alt (lit "it"))))
    (is (= (lit "left") (force (:left (alt (lit "left") (lit "right"))))))))

(deftest deriving
  (testing "Derivative of"
    (testing "empty parser"
      (is (= (empty-p) (d (empty-p) "anything"))))
    (testing "eps"
      (is (= (empty-p) (d (eps) "anything"))))
    (testing "eps*"
      (is (= (empty-p) (d (eps* "a") "anything"))))
    (testing "lit"
      (is (= (empty-p) (d (lit "a") "not a")))
      (is (= (eps* "a") (d (lit "a") "a"))))
    (testing "alt"
      (is (eq (alt (d (lit "a") "a") (d (lit "b") "a"))
             (d (alt (lit "a") (lit "b")) "a"))))))

(deftest comparing
  (testing "Comparing"
    (testing "empty"
      (is (eq (empty-p) (empty-p)))
      (is (not (eq (empty-p) (eps))))
      (is (not (eq (empty-p) (eps* "a"))))
      (is (not (eq (empty-p) (lit "a"))))
      (is (not (eq (empty-p) (alt (lit "a") (lit "b"))))))
    (testing "eps"
      (is (eq (eps) (eps)))
      (is (not (eq (eps) (eps* "a"))))
      (is (not (eq (eps) (lit "a"))))
      (is (not (eq (eps) (alt)))))
    (testing "unions"
      (is (eq (alt) (alt)))
      (is (eq (alt (empty-p)) (alt (empty-p))))
      (is (eq (alt (lit "a") (lit "b")) (alt (lit "a") (lit "b")))))))

(deftest parsing-null
  (testing "Null parses"
    (is (= #{nil} (parse-null (eps))))
    (is (= #{} (parse-null (empty-p))))
    (is (= #{} (parse-null (lit "a"))))
    (is (= #{"a"} (parse-null (eps* "a"))))
    (testing "of Union"
      (is (= #{"a" "b"} (parse-null (alt (eps* "a") (eps* "b"))))))))

(deftest parsing
  (testing "Basic parse tests"
    (is (= #{} (parse (empty-p) [])))
    (is (= #{} (parse (empty-p) ["a"])))
    (is (= #{nil} (parse (eps) [])))
    (is (= #{"a"} (parse (lit "a") ["a"])))))
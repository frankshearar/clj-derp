(ns clj-derp.core-test
  (:use clojure.test
        clj-derp.core))

(deftest parsing-null
  (testing "Parse null"
    (is (= #{nil} (parse-null (eps))))
    (is (= #{} (parse-null (empty-p))))
    (is (= #{} (parse-null (literal "a"))))
    (is (= #{"a"} (parse-null (eps* "a"))))))
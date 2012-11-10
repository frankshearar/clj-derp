(ns clj-derp.core-test
  (:use clojure.test
        clj-derp.core))

(deftest making-parsers
  (testing "Making parsers"
    (is (= (empty-p) (alt)))
;    (is (= (lit "it") (alt (lit "it"))))
    (is (= (lit "left") (force (:left (alt (lit "left") (lit "right"))))))))

 (deftest parsing-null
   (testing "Null parses"
    (is (= #{nil} (parse-null (eps))))
    (is (= #{} (parse-null (empty-p))))
    (is (= #{} (parse-null (lit "a"))))
    (is (= #{"a"} (parse-null (eps* "a"))))
     (testing "of Union"
       (is (= #{"a" "b"} (parse-null (alt (eps* "a") (eps* "b"))))))))
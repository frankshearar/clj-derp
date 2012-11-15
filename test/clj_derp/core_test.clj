(ns clj-derp.core-test
  (:use clojure.test
        clj-derp.core))

(deftest cartesian-product-of-sets
  (let [cat (fn [a b] [a b])]
    (is (= #{} (cart-prod #{} #{} cat)))
    (is (= #{} (cart-prod #{} #{1} cat)))
    (is (= #{} (cart-prod #{1} #{} cat)))
    (is (= #{[1 2]} (cart-prod #{1} #{2} cat)))
    (is (= #{[:a 1] [:a 2] [:b 1] [:b 2]} (cart-prod #{:a :b} #{1 2} cat)))))

(deftest making-parsers
  (testing "Making"
    (testing "reduction parser"
      (let [p (eps* 1)
            fn (fn [x] (+ x 1))]
        (is (= p (:parser (red p fn))))
        (is (= 2 (apply (:fn (red p fn)) [1])))))
    (testing "star parser"
      (let [p (eps* 1)]
        (is (= p (:parser (star p))))))
    (testing "sequence parser"
      (is (= (empty-p) (cat)))
      (is (= (lit "a") (cat (lit "a"))))
      (is (= (lit "first") (force (:first (cat (lit "first") (lit "second"))))))
      (is (= (lit "second") (force (:second (cat (lit "first") (lit "second")))))))
    (testing "union parser"
      (is (= (empty-p) (alt)))
      (is (= (lit "it") (alt (lit "it"))))
      (is (= (lit "left") (force (:left (alt (lit "left") (lit "right"))))))
      (is (= (lit "right") (force (:right (alt (lit "left") (lit "right")))))))))

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
    (testing "red"
      (let [fn (fn [x] (+ 1 x))]
        (is (= (red (eps* "a") fn) (d (red (lit "a") fn) "a")))))
    (testing "star"
      (is (eq (cat (d (lit "a") "a") (star (lit "a")))
              (d (star (lit "a")) "a"))))
    (testing "alt"
      (is (eq (alt (d (lit "a") "a") (d (lit "b") "a"))
              (d (alt (lit "a") (lit "b")) "a"))))
    (testing "cat"
      (testing "when first parser nullable"
        (is (eq (alt (cat (eps* "a") (empty-p))
                     (cat (empty-p) (lit "b")))
                (d (cat (eps* "a") (lit "b")) "not-an-a"))))
      (testing "when first parser not nullable"
        (is (eq (cat (eps* "a") (lit "b"))
                (d (cat (lit "a") (lit "b")) "a")))))))

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
      (is (eq (eps* "a") (eps* "a")))
      (is (eq (eps** #{"a" "b"}) (eps** #{"a" "b"})))
      (is (not (eq (eps) (eps* "a"))))
      (is (not (eq (eps) (lit "a"))))
      (is (not (eq (eps) (alt)))))
    (testing "lit"
      (is (eq (lit "a") (lit "a")))
      (is (not (eq (lit "a") (lit "b")))))
    (testing "red"
      (let [fn identity]
        (is (eq (red (lit "a") fn) (red (lit "a") fn)))
        (is (not (eq (red (lit "a") fn)
                     (red (lit "b") fn))))
        (is (not (eq (red (lit "a") fn)
                     (red (lit "a") (comp fn identity)))))))
    (testing "star"
      (is (eq (star (lit "a")) (star (lit "a"))))
      (is (not (eq (star (lit "a")) (star (lit "b"))))))
    (testing "sequences"
      (is (eq (cat) (cat)))
      (is (eq (cat (lit "a")) (cat (lit "a"))))
      (is (eq (cat (alt (lit "a") (lit "b")) (eps))
              (cat (alt (lit "a") (lit "b")) (eps))))
      (is (not (eq (cat (lit "a")) (cat (lit "b")))))
      (is (not (eq (cat (eps) (lit "a")) (cat (eps) (lit "b"))))))
    (testing "unions"
      (is (eq (alt) (alt)))
      (is (eq (alt (empty-p)) (alt (empty-p))))
      (is (eq (alt (lit "a") (lit "b")) (alt (lit "a") (lit "b"))))
      (is (not (eq (alt (lit "a")) (alt (lit "b")))))
      (is (not (eq (alt (eps) (lit "a")) (alt (eps) (lit "b"))))))))

(deftest nullability
  (testing "Nullability"
    (testing "of empty"
      (is (not (nullable? (empty-p)))))
    (testing "of eps"
      (is (nullable? (eps))))
    (testing "of eps*"
      (is (nullable? (eps* "a")))
      (is (nullable? (eps** #{"a" "b"}))))
    (testing "of lit"
      (is (not (nullable? (lit "a")))))
    (testing "of red"
      (is (nullable? (red (eps* 1) (fn [x] (+ x 1)))))
      (is (not (nullable? (red (empty-p) (fn [_] 1))))))
    (testing "of star"
      (testing "when subparser not empty"
        (is (not (nullable? (star (lit "a")))))
        (is (nullable? (star (eps)))))
      (testing "when subparser empty"
        (is (nullable? (star (empty-p))))
        (is (nullable? (star (cat (empty-p) (empty-p)))))))
    (testing "of cat"
      (is (nullable? (cat (eps) (eps))))
      (is (not (nullable? (cat (empty-p) (eps)))))
      (is (not (nullable? (cat (eps) (empty-p)))))
      (is (not (nullable? (cat (empty-p) (empty-p)))))
      (is (nullable? (cat (cat (eps) (eps)) (eps))))
      (is (nullable? (cat (eps) (cat (eps) (eps))))))
    (testing "of union"
      (is (nullable? (alt (eps) (eps))))
      (is (nullable? (alt (empty-p) (eps))))
      (is (nullable? (alt (eps) (empty-p))))
      (is (not (nullable? (alt (empty-p) (empty-p)))))
      (is (nullable? (alt (alt (empty-p) (eps)) (empty-p))))
      (is (nullable? (alt (empty-p) (alt (empty-p) (eps))))))))

(deftest emptiness
  (testing "of empty"
    (is (empty-p? (empty-p))))
  (testing "of eps"
    (is (not (empty-p? (eps)))))
  (testing "of eps*"
    (is (not (empty-p? (eps* "a")))))
  (testing "of lit"
    (is (not (empty-p? (lit "a")))))
  (testing "of red"
    (is (not (empty-p? (red (eps* 1) (fn [x] (+ 1 x))))))
    (is (empty-p? (red (empty-p) (fn [_] 1)))))
  (testing "of star"
    (is (not (empty-p? (star (empty-p)))))
    (is (not (empty-p? (star (eps)))))
    (is (not (empty-p? (star (eps* "a")))))
    (is (not (empty-p? (star (lit "a")))))
    (is (not (empty-p? (star (cat (eps) (eps))))))
    (is (not (empty-p? (star (alt (empty-p) (empty-p)))))))
  (testing "of cat"
    (is (empty-p? (cat (empty-p) (empty-p))))
    (is (empty-p? (cat (empty-p) (lit "a"))))
    (is (empty-p? (cat (lit "a") (empty-p))))
    (is (not (empty-p? (cat (eps) (eps))))))
  (testing "of alt"
    (is (empty-p? (alt (empty-p) (empty-p))))
    (is (not (empty-p? (alt (empty-p) (eps)))))
    (is (not (empty-p? (alt (eps) (empty-p)))))
    (is (not (empty-p? (alt (eps) (eps)))))))

(deftest parsing-null
  (testing "Null parses"
    (is (= #{nil} (parse-null (eps))))
    (is (= #{} (parse-null (empty-p))))
    (is (= #{} (parse-null (lit "a"))))
    (is (= #{"a"} (parse-null (eps* "a"))))
    (testing "of red"
      (is (= #{1 2} (parse-null (red (eps** #{0 1}) (fn [x] (+ 1 x)))))))
    (testing "of Cat"
      (is (= #{} (parse-null (cat))))
      (is (= #{["a" "b"]} (parse-null (cat (eps* "a") (eps* "b")))))
      (is (= #{["a" "b"] ["c" "b"]} (parse-null (cat (eps** #{"a" "c"}) (eps* "b"))))))
    (testing "of Union"
      (is (= #{} (parse-null (alt))))
      (is (= #{"a" "b"} (parse-null (alt (eps* "a") (eps* "b"))))))))

(deftest parsing
  (testing "Basic parse tests"
    (is (= #{} (parse (empty-p) [])))
    (is (= #{} (parse (empty-p) ["a"])))
    (is (= #{nil} (parse (eps) [])))
    (is (= #{"a"} (parse (lit "a") ["a"])))))
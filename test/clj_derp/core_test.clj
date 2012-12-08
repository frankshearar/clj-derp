(ns clj-derp.core-test
  (:use clojure.test
        clj-derp.core)
  (:require [clojure.string :as str]))

(deftest cartesian-product-of-sets
  (let [cat (fn [a b] [a b])]
    (is (= #{} (cart-prod #{} #{} cat)))
    (is (= #{} (cart-prod #{} #{1} cat)))
    (is (= #{} (cart-prod #{1} #{} cat)))
    (is (= #{[1 2]} (cart-prod #{1} #{2} cat)))
    (is (= #{[:a 1] [:a 2] [:b 1] [:b 2]} (cart-prod #{:a :b} #{1 2} cat)))))

(deftest making-parsers
  (testing "Making"
    (testing "literal parser"
      (is (= (lit "a") (lit+ "a")))
      (is (= (eps) (lit+)))
      (is (= (lit "a") (lit+ "a")))
      (is (= (lit+ "a" "b") (lit+ "a" "b")))
      (is (= (lit+ "a" "b") (lit* ["a" "b"]))))
    (testing "reduction parser"
      (let [p (eps* 1)
            fn (fn [x] (+ x 1))]
        (is (= p (:parser (red p fn))))
        (is (= 2 (apply (:fn (red p fn)) [1])))))
    (testing "star parser"
      (let [p (eps* 1)]
        (is (= p (:parser (star p))))))
    (testing "delegate"
      (is (delegate? (-->)))
      (let [p (lit "a")
            dp (--> p)]
        (is (= p (deref (:ref dp)))))) ; <-- :ref's an atom, after all.
    (testing "sequence parser"
      (is (= (empty-p) (cat)))
      (is (= (lit "a") (cat (lit "a"))))
      (is (= (lit "first") (force (:fst (cat (lit "first") (lit "second"))))))
      (is (= (lit "second") (force (:snd (cat (lit "first") (lit "second")))))))
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
      (is (= (eps* "a") (d (lit "a") "a")))
      (is (= (eps* "a") (d (lit+ "a" "b") "a")))
      (is (= (eps* "b") (d (lit+ "a" "b") "b")))
      (is (= (empty-p) (d (lit+ "a" "b") "c"))))
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
      (is (not (eq (lit "a") (lit "b"))))
      (is (eq (lit+ "a" "b") (lit+ "a" "b")))
      (is (eq (lit+ "a" "b") (lit+ "b" "a")))
      (is (not (eq (lit+ "a" "b") (lit+ "b" "c")))))
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
      (is (not (nullable? (lit "a"))))
      (is (not (nullable? (lit+ "a" "b")))))
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
    (is (not (empty-p? (lit "a"))))
    (is (not (empty-p? (lit+ "a" "b")))))
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
    (is (= #{} (parse-null (lit+ "a" "b"))))
    (is (= #{"a"} (parse-null (eps* "a"))))
    (testing "of red"
      (is (= #{1 2} (parse-null (red (eps** #{0 1}) (fn [x] (+ 1 x)))))))
    (testing "of star"
      (is (= #{[]} (parse-null (star (lit "a"))))
          (= #{[]} (parse-null (star (eps))))))
    (testing "of Cat"
      (is (= #{} (parse-null (cat))))
      (is (= #{["a" "b"]} (parse-null (cat (eps* "a") (eps* "b")))))
      (is (= #{["a" "b"] ["c" "b"]} (parse-null (cat (eps** #{"a" "c"}) (eps* "b"))))))
    (testing "of Union"
      (is (= #{} (parse-null (alt))))
      (is (= #{"a" "b"} (parse-null (alt (eps* "a") (eps* "b"))))))))

(deftest compacting
  (testing "empty"
    (is (eq (empty-p) (compact (empty-p)))))
  (testing "eps"
    (is (eq (eps) (compact (eps))))
    (is (eq (eps* "a") (compact (eps* "a"))))
    (is (eq (eps** #{"a" "b"}) (compact (eps** #{"a" "b"})))))
  (testing "lit"
    (is (eq (lit "a") (compact (lit "a"))))
    (is (eq (lit+ "a" "b") (compact (lit+ "a" "b"))))
    )
  (testing "red"
    ;; Compaction of red is red of compacted subparser
    (is (eq (red (eps* "a") identity)
            (compact (red (eps* "a") identity))))
    (is (eq (red (eps* "a") identity)
            (compact (red (alt (empty-p) (eps* "a")) identity))))
    (testing "-red -> red"
      (let [inner-fn (fn [a] (+ a 1))
            outer-fn (fn [a] (* a 2))
            red (compact (red (red (eps* 1) inner-fn) outer-fn))]
        (is (= (apply (comp outer-fn inner-fn) [1])
               (apply (:fn red) [1]))))))
  (testing "star"
    ;; Compaction of star is star of compacted subparser
    (is (eq (star (eps)) (compact (star (alt (empty-p) (eps)))))))
  (testing "alt"
    (is (eq (alt (eps* "a") (eps* "b"))
            (compact (alt (eps* "a") (eps* "b")))))
    (is (eq (empty-p) (compact (alt (empty-p) (empty-p)))))
    (is (eq (eps) (compact (alt (eps) (empty-p)))))
    (is (eq (eps) (compact (alt (empty-p) (eps)))))
    (testing "compacts deeply"
      ;; Compaction recurs through the subparsers
      (is (eq (lit "a") (compact (alt (alt (empty-p) (lit "a")) (empty-p)))))
      (is (eq (lit "a") (compact (alt (empty-p) (alt (empty-p) (lit "a"))))))))
  (testing "cat"
    (let [ab (cat (lit "a") (lit "b"))]
      (is (= ab (compact ab))))
    ;; singleton parse -> red
    (testing "with singleton parses"
      (testing "for first parser"
        (let [c (compact (cat (eps* \a) (eps** #{\a \b})))]
          (is (red? c))
          (is (= #{'(\a \a) '(\a \b)} (parse-null c)))))
      (testing "for second parser"
        (let [c (compact (cat (eps** #{\a \b}) (eps* \b)))]
          (is (red? c))
          (is (= #{'(\a \b) '(\b \b)} (parse-null c))))))
    (testing "with empty subparsers"
      (testing "(first)"
        (is (eq (eps) (compact (cat (empty-p) (eps))))))
      (testing "(second)"
        (is (eq (eps) (compact (cat (eps) (empty-p)))))))
    (testing "compacts deeply"
      ;; Compaction recurs through the subparsers
      (is (eq (cat (lit "a") (lit "b")) (compact (cat (lit "a") (alt (empty-p) (lit "b"))))))
      (is (eq (cat (lit "a") (lit "b")) (compact (cat (alt (empty-p) (lit "a"))(lit "b"))))))))

(deftest parsing
  (testing "Basic parse tests"
    (is (= #{} (parse (empty-p) '())))
    (is (= #{} (parse (empty-p) ["a"])))
    (is (= #{nil} (parse (eps) '())))
    (is (= #{"a"} (parse (lit "a") ["a"])))
    (is (= #{"a"} (parse (lit+ "a" "b") ["a"])))
    (is (= #{"b"} (parse (lit+ "a" "b") ["b"])))
    (is (= #{'("a" ())} (parse (star (lit "a")) ["a"])))
    (is (= #{'("a" ("a" ()))} (parse (star (lit "a")) ["a" "a"])))))

(deftest singleton-parse-test
  (testing "no parse trees"
    (is (not (singleton-parse? (empty-p)))))
  (testing "one parse tree"
    (is (singleton-parse? (eps)))
    (is (singleton-parse? (eps* "a"))))
  (testing "many parse trees"
    (is (not (singleton-parse? (eps** #{"a" "b"}))))
    (is (not (singleton-parse? (alt (eps* "a") (eps* "b")))))))

(deftest testing-classifiers
  (testing "delegate?"
    (is (delegate? (-->)))
    (is (not (delegate? (empty-p))))
    (is (not (delegate? (eps))))
    (is (not (delegate? (lit "a"))))
    (is (not (delegate? (star (eps)))))
    (is (not (delegate? (red (eps) identity))))
    (is (not (delegate? (alt (eps) (eps)))))
    (is (not (delegate? (cat (eps) (eps))))))
  (testing "red?"
    (is (red? (red (eps) identity)))
    (is (not (red? (empty-p))))
    (is (not (red? (eps))))
    (is (not (red? (eps* "a"))))
    (is (not (red? (lit "a"))))
    (is (not (red? (star (lit "a")))))
    (is (not (red? (alt (eps) (eps)))))
    (is (not (red? (cat (eps) (eps)))))))

(deftest testing-in
  (is (not (in? 1 [])))
  (is (not (in? 1 [2])))
  (is (in? 1 [1]))
  (is (not (in? 1 [2 3 4])))
  (is (in? 1 [2 1 3 4])))

(deftest testing-not-in
  (is (not-in? 1 []))
  (is (not-in? 1 [2]))
  (is (not (not-in? 1 [1])))
  (is (not-in? 1 [2 3 4]))
  (is (not (not-in? 1 [2 1 3 4]))))

(deftest structural-inspection
  (is (= [] (subparsers (empty-p))))
  (is (= [] (subparsers (eps))))
  (is (= [] (subparsers (lit "a"))))
  (is (= [(eps)]) (subparsers (red (eps) identity)))
  (is (= [(eps)] (subparsers (star (eps)))))
  (is (= [(eps)]) (subparsers (--> (eps))))
  (let [a (lit "a")
        b (lit "b")]
    (is (= [a b] (subparsers (alt a b))))
    (is (= [a b] (subparsers (cat a b))))))

(extend-type clojure.lang.IPersistentVector
  ComparableParser
  (eq [this that] (some #(not (eq %1 %2)) (map eq this that))))

(deftest traversing
  (is (= [(empty-p)] (keys (search (empty-p) identity))))
  (is (= [(eps)] (keys (search (eps) identity))))
  (let [a (lit "a")
        b (lit "b")
        a+b (alt a b)
        ab (cat a b)]
    (is (= [a+b a b] (keys (search a+b identity))))
    (is (= [ab a b] (keys (search ab identity))))))

(deftest testing-mark-uniquely
  (let [a (empty-p)
        b (eps)
        a+b (alt (empty-p) (eps))
        marked (mark-uniquely a+b)]
    (is (= 0 (get marked a+b)))
    (is (= 1 (get marked a)))
    (is (= 2 (get marked b)))))

(defn expect-print [s p]
  (let [int-map (mark-uniquely p)]
    (is (= s (print-node p int-map)))))

(deftest printing
  (expect-print "\"0\" [label=\"empty\"]" (empty-p))
  (expect-print "\"0\" [shape=\"record\", label=\"eps* | #{\"a\"}\"]" (eps* "a"))
  (expect-print "\"0\" [shape=\"record\", label=\"eps* | #{\"a\" \"b\"}\"]" (eps** #{"a" "b"}))
  (expect-print "\"0\" [shape=\"record\", label=\"eps* | #{nil}\"]" (eps))
  (expect-print "\"0\" [shape=\"record\", label=\"token | 1\"]" (lit 1))
  (expect-print "\"0\" [shape=\"record\", label=\"token | #{1 2}\"]" (lit+ 1 2))
  (expect-print "\"0\" [label=\"red\"]\n\"0\" -> \"1\"" (red (empty-p) identity))
  (expect-print "\"0\" [label=\"star\"]\n\"0\" -> \"1\"" (star (empty-p)))
  (expect-print (str/join "\n"
                          ["\"0\" [label=\"or\"]"
                           "\"0\" -> \"1\""
                           "\"0\" -> \"2\""])
                (alt (empty-p) (eps* "a")))
  (expect-print (str/join "\n"
                          ["\"0\" [shape=\"none\", margin=0, label=<<table border=\"0\" cellborder=\"1\" cellspacing=\"0\" cellpadding=\"4\"><tr><td colspan=\"2\">seq</td></tr><tr><td port=\"L\">L</td><td port=\"R\">R</td></tr></table>>]"
                           "\"0\":L -> \"1\""
                           "\"0\":R -> \"2\""])
                (cat (empty-p) (eps* "a")))
  (is (= "digraph {\n\"0\" [shape=\"none\", margin=0, label=<<table border=\"0\" cellborder=\"1\" cellspacing=\"0\" cellpadding=\"4\"><tr><td colspan=\"2\">seq</td></tr><tr><td port=\"L\">L</td><td port=\"R\">R</td></tr></table>>]\n\"0\":L -> \"1\"\n\"0\":R -> \"4\"\n\"3\" [shape=\"record\", label=\"token | 2\"]\n\"2\" [shape=\"record\", label=\"token | 1\"]\n\"1\" [label=\"or\"]\n\"1\" -> \"2\"\n\"1\" -> \"3\"\n\"4\" [label=\"empty\"]\n}")) (print-as-digraph (cat (alt (lit 1) (lit 2)) (empty-p))))
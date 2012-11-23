(ns clj-derp.core
  (:require [clojure.set :as set]))

(defn cart-prod [set-one set-two concat-fn]
  "Return the Cartesian product of set-one and set-two."
  (set (mapcat
        (fn [a] (map
                 (fn [b]
                   (apply concat-fn [a b]))
                 set-two))
        set-one)))

(defmacro defn-fix [name bottom fn]
  `(let* [*cache*# (atom {})
       *changed?*# (atom false)
       *running?*# (atom false)
       *visited*# (atom {})]
      (defn ~name [x#]
        (let [cached?# (contains? (deref *cache*#) x#)
              cached# (get (deref *cache*#) x#)
              run?# (deref *running?*#)]
          (cond
           (and cached?# (not run?#))
           cached#
           (and run?# (get (deref *visited*#) x#))
           (if cached?# cached# ~bottom)
           run?#
           (do
             (swap! *visited*# assoc x# true)
             (let [new-val# (apply ~fn [x#])]
               (when (not (= new-val# cached#))
                 (swap! *changed?*# (fn [_#] true))
                 (swap! *cache*# assoc x# new-val#))
               new-val#))
           (and (not cached?#) (not run?#))
           (do
             (swap! *changed?*# (fn [_#] true))
             (swap! *running?*# (fn [_#] true))
             (swap! *visited*# (fn [_#] {}))
             (let [v# (atom ~bottom)]
               (while (deref *changed?*#)
                 (swap! *changed?*# (fn [_#] false))
                 (swap! *visited*# (fn [_#] {}))
                 (swap! v# (fn [_#] (apply ~fn [x#]))))
               (deref v#))))))))

(defprotocol Parser
  (d [this token])
  (compact-int [this])
  (empty-int? [this])
  (nullable-int? [this])
  (parse-null-int [this]))

;; Since we use Delays, equality comparison becomes troublesome.
;; eq gives us a chance to force any delays, permitting us to
;; still compare graphs for structural equality.
(defprotocol ComparableParser
  (eq [this that]))

(def compact (memoize (fn [parser] (compact-int parser))))
(defn-fix parse-null {} (fn [parser] (parse-null-int parser)))
(defn-fix nullable? false (fn [parser] (nullable-int? parser)))
(defn-fix empty-p? false (fn [parser] (empty-int? parser)))

(defn singleton-parse? [parser]
  (= 1 (count (parse-null parser))))

;; We forward declare the helper constructors because we use them
;; in the defrecords.
(declare empty-p)
(declare eps)
(declare eps*)
(declare eps**)
(declare lit)
(declare alt)
(declare cat)
(declare red)
(declare star)

;; We have to forward declare helpers like these because we use them
;; in the defrecords.
(declare red?)

(defrecord empty-parser []
  ComparableParser
  (eq [this that]
    (= this that))
  Parser
  (d [this _] this)
  (compact-int [this] this)
  (empty-int? [_] true)
  (nullable-int? [this] false)
  (parse-null-int [_] #{}))

(defrecord empty-string-parser [tree-set]
  ComparableParser
  (eq [this that]
    (= this that))
  Parser
  (d [this _] (empty-p))
  (compact-int [this] this)
  (empty-int? [_] false)
  (nullable-int? [this] true)
  (parse-null-int [_] tree-set))

(defrecord literal-parser [token]
  ComparableParser
  (eq [this that]
    (= this that))
  Parser
  (d [this t]
    (if (= token t)
      (eps* token)
      (empty-p)))
  (compact-int [this] this)
  (empty-int? [_] false)
  (nullable-int? [this] false)
  (parse-null-int [_] #{}))

(defrecord literal-set-parser [token-set]
  ;; A parser that can consume one of a set of literals. You may think of
  ;; (lit+ 1 2) as being an optimised form of (alt (lit 1) (lit 2)).
  ComparableParser
  (eq [this that]
    (= this that))
  Parser
  (d [this t]
    (if (contains? token-set t)
      (eps* t)
      (empty-p)))
  (compact-int [this] this)
  (empty-int? [_] false)
  (nullable-int? [this] false)
  (parse-null-int [_] #{}))

(defrecord red-parser [parser fn]
  ComparableParser
  ;; There's a major limitation here: there is no = for
  ;; functions, so if you really care about equality of
  ;; reduction parsers, you need to control what functions
  ;; you use: perhaps define a handful of primitives that
  ;; you can compose as necessary.
  (eq [this that]
    (and (eq (:parser this) (:parser that))
         (= (:fn this) (:fn that))))
  Parser
  (d [this t]
    (red (d (:parser this) t) (:fn this)))
  (compact-int [this]
    (cond
     (red? (:parser this))
     (red
      (compact (:parser (:parser this)))
      (comp (:fn this) (:fn (:parser this))))
     :else (red (compact (:parser this)) (:fn this))))
  (empty-int? [this] (empty-p? (:parser this)))
  (nullable-int? [this]
    (nullable? (:parser this)))
  (parse-null-int [this]
    (set (map (:fn this) (parse-null (:parser this))))))

(defrecord star-parser [parser]
  ComparableParser
  (eq [this that]
    (= this that))
  Parser
  (d [this t]
    (cat (d (:parser this) t) this))
  (compact-int [this]
    (star (compact (:parser this))))
  (empty-int? [_] false)
  (nullable-int? [this]
    (or (nullable? (:parser this))
        (empty-p? (:parser this))))
  (parse-null-int [this]
    #{'()}))

(defn- -is-seq [t]
  "If t isn't a seq (a list), make it one"
  (if (seq? t) t (list t)))

(defn- -append [t l]
  "Add t to the back of l, forcing either or both into listy-ness"
  (concat (-is-seq l) (-is-seq t)))
(defn- -prepend [t l]
  "Add t to the front of l, forcing either or both into listy-ness"
  (concat (-is-seq t) (-is-seq l)))

(defrecord sequence-parser [fst snd]
  ComparableParser
  (eq [this that]
    (and (eq (force (:fst this)) (force (:fst that)))
         (eq (force (:snd this)) (force (:snd that)))))
  Parser
  (d [this t]
    (let [fst (force (:fst this))
          snd (force (:snd this))]
      (if (nullable? fst)
        (alt (cat (eps** (parse-null fst))
                  (d snd t))
             (cat (d fst t) snd))
        (cat (d fst t) snd))))
  (compact-int [this]
    (let [fst (force (:fst this))
          snd (force (:snd this))]
      (cond
       (empty-p? fst)
       snd
       (empty-p? snd)
       fst
       (singleton-parse? fst)
       (let [t (first (parse-null fst))]
         (red snd
              (fn [w2] (-prepend t w2))))
       (singleton-parse? snd)
       (let [t (first (parse-null snd))]
         (red fst
              (fn [w1] (-append t w1))))
       :else
       this)))
  (empty-int? [this]
    (or (empty-p? (force (:fst this)))
        (empty-p? (force (:snd this)))))
  (nullable-int? [this]
    (and (nullable? (force (:fst this)))
         (nullable? (force (:snd this)))))
  (parse-null-int [this]
    (cart-prod
     (parse-null (force (:fst this)))
     (parse-null (force (:snd this)))
     (fn [a b] [a b]))))

(defrecord union-parser [left right]
  ComparableParser
  (eq [this that]
    (and (eq (force (:left this)) (force (:left that)))
         (eq (force (:right this)) (force (:right that)))))
  Parser
  (d [this t]
    (alt (d (force (:left this)) t)
         (d (force (:right this)) t)))
  (compact-int [this]
    (let [l-empty (eq (empty-p) (force (:left this)))
          r-empty (eq (empty-p) (force (:right this)))]
      (cond
       (and l-empty r-empty)
       (empty-p)
       (and (not l-empty) r-empty)
       (force (:left this))
       (and l-empty (not r-empty))
       (force (:right this))
       :else
       this)))
  (empty-int? [this]
    (and (empty-p? (force (:left this)))
         (empty-p? (force (:right this)))))
  (nullable-int? [this]
    (or (nullable? (force (:left this)))
        (nullable? (force (:right this)))))
  (parse-null-int [p]
    (set/union
     (parse-null (force (:left p)))
     (parse-null (force (:right p))))))

;; Utility constructors
(defn empty-p [] (empty-parser.))
(defn eps [] (empty-string-parser. #{nil}))
(defn eps* [token] (empty-string-parser. #{token}))
;; A temporary function until I find out the syntax for multimethods
(defn eps** [token] (empty-string-parser. token))
(defn lit [token] (literal-parser. token))
(defn lit+ [& rest]
  (case (count rest)
    0 (eps)
    1 (lit (first rest))
    (literal-set-parser. (set rest))))
(defn alt
  ([] (empty-p))
  ([a & parsers]
     (if parsers
       (union-parser. (delay a) (delay (apply alt parsers)))
       a)))
(defn cat
  ([] (empty-p))
  ([a & parsers]
     (if parsers
       (sequence-parser. (delay a) (delay (apply cat parsers)))
       a)))
(defn red [parser arity-1-fn]
  (red-parser. parser arity-1-fn))
(defn star [parser]
  (star-parser. parser))

(defn red? [parser]
  (instance? red-parser parser))

(defn parse [parser input]
  (if (empty? input)
    (parse-null parser)
    (parse (d parser (first input)) (rest input))))
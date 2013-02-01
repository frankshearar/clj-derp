(ns clj-derp.core
  ^{:author "Frank Shearar",
    :doc
"This file defines the derivative of a context free grammar,
and provides a simple API for parsing streams."}
  (:require [clojure.set :as set])
  (:require [clojure.string :as string])
  (:require [clojure.core.cache :as cache]))

(defn cart-prod [set-one set-two concat-fn]
  "Return the Cartesian product of set-one and set-two."
  (set (mapcat
        (fn [a] (map
                 (fn [b]
                   (apply concat-fn [a b]))
                 set-two))
        set-one)))

(defmacro defn-fix [name bottom fn]
  `(let* [*cache*# (atom (cache/soft-cache-factory {}))
       *changed?*# (atom false)
       *running?*# (atom false)
       *visited*# (atom {})]
      (defn ~name [x#]
        (let [cached?# (cache/has? (deref *cache*#) x#)
              cached# (cache/lookup (deref *cache*#) x#)
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
                 (swap! *cache*# (fn [c#] (if cached?#
                                            (cache/hit c# x#)
                                            (cache/miss c# x# new-val#)))))
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

(defn- memoize-with-cache [f new-cache]
  (let [cache (atom new-cache)
        recalc (fn [args]
                 (let [answer (apply f args)]
                   (swap! cache (fn [c] (cache/miss c args answer)))
                   answer))]
    (fn [& args]
      (if (cache/has? @cache args)
        (do
          (swap! cache (fn [c] (cache/hit c args)))
          (if-let [answer (cache/lookup @cache args)]
            answer
            (recalc args)))
        (recalc args)))))

(defn memoized
  ^{:doc "Given a function f, return a memoized function using either a given cache, or a cache using soft references."}
  ([f] (memoize-with-cache f (cache/soft-cache-factory {})))
  ([f cache] (memoize-with-cache f cache)))

(defprotocol Parser
  (d-int [this token])
  (compact-int [this])
  (empty-int? [this])
  (nullable-int? [this])
  (parse-null-int [this]))

(defprotocol ComparableParser
  "Since we use Delays, equality comparison becomes troublesome. This protocol gives us a chance to force any delays, permitting us to still compare graphs for structural equality."
  (eq [this that]))

(defprotocol StructuralParser
  "A means for a parser to expose its subparsers in a uniform manner."
  (subparsers [this]))

(defprotocol PrintableParser
  "A protocol to provide for the printing of a graph in dotfile format."
  (print-node [this int-map]
    "Print this parser as part of a dotfile. int-map maps parsers to integers."))

(defprotocol SettableParser
  (set-ref! [this parser]
    "Set the reference of a delegate-parser."))

(def compact (memoized (fn [parser] (compact-int parser))))
(def d (memoized (fn [parser token] (d-int parser token))))
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

(defrecord delegate-parser [ref]
  ComparableParser
  (eq [this that]
    (eq (:ref this) that))
  StructuralParser
  (subparsers [this]
    (subparsers (deref (:ref this))))
  SettableParser
  (set-ref! [this new-parser-ref]
    (swap! (:ref this) (fn [_] new-parser-ref)))
  Parser
  (d-int [this t] (d (:ref this) t))
  (compact-int [this] (compact (:ref this)))
  (empty-int? [this] (empty-int? (:ref this)))
  (nullable-int? [this] (nullable-int? (:ref this)))
  (parse-null-int [this] (parse-null-int (:ref this))))

(defrecord empty-parser []
  ComparableParser
  (eq [this that]
    (= this that))
  StructuralParser
  (subparsers [_] [])
  PrintableParser
  (print-node [this int-map]
    (format "\"%s\" [label=\"empty\"]" (get int-map this :not-found)))
  Parser
  (d-int [this _] this)
  (compact-int [this] this)
  (empty-int? [_] true)
  (nullable-int? [this] false)
  (parse-null-int [_] #{}))

(defrecord empty-string-parser [tree-set]
  ComparableParser
  (eq [this that]
    (= this that))
  StructuralParser
  (subparsers [_] [])
  PrintableParser
  (print-node [this int-map]
    (format "\"%s\" [shape=\"record\", label=\"eps* | %s\"]" (get int-map this) tree-set))
  Parser
  (d-int [this _] (empty-p))
  (compact-int [this] this)
  (empty-int? [_] false)
  (nullable-int? [this] true)
  (parse-null-int [_] tree-set))

(defrecord literal-parser [token]
  ComparableParser
  (eq [this that]
    (= this that))
  StructuralParser
  (subparsers [_] [])
  PrintableParser
  (print-node [this int-map]
    (format "\"%s\" [shape=\"record\", label=\"token | %s\"]" (get int-map this) token))
  Parser
  (d-int [this t]
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
  StructuralParser
  (subparsers [_] [])
  PrintableParser
  (print-node [this int-map]
    (format "\"%s\" [shape=\"record\", label=\"token | %s\"]" (get int-map this) token-set))
  Parser
  (d-int [this t]
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
  StructuralParser
  (subparsers [this] [(:parser this)])
  PrintableParser
  (print-node [this int-map]
    (let [this-n (get int-map this)
          sub-n (get int-map parser)]
      (string/join "\n" [(format "\"%s\" [label=\"red\"]" this-n)
                         (format "\"%s\" -> \"%s\"" this-n sub-n)])))
  Parser
  (d-int [this t]
    (red (d parser t) fn))
  (compact-int [this]
    (cond
     (red? parser)
     (red
      (compact parser)
      (comp fn (:fn parser)))
     :else (red (compact parser) fn)))
  (empty-int? [this] (empty-p? parser))
  (nullable-int? [this]
    (nullable? parser))
  (parse-null-int [this]
    (set (map fn (parse-null parser)))))

(defrecord star-parser [parser]
  ComparableParser
  (eq [this that]
    (= this that))
  StructuralParser
  (subparsers [this] [parser])
  PrintableParser
  (print-node [this int-map]
    (let [this-n (get int-map this)
          sub-n (get int-map parser)]
      (string/join "\n" [(format "\"%s\" [label=\"star\"]" this-n)
                         (format "\"%s\" -> \"%s\"" this-n sub-n)])))
  Parser
  (d-int [this t]
    (cat (d parser t) this))
  (compact-int [this]
    (star (compact parser)))
  (empty-int? [_] false)
  (nullable-int? [this]
    (or (nullable? parser)
        (empty-p? parser)))
  (parse-null-int [this]
    #{'()}))

(defn- -is-seq [t]
  "If t isn't a seq (a list), make it one. Can't use sequence because t might not be a collection."
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
  StructuralParser
  (subparsers [this] [(force fst) (force snd)])
  PrintableParser
  (print-node [this int-map]
    (let [this-n (get int-map this)
          fst-n (get int-map (force fst))
          snd-n (get int-map (force snd))]
      (string/join "\n" [(format "\"%s\" [shape=\"none\", margin=0, label=<<table border=\"0\" cellborder=\"1\" cellspacing=\"0\" cellpadding=\"4\"><tr><td colspan=\"2\">seq</td></tr><tr><td port=\"L\">L</td><td port=\"R\">R</td></tr></table>>]" this-n)
                         (format "\"%s\":L -> \"%s\"" this-n fst-n)
                         (format "\"%s\":R -> \"%s\"" this-n snd-n)])))
    Parser
  (d-int [this t]
    (let [fst (force fst)
          snd (force snd)]
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
       (let [c1 (compact fst)
             c2 (compact snd)]
         (if (= [c1 c2] [fst snd]) ; = effectively means pointer equality here
           this
           (cat c1 c2))))))
  (empty-int? [this]
    (or (empty-p? (force fst))
        (empty-p? (force snd))))
  (nullable-int? [this]
    (and (nullable? (force fst))
         (nullable? (force snd))))
  (parse-null-int [this]
    (cart-prod
     (parse-null (force fst))
     (parse-null (force snd))
     (fn [a b] [a b]))))

(defrecord union-parser [left right]
  ComparableParser
  (eq [this that]
    (and (eq (force (:left this)) (force (:left that)))
         (eq (force (:right this)) (force (:right that)))))
  StructuralParser
  (subparsers [this] [(force (:left this)) (force (:right this))])
  PrintableParser
  (print-node [this int-map]
    (let [this-n (get int-map this)
          left-n (get int-map (force left))
          right-n (get int-map (force right))]
      (string/join "\n" [(format "\"%s\" [label=\"or\"]" this-n)
                         (format "\"%s\" -> \"%s\"" this-n left-n)
                         (format "\"%s\" -> \"%s\"" this-n right-n)])))
  Parser
  (d-int [this t]
    (alt (d (force (:left this)) t)
         (d (force (:right this)) t)))
  (compact-int [this]
    (let [l-empty (eq (empty-p) (force (:left this)))
          r-empty (eq (empty-p) (force (:right this)))]
      (cond
       (and l-empty r-empty)
       (empty-p)
       (and (not l-empty) r-empty)
       (compact (force (:left this)))
       (and l-empty (not r-empty))
       (compact (force (:right this)))
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
(defn empty-p []
  "The empty language parser."
  (empty-parser.))
(defn eps []
  "That parser whose language is just the empty string."
  (empty-string-parser. #{nil}))
(defn eps* [token]
  "A parser that stores a partial parse tree. You probably don't need to use this function."
  (empty-string-parser. #{token}))
;; A temporary function until I find out the syntax for multimethods
(defn eps** [token-set]
  "Like eps*, but takes a set of parse trees."
  (empty-string-parser. token-set))
(defn lit [token]
  "A parser that consumes exactly this token."
  (literal-parser. token))
(defn lit+ [& rest]
  "Given any number of tokens, make a parser that can accept any of them."
  (case (count rest)
    0 (eps)
    1 (lit (first rest))
    (literal-set-parser. (set rest))))
(defn lit* [l]
  "Given a seq of tokens, make a parser that can accept any of them"
  (apply lit+ l))
(defn alt
  "Make a parser that can act as any of a number of subparsers"
  ([] (empty-p))
  ([a & parsers]
     (if parsers
       (union-parser. (delay a) (delay (apply alt parsers)))
       a)))
(defn cat
  "Define a parser whose subparsers each consume things in sequence"
  ([] (empty-p))
  ([a & parsers]
     (if parsers
       (sequence-parser. (delay a) (delay (apply cat parsers)))
       a)))
(defn red [parser arity-1-fn]
  "Define a parser that runs some function over the parse trees of its subparsers. This is the hook for any semantic actions you might need."
  (red-parser. parser arity-1-fn))
(defn star [parser]
  "The Kleene star parser"
  (star-parser. parser))
(defn -->
  ([] (delegate-parser. (atom nil)))
  ([parser] (delegate-parser. (atom parser))))

(defn delegate? [parser]
  (instance? delegate-parser parser))

(defn red? [parser]
  (instance? red-parser parser))

(defn parse [parser input]
  (if (empty? input)
    (parse-null parser)
    (parse (d parser (first input)) (rest input))))

(defn in? [needle haystack]
  (some #(= needle %) haystack))

(defn not-in? [needle haystack]
  (not (in? needle haystack)))

(defn- search-1 [p fn seed cache]
  "Apply fn to each subparser in p in pre-order, returning the collected results in a dictionary mapping p to (fn p). Skip elements in cache."
  (let [val (apply fn [p])]
    (assoc (merge
            seed
            (reduce merge (map #(search-1 % fn seed (conj cache p))
                               (filter #(not-in? % cache) (subparsers p)))))
      p
      val)))

(defn search [p fn]
  "A cycle-safe map over p."
  (let [visited #{}]
    (search-1 p fn {} visited)))

(defn mark-uniquely [parser]
  "Recursively 'mark' the subparsers of p by associating each parser with a unique integer"
  (let [n (atom -1)]
    (search parser (fn [p] (swap! n inc)))))

(defn print-as-digraph [parser]
  "Return a parser in a dot format string."
  (let [int-map (mark-uniquely parser)]
    (string/join "\n"
                 (concat
                  ["digraph {"]
                  (vals (search parser #(print-node % int-map)))
                  ["}"]))))
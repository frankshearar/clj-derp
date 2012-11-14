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
        ;(prn x#)
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
  (nullable-int? [this])
  (parse-null-int [this]))

;; Since we use Delays, equality comparison becomes troublesome.
;; eq gives us a chance to force any delays, permitting us to
;; still compare graphs for structural equality.
(defprotocol ComparableParser
  (eq [this that]))

(defn-fix parse-null {} (fn [parser] (parse-null-int parser)))
(defn-fix nullable? false (fn [parser] (nullable-int? parser)))

;; We forward declare the helper constructors because we use them
;; in the deftypes.
(declare empty-p)
(declare eps)
(declare eps*)
(declare eps**)
(declare lit)
(declare alt)
(declare cat)

(defrecord empty-parser []
  ComparableParser
  (eq [this that]
    (= this that))
  Parser
  (d [this _] this)
  (nullable-int? [this] false)
  (parse-null-int [_] #{}))

(defrecord empty-string-parser [treeSet]
  ComparableParser
  (eq [this that]
    (= this that))
  Parser
  (d [this _] (empty-p))
  (nullable-int? [this] true)
  (parse-null-int [_] treeSet))

(defrecord literal-parser [token]
  ComparableParser
  (eq [this that]
    (= this that))
  Parser
  (d [this t]
    (if (= token t)
      (eps* token)
      (empty-p)))
  (nullable-int? [this] false)
  (parse-null-int [_] #{}))

(defrecord sequence-parser [first second]
  ComparableParser
  (eq [this that]
    (and (eq (force (:first this)) (force (:first that)))
         (eq (force (:second this)) (force (:second that)))))
  Parser
  (nullable-int? [this]
    (and (nullable? (force (:first this)))
         (nullable? (force (:second this)))))
  (d [this t]
    (if (nullable? (force (:first this)))
      (alt (cat (eps** (parse-null (force (:first this))))
                (d (force (:second this)) t))
           (cat (d (force (:first this)) t)
                (force (:second this))))
      (cat (d (force (:first this)) t) (force (:second this)))))
  (parse-null-int [this]
    (cart-prod
     (parse-null (force (:first this)))
     (parse-null (force (:second this)))
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

(defn parse [parser input]
  (if (empty? input)
    (parse-null parser)
    (parse (d parser (first input)) (rest input))))
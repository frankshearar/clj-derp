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
      (defn parse-null [x#]
        ;(prn x#)
        (let [cached?# (contains? (deref *cache*#) x#)
              cached# (get (deref *cache*#) x#)
              run?# (deref *running?*#)]
          (cond
           (and cached?# (not run?#))
           cached#
           (and run?# (get (deref *visited*#) x#))
           (if cached?# cached# {})
           run?#
           (do
             (swap! *visited*# assoc x# true)
             (let [new-val# (parse-null-int x#)]
               (when (not (= new-val# cached#))
                 (swap! *changed?*# (fn [_#] true))
                 (swap! *cache*# assoc x# new-val#))
               new-val#))
           (and (not cached?#) (not run?#))
           (do
             (swap! *changed?*# (fn [_#] true))
             (swap! *running?*# (fn [_#] true))
             (swap! *visited*# (fn [_#] {}))
             (let [v# (atom {})]
               (while (deref *changed?*#)
                 (swap! *changed?*# (fn [_#] false))
                 (swap! *visited*# (fn [_#] {}))
                 (swap! v# (fn [_#] (parse-null-int x#))))
               (deref v#))))))))

(defprotocol Parser
  (d [this token])
  (parse-null-int [this]))

;; Since we use Delays, equality comparison becomes troublesome.
;; eq gives us a chance to force any delays, permitting us to
;; still compare graphs for structural equality.
(defprotocol ComparableParser
  (eq [this that]))

(defn-fix parse-null {} (fn [parser] parse-null-int parser))

;; We forward declare the helper constructors because we use them
;; in the deftypes.
(declare empty-p)
(declare eps)
(declare eps*)
(declare lit)
(declare alt)
(declare cat)

(defrecord empty-parser []
  ComparableParser
  (eq [this that]
    (= this that))
  Parser
  (d [this _] this)
  (parse-null-int [_] #{}))

(defrecord empty-string-parser [treeSet]
  ComparableParser
  (eq [this that]
    (= this that))
  Parser
  (d [this _] (empty-p))
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
  (parse-null-int [_] #{}))

(defrecord sequence-parser [first second]
  ComparableParser
  (eq [this that]
    (and (eq (force (:first this)) (force (:first that)))
         (eq (force (:second this)) (force (:second that)))))
  Parser
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
  (parse-null-int [p]
    (set/union
     (parse-null-int (force (:left p)))
     (parse-null-int (force (:right p))))))

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
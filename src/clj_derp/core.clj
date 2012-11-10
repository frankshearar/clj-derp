(ns clj-derp.core
  (:require [clojure.set :as set]))

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
  (parse-null-int [this]))

(defn-fix parse-null {} (fn [parser] parse-null-int parser))

(defrecord empty-parser []
  Parser
  (parse-null-int [_] #{}))
(defrecord empty-string-parser [treeSet]
  Parser
  (parse-null-int [_] treeSet))
(defrecord literal-parser [token]
  Parser
  (parse-null-int [_] #{}))
(defrecord union-parser [left right]
  Parser
  (parse-null-int [p]
    (set/union
     (parse-null-int (force (:left p)))
     (parse-null-int (force (:right p))))))

;; Utility constructors
(defn empty-p [] (empty-parser.))
(defn eps [] (empty-string-parser. #{nil}))
(defn eps* [token] (empty-string-parser. #{token}))
(defn lit [token] (literal-parser. token))
(defn alt
  ([] (empty-p))
  ([a & parsers]
     (union-parser. (delay a) (delay (apply alt parsers)))))
(ns clj-derp.core)

(defprotocol Parser
  (parse-null [this]))

(defrecord empty-parser []
  Parser
  (parse-null [_] #{}))
(defrecord empty-string-parser [treeSet]
  Parser
  (parse-null [_] treeSet))
(defrecord literal-parser [token]
  Parser
  (parse-null [_] #{}))

; Utility constructors
(defn empty-p [] (empty-parser.))
(defn eps [] (empty-string-parser. #{nil}))
(defn eps* [token] (empty-string-parser. #{token}))
(defn literal [token] (literal-parser. token))
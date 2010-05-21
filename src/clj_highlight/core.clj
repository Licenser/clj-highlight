(ns clj-highlight.core
  (:use clj-highlight.mangler)
  (:import java.util.Scanner))


(defn- next-token [s idx token-def defs states]
  (if (empty? defs)
    [[:error (str (first s))  {:state states}] states token-def (get token-def (first states))]
    (let [[matcher & defs] defs] 
      (if-let [result (matcher s idx states token-def)]
	[result matcher]
	(recur s idx token-def defs states)))))

(defn- token-seq* [s idx size token-def defs states] 
  (lazy-seq
   (if (= idx size)
     '()
     (let [[[token states token-def defs] matcher] (next-token s idx token-def defs states)]
       (cons token (token-seq* s (+ idx (count (fnext token))) size token-def defs states))))))

(defn tokenizer 
  "Creates a tokenizer for a given syntax definition."
  [syntax]
  (let [tkn
    	(fn tokenizer*
	  ([string state]
	     (token-seq* string 0 (count string) syntax (get syntax state) (list state)))
	  ([string]
	  (tokenizer* string :initial)))]
    (if-let [keywords (:keywords syntax)]
      (mangle-tokens :identifier (fn [k t s] (if (keywords t) [:keyword t s] [k t s])) tkn)
      tkn)))

(defn highlighter
  "Creates a highlighter consisting of a given syntax, a output generator and a set of manglers.
The manglers are applied to the token stream in the order before it is passed to the output generator."
  [syntax output & manglers]
  (let [tkn (reduce (fn [tkn mgl] (mgl tkn)) (tokenizer syntax) manglers)]
    (fn highlighter* [code]
      (output (tkn code)))))

       
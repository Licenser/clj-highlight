(ns clj-highlight.lazy
  (:use clj-highlight.mangler)
  (:use [clj-highlight.syntax.general :only [next-token]]))

(defn- token-seq* [s idx size syntax states] 
  (lazy-seq
   (if (= idx size)
     '()
     (let [[token states] (next-token s idx syntax states)]
       (cons token (token-seq* s (+ idx (count (fnext token))) size syntax states))))))

(defn tokenizer
  "Creates a tokenizer for a given syntax definition."
  [syntax]
  (let [tkn
    	(fn tokenizer*
	  ([^String string state]
	     (token-seq* string 0 (count string) syntax (list state)))
	  ([^String string]
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

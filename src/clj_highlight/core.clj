(ns clj-highlight.core
  (:use clj-highlight.mangler)
  (:import java.util.Scanner))

(defn- token-seq* [s token-def defs states]
   (lazy-seq 
    (if (empty? defs)
      (conj (token-seq* (subs s 1) token-def (get token-def (first states)) states) [:error (str (first s))  {:state states}])
      (let [[matcher & defs] defs] 
	(if (empty? s)
	  '()
	  (if-let [result (matcher s states token-def)]
	    (let [[token states token-def defs] result]
	      (conj (token-seq* (subs s (count token)) token-def defs states) token))
	    (token-seq* s token-def defs states)))))))

(defn tokenizer [syntax]
  (let [tkn
    	(fn tokenizer*
	  ([string state]
	     (token-seq* string syntax (get syntax state) (list state)))
	  ([string]
	  (tokenizer* string :initial)))]
    (if-let [keywords (:keywords syntax)]
      (mangle-tokens :identifier (fn [k t s] (if (keywords t) [:keyword t s] [k t s])) tkn)
      tkn)))

(defn highlighter [syntax output & manglers]
  (let [tkn (reduce (fn [tkn mgl] (mgl tkn)) (tokenizer syntax) manglers)]
    (fn highlighter* [code]
      (output (tkn code)))))

       
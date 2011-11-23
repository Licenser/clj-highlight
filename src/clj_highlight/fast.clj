(ns clj-highlight.fast
  (:use clj-highlight.mangler)
  (:use [clj-highlight.syntax.general :only [next-token]]))

(defn tokenizer
  "Creates a instant tokenizer for a given syntax definition.
This is not lazy but optimized for performance."
  [syntax]
  (let [tkn
    	(fn tokenizer*
	  ([^String string state]
             (let [size (count string)]
               (loop [tokens (transient []) 
                      idx 0
                      states (list state)]
                 (let [state (first states)
                       [token states :as res] (next-token string idx syntax states)]
                   (if (= idx size)
                     (persistent! tokens)
                     (recur (conj! tokens token) (+ idx (count (second token))) states))))))
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
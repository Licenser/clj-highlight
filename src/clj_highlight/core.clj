(ns clj-highlight.core
  (:import java.util.Scanner))

(defn- token-seq* [s token-def defs]
  (lazy-seq
   (if (empty? defs)
     (throw (Exception. (str "Unknown token: " s)))
     (let [[[regexp kind] & defs] defs] 
       (if (empty? s)
	 '()
	 (if-let [token (re-find regexp s)]
	  (cons [kind token] (token-seq* (subs s (count token)) token-def token-def))
	  (token-seq* s token-def defs)))))))


(defn token-seq [s tokens]
  (token-seq* s tokens tokens))
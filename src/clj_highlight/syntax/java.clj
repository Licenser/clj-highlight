(ns clj-highlight.syntax.java
  (:use clj-highlight.syntax.general))


(defn str-matcher [token]
  (fn [string idx]
    (if (.startsWith string token idx)
      token)))

(defn java-ident-token 
  [default-kind]
  (let [atom 
  (fn java-ident-token* [string idx states token-def]   
    (if-let [token (re-find [a-zA-Z_][A-Za-z_0-9]* (subs string idx))]
      (condp = token
	"import"
	(let [states (conj states :include)]
	  [[:keyword token {:state states}] states token-def ((first states) token-def)])
	"package"
	(let [states (conj states :namespace)]
	  [[:keyword token {:state states}] states token-def ((first states) token-def)])
	"class"
	(let [states (conj states :class)]
	  [[:keyword token {:state states}] states token-def ((first states) token-def)])
	"interface"
	(let [states (conj states :class)]
	  [[:keyword token {:state states}] states token-def ((first states) token-def)])
	token
	[[default-kind token {:state states}] states token-def ((first states) token-def)])))


(def  java-default-tokens
     (list
      (re-token #"\s+|\n" :space)
      (re-token #"//[^\n\\]*(?:\\.[^\n\\]*)*" :comment)
      (re-token #"/\*.*?\*/" :comment)
      (re-token #"\.(?!\d)|[,?:()\[\]{};]|--|\+\+|&&|\|\||\*\*=?|[-+*\/%^~&|<>=!]=?|<<<?=?|>>>?=?" :opperator)
      (token (str-matcher "\"") :string :string1)
      (token (str-matcher "\"") :string :string2)
      ))

(def java-sub-tokens
     (list (token (str-matcher ";") :opperator :initial)
	   (token (str-matcher "{") :opperator :initial)
	   (java-ident-token :ident)))

(def java-syntax
     {:keywords java-keywords
      :initial (conj java-default-tokens (java-ident-token :ident))
      :include 
      (concat (list (re-token #"[a-zA-Z_][A-Za-z_0-9]*(?:\.[a-zA-Z_][A-Za-z_0-9]*)*(?:\*)?" :include :initial))
	      java-sub-tokens
	      java-default-tokens)
      :namespace 
      (concat (list (re-token #"[a-zA-Z_][A-Za-z_0-9]*(?:\.[a-zA-Z_][A-Za-z_0-9]*)*(?:\*)?" :namespace :initial))
	      java-sub-tokens
	      java-default-tokens)
      :class 
      (concat (list (java-ident-token :class :initial))
	      java-sub-tokens
	      default-java-tokens)
     :string1 (list
	       
	       )})
(ns clj-highlight.syntax.java
  (:use clj-highlight.syntax.general))

(def java-keywords #{})

(defn str-matcher [token]
  (fn [string idx]
    (if (.startsWith string token idx)
      token)))

(defn java-ident-token 
  [default-kind]
  (fn java-ident-token* [string idx states token-def]   
    (if-let [token (re-find #"^[a-zA-Z_][a-zA-Z_0-9]*" (subs string idx))]
      (let [token (str token)]
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
	  [[default-kind token {:state states}] states token-def ((first states) token-def)])))))


(defn java-number-token [string idx states token-def]
     (let [s (subs string idx)]
       (if (re-find #"^[\d.]" s)
	 (if-let [token (re-find #"^\d+[fFdD]|\d*\.\d+(?:[eE][+-]?\d+)?[fFdD]?|\d+[eE][+-]?\d+[fFdD]?" s)]
	   [[:float token {:state states}] states token-def ((first states) token-def)]
	   (if-let [token (re-find #"^0[xX][0-9A-Fa-f]+" s)]
	     [[:hex token {:state states}] states token-def ((first states) token-def)]
	     (if-let [token (re-find #"^(?>0[0-7]+)(?![89.eEfF])" s)]
	       [[:oct token {:state states}] states token-def ((first states) token-def)]
	       (if-let [token (re-find #"^\d+[lL]?" s)]
		 [[:integer token {:state states}] states token-def ((first states) token-def)]
		 nil)))))))

(def  java-default-tokens
     (list
      (re-token #"\s+|\n" :space)
      (re-token #"//[^\n\\]*(?:\\.[^\n\\]*)*" :comment)
      (re-token #"/\*(?s:.*?)\*/" :comment)
      (re-token #"\.(?!\d)|[,?:()\[\]{};]|--|\+\+|&&|\|\||\*\*=?|[-+*\/%^~&|<>=!]=?|<<<?=?|>>>?=?" :opperator)
      java-number-token
      (token (str-matcher "\"") :string :string1)
      (token (str-matcher "'") :string :string2)
      (re-token #"@[a-zA-Z_][A-Za-z_0-9]*" :annotation)
      ))

(def java-sub-tokens
     (list (token (str-matcher ";") :opperator :pop)
	   (token (str-matcher "{") :opperator :pop)
	   (java-ident-token :ident)))

(def java-syntax
     {:keywords java-keywords
      :initial (concat java-default-tokens 
		       (list
			(re-token #"[{;]" :opperator)
			(java-ident-token :ident)))
      :include 
      (concat (list (re-token #"[a-zA-Z_][A-Za-z_0-9]*(?:\.[a-zA-Z_][A-Za-z_0-9]*)*(?:\*)?" :include :pop))
	      java-sub-tokens
	      java-default-tokens)
      :namespace 
      (concat (list (re-token #"[a-zA-Z_][A-Za-z_0-9]*(?:\.[a-zA-Z_][A-Za-z_0-9]*)*(?:\*)?" :namespace :pop))
	      java-sub-tokens
	      java-default-tokens)
      :class 
      (concat (list 
	       (re-token #"[a-zA-Z_][A-Za-z_0-9]*" :class))
	      java-sub-tokens
	      java-default-tokens)
     :string1 (list
	       (re-token #"[^\"\\]+" :string)
	       (token (str-matcher "\"") :string :pop)
	       (re-token #"\\(?:[bfnrtv\n\"'\\]|x[a-fA-F0-9]{1,2}|[0-7]{1,3}|u[a-fA-F0-9]{4}|U[a-fA-F0-9]{8})" :string))
     :string2 (list
	       (re-token #"[^'\\]+" :string)
	       (token (str-matcher "'") :string :pop)
	       (re-token #"\\(?:[bfnrtv\n\"'\\]|x[a-fA-F0-9]{1,2}|[0-7]{1,3}|u[a-fA-F0-9]{4}|U[a-fA-F0-9]{8})" :string))})
(ns clj-highlight.syntax.java
  (:use clj-highlight.syntax.general))

(def java-keywords #{})

(defn str-matcher [token]
  (fn [^String string idx]
    (if (.startsWith string token idx)
      token)))

(defn java-ident-token 
  [default-kind]
  (fn java-ident-token* [string idx states syntax]
    (if-let [token (re-find #"^[a-zA-Z_][a-zA-Z_0-9]*" (subs string idx))]
      (let [token (str token)]
	(condp = token
	  "import"
	  (let [states (conj states :include)]
	    [[:keyword token {:state states}] states])
	  "package"
	  (let [states (conj states :namespace)]
	    [[:keyword token {:state states}] states])
	  "class"
	  (let [states (conj states :class)]
	    [[:keyword token {:state states}] states])
	  "interface"
	  (let [states (conj states :class)]
	    [[:keyword token {:state states}] states])
	  token
	  [[default-kind token {:state states}] states])))))

(defn java-number-token [string idx states syntax]
     (let [s (subs string idx)]
       (if (re-find #"^[\d.]" s)
	 (if-let [token (re-find #"^\d+[fFdD]|\d*\.\d+(?:[eE][+-]?\d+)?[fFdD]?|\d+[eE][+-]?\d+[fFdD]?" s)]
	   [[:float token {:state states}] states]
	   (if-let [token (re-find #"^0[xX][0-9A-Fa-f]+" s)]
	     [[:hex token {:state states}] states]
	     (if-let [token (re-find #"^(?>0[0-7]+)(?![89.eEfF])" s)]
	       [[:oct token {:state states}] states]
	       (if-let [token (re-find #"^\d+[lL]?" s)]
		 [[:integer token {:state states}] states]
		 nil)))))))

(def java-sub-tokens
  (list
   (re-token #"[;{]"  :opperator :pop)
   (java-ident-token :ident)))

(def syntax
  {:keywords java-keywords
   :syntax-name "Java"
   :sub-tokens [(re-token #"[;{]"  :opperator :pop)
                (java-ident-token :ident)]
   :default-tokens  [(re-token #"\s+|\n" :space)
                     (re-token #"\.(?!\d)|[,?:()\[\]{};]|--|\+\+|&&|\|\||\*\*=?|[-+*\/%^~&|<>=!]=?|<<<?=?|>>>?=?" :opperator)
                     java-number-token
                     (token (str-matcher "\"") :string :string1)
                     (token (str-matcher "'") :string :string2)
                     (re-token #"/\*(?s:.*?)\*/|//[^\n\\]*(?:\\.[^\n\\]*)*" :comment)
                     (re-token #"@[a-zA-Z_][A-Za-z_0-9]*" :annotation)]
   :initial [(include :default-tokens)
             (java-ident-token :ident)]
   :include [(re-token #"[a-zA-Z_][A-Za-z_0-9]*+(?:\.[a-zA-Z_][A-Za-z_0-9]*+)*+(?:\.\*)?" :include)
             (include :sub-tokens)
             (include :default-tokens)]
   :namespace [(re-token #"[a-zA-Z_][A-Za-z_0-9]*+(?:\.[a-zA-Z_][A-Za-z_0-9]*+)*+" :namespace)
               (include :sub-tokens)
               (include :default-tokens)]
   :class [(re-token #"[a-zA-Z_][A-Za-z_0-9]*" :class)
           (include :sub-tokens)
           (include :default-tokens)]
   :string1 [(re-token #"[^\"\\]+" :string)
             (token (str-matcher "\"") :string :pop)
             (re-token #"\\(?:[bfnrtv\n\"'\\]|x[a-fA-F0-9]{1,2}|[0-7]{1,3}|u[a-fA-F0-9]{4}|U[a-fA-F0-9]{8})" :string)]
   :string2 [(re-token #"[^'\\]+" :string)
             (token (str-matcher "'") :string :pop)
             (re-token #"\\(?:[bfnrtv\n\"'\\]|x[a-fA-F0-9]{1,2}|[0-7]{1,3}|u[a-fA-F0-9]{4}|U[a-fA-F0-9]{8})" :string)]})
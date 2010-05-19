(ns clj-highlight.core
  (:import java.util.Scanner))



(defn- token-seq* [s token-def defs state states]
   (lazy-seq (if (empty? defs)
    (throw (Exception. (str "Unknown token: " s)))
    (let [[[matcher kind & [new-state]] & defs] defs] 
      (if (empty? s)
	'()
	(if-let [token (matcher s states)]
	  (cond
;	   (prn token kind) nil
	   (nil? new-state) 
	    (conj (token-seq* (subs s (count token)) token-def (get token-def state) state states) [kind token])
	    (= new-state :pop)
	    (let [states (pop states)
		  state (first states)]
	      (conj (token-seq* (subs s (count token)) token-def (get token-def state) states states) [kind token]))
	    :else
	    (conj (token-seq* (subs s (count token)) token-def (get token-def new-state) new-state (conj states new-state)) [kind token]))
	  (token-seq* s token-def defs state states)))))))


(defn re-token [re]
  (fn [s _] (re-find (re-pattern (str "^(?:" re ")")) s)))
  

(def didgets "0123456789abcdefghijklmnopqrstuvwxyz")

(let [basic-identifier* "(?i)[a-z$%$*_+!?&<>=-][a-z0-9§$&*=+!_?<>-]*(?-i)"
      identifier* (str "(?:[@']?(?:"basic-identifier*"\\.)*"basic-identifier*"(?:/"basic-identifier*")?\\.?)|\\.\\.?")
      symbol* (str "::?"identifier*)
      number-token (fn [s _]
		       (if-let [p (re-find #"^(\d+)r" s)]
			 (let [[prefix precision] p
			       precision (Integer/parseInt precision)
			       number-re (re-pattern (str "^" prefix "[" (subs didgets 0 precision) "]+"))]
			   (if-let [num (re-find number-re)]
			     num
			     nil))
			 (if-let [num (re-find #"^\d+(?:(?:/\d+)|(?:(?:.\d*)?(?:e[+-]\d+)?))" s)]
			   num
			   nil)))]
  (def clj-syntax
       {:keywords #{"do"}
	:initial
	[
	 [(re-token identifier*) :identifyer]
	 [(re-token symbol*) :symbol]
	 [(re-token #"\"(?:\\.|[^\"])*\"") :string]
	 [(re-token #"[\s,\n]+") :space]
	 [(re-token #"[\(\)\[\]{}]") :pren]
	 [(re-token #"[\(\)\[\]{}]") :pren]
	 [(re-token #";.*\n?") :comment]
	 [(re-token #"\\.") :string]
	 [(re-token #"[#.'`]") :operator]
	 [number-token :number]
	 ]}))


(defn mangle-tokens [kind mangle-fn tokenizer]
  (fn mangle-tokens*
    ([string state]
       (map (fn [[k t]]
	 (if (= k kind)
	   (mangle-fn k t)
	   [k t]))
	    (tokenizer string state)))
    ([string]
       (mangle-tokens* string :initial))))


(defn tokenizer [syntax]
  (let [tkn
    	(fn tokenizer*
	  ([string state]
	     (token-seq* string syntax (get syntax state) state (list state)))
	  ([string]
	  (tokenizer* string :initial)))]
    (if-let [keywords (:keywords syntax)]
      (mangle-tokens :identifyer (fn [k t] (if (keywords t) [:keyword t] [k t])) tkn)
      tkn)))
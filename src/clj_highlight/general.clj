(ns clj-highlight.general
  (:use clj-highlight.mangler)
  (:import java.util.Scanner))


(defn next-token [^String s idx token-def defs states]
  (if (empty? defs)
    [[:error (str (first (subs s idx))) {:state states :index idx}] states token-def (get token-def (first states))]
    (let [[matcher & defs] defs] 
      (if-let [result (matcher s idx states token-def)]
	result
	(recur s idx token-def defs states)))))
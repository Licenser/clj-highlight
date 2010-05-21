(var-set *warn-on-reflection* true)
(ns test
  (:use clj-highlight.core clj-highlight.syntax.clojure)
  (:gen-class))


(defn -main []
  (let [tkn (highlighter clj-syntax identity)
	code (slurp "src/clj_highlight/syntax/clojure.clj")]
    (println "Waiting...")
    (read-line)
    (println "Tokenizng...")
    (time (count (tkn code)))
    (time (count (tkn code)))
    (println "Pausing...")
    (read-line)
    (println "Tokenizng...")
    (time (count (tkn code)))
    (time (count (tkn code)))
    (println "Done")
    (read-line)))
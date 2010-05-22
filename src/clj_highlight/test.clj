(ns clj-highlight.test
  (:use clj-highlight.core clj-highlight.syntax.clojure)
  (:gen-class))

(defn -main []
  (let [tkn (highlighter clj-syntax identity)
	code (slurp "src/clj_highlight/syntax/clojure.clj")]
    (read-line)
    (time (dotimes [i 50] (count (tkn code))))))

;(comment 
;  (def code (slurp "src/clj_highlight/syntax/clojure.clj"))
;  (use 'clj-highlight.core 'clj-highlight.syntax.clojure :reload-all)
;  (def tkn (highlighter clj-syntax identity))
;  clj_highlight.**
;  , sun.rmi.transport.*, clojure.*, swank.*, sun.rmi.*
;  (time (dotimes [i 10] (count (tkn code))))
;)
  
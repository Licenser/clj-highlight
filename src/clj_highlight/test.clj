(ns clj-highlight.test
  (:use clj-highlight.core 
	[clj-highlight.syntax java clojure])
  (:gen-class))


(defn time-method [fun]
  (let [_ (dotimes [n 1] (fun) (print ".") (flush))
	a (. System currentTimeMillis)
	r (fun)
	b (. System currentTimeMillis)]
    [r (int (/ (- b a) 2))]))

(defn test-scanner [syntax in-file]
     (let [code (slurp in-file)
	   tkn (highlighter syntax identity)
	   _(print "Timing scanner")
	   [[c e] t] (time-method (fn [] [(count (tkn code)) (count (filter #(= :error (first %)) (tkn code)))]))
	   _ (println "done.")]
       (println "Parsed" in-file "with" c "tokens " (str "(" e " errors)") "in" (str t "ms (" (int (/ c t)) " kTok/s)." ))))

(defn -main []
  (test-scanner java-syntax "benchmarks/jruby.in.java")
  (test-scanner clj-syntax "benchmarks/core.clj"))



;(comment 
;  (def code (slurp "src/clj_highlight/syntax/clojure.clj"))
;  (use 'clj-highlight.core 'clj-highlight.syntax.clojure :reload-all)
;  (def tkn (highlighter clj-syntax identity))(
;  clj_highlight.**
;  , sun.rmi.transport.*, clojure.*, swank.*, sun.rmi.*
;  (time (dotimes [i 10] (count (tkn code))))
;)
  
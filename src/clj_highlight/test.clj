(ns clj-highlight.test
  (:require
   [clj-highlight.syntax.java :as java]
   [clj-highlight.syntax.clojure :as clj]
   [clj-highlight.fast :as fast]
   [clj-highlight.lazy :as lazy])
  (:gen-class))


(defn time-method [fun cnt]
  (let [a (. System currentTimeMillis)
	_ (dotimes [_ (dec cnt)]
            (print ".")
            (flush)
            (fun))
        r (fun)
	b (. System currentTimeMillis)]
    [r (int (/ (- b a) cnt))]))

(defn evaluate [in-file [tkns t]]
  (let [c (count tkns)
        e (count (filter #(= :error (first %)) tkns))]
    (println "\n  Parsed" in-file "with" c "tokens " (str "(" e " errors)") "in" (str t "ms (" (int (/ c t)) " kTok/s)." ))))

(defn test-scanner-fast [syntax in-file n]
     (let [code (slurp in-file)]
       (print "Timing" (:syntax-name syntax) "scanner in fast mode")
       (evaluate in-file (time-method (fn [] ((fast/highlighter syntax identity) code)) n))))

(defn test-scanner-lazy [syntax in-file n]
     (let [code (slurp in-file)]
       (print "Timing" (:syntax-name syntax) "scanner in lazy mode")
       (evaluate in-file (time-method (fn [] (doall ((lazy/highlighter syntax identity) code))) n))))

(defn -main []
  (test-scanner-lazy java/syntax "benchmarks/jruby.in.java" 1)
  (test-scanner-lazy clj/syntax "benchmarks/core.clj" 20)
  (test-scanner-fast clj/syntax "benchmarks/core.clj" 20))



;(comment 
;  (def code (slurp "src/clj_highlight/syntax/clojure.clj"))
;  (use 'clj-highlight.core 'clj-highlight.syntax.clojure :reload-all)
;  (def tkn (highlighter clj-syntax identity))(
;  clj_highlight.**
;  , sun.rmi.transport.*, clojure.*, swank.*, sun.rmi.*
;  (time (dotimes [i 10] (count (tkn code))))
;)
  
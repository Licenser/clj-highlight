(ns clj-highlight.benchmark
  (:require
   [clj-highlight.syntax.java :as java]
   [clj-highlight.syntax.clojure :as clj]
   [clj-highlight.fast :as fast]
   [clj-highlight.lazy :as lazy])
  (:use clj-highlight.syntax.general)
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
    (println "\n  Parsed" in-file "with" c "tokens" (str "(" e " errors)") "in" (str t "ms (" (int (/ c t)) " kTok/s)." ))))

(defn benchmark-scanner-fast [syntax in-file n]
     (let [code (slurp in-file)]
       (print "Timing" (:syntax-name syntax) "scanner in fast mode")
       (evaluate in-file (time-method (fn [] ((fast/tokenizer syntax) code)) n))))

(defn benchmark-scanner-lazy [syntax in-file n]
     (let [code (slurp in-file)]
       (print "Timing" (:syntax-name syntax) "scanner in lazy mode")
       (evaluate in-file (time-method (fn [] (doall ((lazy/tokenizer syntax) code))) n))))

(defn -main []
  (benchmark-scanner-lazy java/syntax "benchmarks/jruby.in.java" 1)
  (benchmark-scanner-fast java/syntax "benchmarks/jruby.in.java" 1)
  (benchmark-scanner-lazy clj/syntax "benchmarks/core.clj" 1)
  (benchmark-scanner-fast clj/syntax "benchmarks/core.clj" 1))


(def test-syntax
  {:initial [(re-token #"a" :a :b)
             (re-token #"d" :d :c)
             (include :c)]
   :b [(re-token #"b" :b :pop)]
   :c [(re-token #"c" :c)]})

;(comment 
;  (def code (slurp "src/clj_highlight/syntax/clojure.clj"))
;  (use 'clj-highlight.core 'clj-highlight.syntax.clojure :reload-all)
;  (def tkn (highlighter clj-syntax identity))(
;  clj_highlight.**
;  , sun.rmi.transport.*, clojure.*, swank.*, sun.rmi.*
;  (time (dotimes [i 10] (count (tkn code))))
;)
  
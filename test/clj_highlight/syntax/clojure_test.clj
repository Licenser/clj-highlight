(ns clj-highlight.syntax.clojure-test
  (:use [clj-highlight.syntax.clojure] :reload-all)
  (:use [clj-highlight.core :only [tokenizer]] :reload-all)
  (:use [clojure.test]))

(deftest spaces
  (is (= ((tokenizer clj-syntax) " \t\n," :initial)
         '([:space " \t\n," {:state (:initial), :index 0}]))))



(deftest parens
  (is (= ((tokenizer clj-syntax) "()" :initial)
         '([:paren "(" {:state (:initial), :index 0}] [:paren ")" {:state (:initial), :index 1}])))
  (is (= ((tokenizer clj-syntax) "[]" :initial)
         '([:paren "[" {:state (:initial), :index 0}] [:paren "]" {:state (:initial), :index 1}])))
  (is (= ((tokenizer clj-syntax) "{}" :initial)
         '([:paren "{" {:state (:initial), :index 0}] [:paren "}" {:state (:initial), :index 1}]))))


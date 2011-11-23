(ns clj-highlight.output.html
  (:use clj-highlight.mangler clj-highlight.output.hiccup))


(defn span [c content]
  (str "<span class='" c "'>" (escape-html content) "</span>"))

(defn html-to-stream [stream root-class style-map tokenstream]
  (binding [*out* stream]
    (print (str "<span class='" root-class "'>"))
    (loop [[kind token & _] (first tokenstream) tkns (next tokenstream)]
      (let [style (get style-map kind (name kind))]
	(print (span style token))
	(if-let [[f & n] (next tokenstream)]
	  (recur f n))))
    (print "</span>")))
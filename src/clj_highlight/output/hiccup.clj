(ns clj-highlight.output.hiccup
  (:use clj-highlight.mangler))

; Copied from hiccup.
(defn- escape-html
  "Change special characters into HTML character entities."
  [text]
  (.. #^String (as-str text)
    (replace "&"  "&amp;")
    (replace "<"  "&lt;")
    (replace ">"  "&gt;")
    (replace "\"" "&quot;")))

(def default-stype-map
     {:identifier nil
      :keyword     "r"
      :symbol     "sy"
      :string     "s"
      :paren      "of"
      :comment    "c"
      :operator   "cl"
      :number     "i"
      })

(defn- hiccupify-tokens [style-map tokens last last-cl]
  (lazy-seq 
   (if (empty? tokens)
     (list (if last-cl [:span {:class last-cl} last] last))
     (let [[k t _] (first tokens)
	   cl (style-map k)]
       (cond
	(nil? last)
	(hiccupify-tokens style-map (rest tokens) t cl)
	(= last-cl cl)
	(hiccupify-tokens style-map (rest tokens) (str last t) cl)
	:else
	(conj (hiccupify-tokens style-map (rest tokens) t cl) (if last-cl [:span {:class last-cl} last] last)))))))

  
(defn to-hiccup [style-map root-class tokens]
  (vec (concat [:span {:class root-class}] (hiccupify-tokens style-map tokens nil nil))))


(def newline-to-br-mangler 
     (new-mangler
      :space
      (fn [k t s]
	[k (.replace t "\n" "<br/>") s])))

(def html-escape-mangler 
     (new-mangler
      :space
      (fn [k t s]
	[k (escape-html) s])))
(ns clj-highlight.mangler)

(defn mangle-tokens [kind mangle-fn tokenizer]
  (fn mangle-tokens*
    ([string state]
       (map (fn [[k t s]]
              (if (or (nil? kind) (= k kind))
                (mangle-fn k t s)
                [k t s]))
	    (tokenizer string state)))
    ([string]
       (mangle-tokens* string :initial))))

(defn new-mangler [kind mangle-fn]
  (fn [tokenizer]
    (mangle-tokens kind mangle-fn tokenizer)))

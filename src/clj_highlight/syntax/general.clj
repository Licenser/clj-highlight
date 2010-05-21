(ns clj-highlight.syntax.general)

(def didgets "0123456789abcdefghijklmnopqrstuvwxyz")

(defn token [matcher kind & [new-state info-fn]]
  (cond
   (nil? new-state)
   (fn [string idx states token-def]
     (if-let [token (matcher string idx)]
       [[kind token (if info-fn (info-fn kind token states) {:state states})] states token-def ((first states) token-def)]))
   (= new-state :pop)
   (fn [string idx states token-def]
     (if-let [token (matcher string idx)]
       (let [states (next states)]
	 [[kind token (if info-fn (info-fn kind token states) {:state states})] states token-def ((first states) token-def)])))
   :else
   (fn [string idx states token-def]
     (if-let [token (matcher string idx)]
       (let [states (conj new-state states)]
	 [[kind token (if info-fn (info-fn kind token states) {:state states})] states token-def ((first states) token-def)])))))


(defn re-token [re kind & [new-state]]
  (let [pattern (re-pattern (str "^(?:" re ")"))]
    (token 
     (fn [string idx]
       (re-find pattern (subs string idx)))
     kind
     new-state)))
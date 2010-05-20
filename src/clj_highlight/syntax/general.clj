(ns clj-highlight.syntax.general)

(def didgets "0123456789abcdefghijklmnopqrstuvwxyz")

(defn token [matcher kind & [new-state info-fn]]
  (cond
   (nil? new-state)
   (fn [string states token-def]
     (if-let [token (matcher string)]
       [[kind token (if info-fn (info-fn kind token states) {:state states})] states token-def ((first states) token-def)]))
   (= new-state :pop)
   (fn [string states token-def]
     (if-let [token (matcher string)]
       (let [states (next states)]
	 [[kind token (if info-fn (info-fn kind token states) {:state states})] states token-def ((first states) token-def)])))
   :else
   (fn [string states token-def]
     (if-let [token (matcher string)]
       (let [states (conj new-state states)]
	 [[kind token (if info-fn (info-fn kind token states) {:state states})] states token-def ((first states) token-def)])))))

(defn re-token [re kind & [new-state]]
  (token 
   (partial re-find (re-pattern (str "^(?:" re ")")))
   kind
   new-state))
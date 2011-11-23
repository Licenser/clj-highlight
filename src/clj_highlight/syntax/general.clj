(ns clj-highlight.syntax.general)

(defn- next-token* [^String s idx syntax states state-syntax]
  (if (empty? state-syntax)
    [[:error (str (first (subs s idx))) {:state states :index idx}] states]
    (if-let [result ((first state-syntax) s idx states syntax)]
      result
      (recur s idx syntax states (rest state-syntax)))))

(defn next-token [^String s idx syntax states]
  (next-token* s idx syntax states (get syntax (first states))))


(def didgets "0123456789abcdefghijklmnopqrstuvwxyz")

(def ^:dynamic *do-profile* true)

(def ^:dynamic *profile* (agent {}))

(defn prof [a idx time]
  (update-in a [idx] conj time))

(defn report-profileing [profiling]
  (println (apply format "%30s %6s %6s %6s %6s" ["regexp" "count" "total" "min" "max"]))
  (doseq [m (sort-by second (map (fn [[idx times]] 
                                   (let [cnt (count times)]
                                     [(str  (apply str (take 25 (str idx)))
                                            (if (> (count (str idx)) 25) "..."))
                                      cnt (reduce + times) (reduce min times) (reduce max times)])) profiling))]
    (println (apply format "%30s %6d %6s %6s %6s" m))))

(defmacro benchmark 
  ([idx form]
     `(let [t0# (. System currentTimeMillis)
            r# ~form
            t1# (. System currentTimeMillis)]
        (send *profile* prof ~idx (- t1# t0#))
        r#))
  ([form] 
     `(let [t0# (. System currentTimeMillis)
            r# ~form
            t1# (. System currentTimeMillis)]
        (send *profile* prof (first '~form) (- t1# t0#))
        r#)))

(defmacro profiled [form]
  `(binding [*do-profile* true]
     ~form))

(defn token [matcher kind & [new-state info-fn]]
  (cond
   (nil? new-state)
   (if info-fn
     (fn no-state-change-token-fn1 [string idx states  syntax]
       (if-let [token (matcher string idx)]
         [[kind token (info-fn kind token states)] states]))
     (fn no-state-change-token-fn2 [string idx states  syntax]
       (if-let [token (matcher string idx)]
         [[kind token {:state states :index idx}] states])))
   (= new-state :pop)
   (if info-fn
     (fn pop-token-fn1 [string idx states  syntax]
       (if-let [token (matcher string idx)]
         (let [states (next states)]
           [[kind token (info-fn kind token states)] states])))
     (fn pop-token-fn2 [string idx states  syntax]
       (if-let [token (matcher string idx)]
         (let [states (next states)]
           [[kind token {:state states :index idx}] states]))))
   :else
   (if info-fn
     (fn state-change-token-fn1 [string idx states syntax]
       (if-let [token (matcher string idx)]
         (let [states (conj states new-state)]
           [[kind token (info-fn kind token states)] states])))
     (fn state-change-token-fn2 [string idx states syntax]
       (if-let [token (matcher string idx)]
         (let [states (conj states new-state)]
           [[kind token {:state states :index idx}] states]))))))

(defn include [state]
  (fn include* [string idx states syntax]
    (next-token* string idx syntax states (get syntax state))))


(defn re-token [re kind & [new-state]]
  (let [pattern (re-pattern (str "^(?:" re ")"))]
    (token
     (fn [string idx]
       (re-find pattern (subs string idx)))
     kind
     new-state)))
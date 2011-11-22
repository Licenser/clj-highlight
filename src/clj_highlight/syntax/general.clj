(ns clj-highlight.syntax.general)

(def didgets "0123456789abcdefghijklmnopqrstuvwxyz")

(def ^:dynamic *do-profile* false)

(def ^:dynamic *profile* (agent {}))

(defn- prof [a idx time]
  (update-in a [idx] conj time))

(defn report-profileing [profiling]
  (sort-by second (map (fn [[idx times]] 
	 (let [cnt (count times)]
	   [idx cnt (reduce + times) (apply min times) (apply max times)])) profiling)))

(defmacro benchmark 
  ([idx form]
     (if *do-profile*
       `(let [t0# (. System currentTimeMillis)
	      r# ~form
	      t1# (. System currentTimeMillis)]
	  (send *profile* prof ~idx (- t1# t0#))
	  r#)
       `~form))
  ([form] 
     (if *do-profile*
       `(let [t0# (. System currentTimeMillis)
	      r# ~form
	      t1# (. System currentTimeMillis)]
	  (send *profile* prof (first '~form) (- t1# t0#))
	  r#)
       `~form)))

(defmacro profiled [form]
  `(binding [*do-profile* true]
     ~form))
	   

(defn token [matcher kind & [new-state info-fn]]
  (cond
   (nil? new-state)
   (if info-fn
     (fn [string idx states token-def]
       (if-let [token (matcher string idx)]
         [[kind token #_(info-fn kind token states)] states token-def ((first states) token-def)]))
     (fn [string idx states token-def]
       (if-let [token (matcher string idx)]
         [[kind token #_{:state states :index idx}] states token-def ((first states) token-def)])))
   (= new-state :pop)
   (if info-fn
     (fn [string idx states token-def]
       (if-let [token (matcher string idx)]
         (let [sts (next states)]
           [[kind token #_(info-fn kind token states)] sts token-def ((first sts) token-def)])))
     (fn [string idx states token-def]
       (if-let [token (matcher string idx)]
         (let [sts (next states)]
           [[kind token #_{:state states :index idx}] sts token-def ((first sts) token-def)]))))
   :else
   (if info-fn
     (fn [string idx states token-def]
     (if-let [token (matcher string idx)]
       (let [sts (conj states new-state)]
	 [[kind token #_(info-fn kind token states)] sts token-def ((first sts) token-def)])))
     (fn [string idx states token-def]
     (if-let [token (matcher string idx)]
       (let [sts (conj states new-state)]
	 [[kind token #_{:state states :index idx}] sts token-def ((first sts) token-def)]))))))


(defmacro re-token [re kind & [new-state]]
  `(let [pattern# (re-pattern (str "^(?:" ~re ")"))]
     (token 
      (fn ~(symbol (str (name kind) "-token")) [string# idx#]
        (re-find pattern# (subs string# idx#)))
      ~kind
      ~(if new-state new-state))))
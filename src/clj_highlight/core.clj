(ns clj-highlight.core
;  (:use hiccup.core)
  (:import java.util.Scanner))



(defn- token-seq* [s token-def defs state states]
   (lazy-seq 
    (if (empty? defs)
      (conj (token-seq* (subs s 1) token-def (get token-def state) state states) [:error (str (first s))  states])
      (let [[[matcher kind & [new-state]] & defs] defs] 
	(if (empty? s)
	  '()
	  (if-let [token (matcher s states)]
					;	   (prn token kind) nil
	    (cond
	     (nil? new-state) 
	     (conj (token-seq* (subs s (count token)) token-def (get token-def state) state states) [kind token states])
	     (= new-state :pop)
	     (let [states (pop states)
		   state (first states)]
	       (conj (token-seq* (subs s (count token)) token-def (get token-def state) states states) [kind token states]))
	     :else
	     (conj (token-seq* (subs s (count token)) token-def (get token-def new-state) new-state (conj states new-state)) [kind token states]))
	    (token-seq* s token-def defs state states)))))))


(defn re-token [re]
  (fn [s _] (re-find (re-pattern (str "^(?:" re ")")) s)))
  

(def didgets "0123456789abcdefghijklmnopqrstuvwxyz")

(let [basic-identifier* "(?i)[a-z$%$*_+!?&<>=-][a-z0-9§$&*=+!_?<>-]*(?-i)"
      identifier* (str "(?:[@']?(?:"basic-identifier*"\\.)*"basic-identifier*"(?:/"basic-identifier*")?\\.?)|\\.\\.?")
      symbol* (str "::?"identifier*)
      number-token (fn [s _]
		       (if-let [p (re-find #"^(\d+)r" s)]
			 (let [[prefix precision] p
			       precision (Integer/parseInt precision)
			       number-re (re-pattern (str "^" prefix "[" (subs didgets 0 precision) "]+"))]
			   (if-let [num (re-find number-re)]
			     num
			     nil))
			 (if-let [num (re-find #"^\d+(?:(?:/\d+)|(?:(?:\.\d*)?(?:e[+-]\d+)?))" s)]
			   num
			   nil)))]
  (def clj-syntax
       {:keywords #{"def", "if", "do", "let", "quote", "var", "fn", "loop", "recur", "throw", "try", "catch", "monitor-enter", "monitor-exit", ".", "new", "nil"
		    "+", "-", "->", "->>", "..", "/", "<", "<=", "=", "==", ">", ">=", "accessor", "aclone", "add-classpath", "add-watch", "agent", "agent-error", "agent-errors", "aget", "alength", "alias", "all-ns", "alter", "alter-meta!", "alter-var-root", "amap", "ancestors", "and", "apply", "areduce", "array-map", "aset", "aset-boolean", "aset-byte", "aset-char", "aset-double", "aset-float", "aset-int", "aset-long", "aset-short", "assert", "assoc", "assoc!", "assoc-in", "associative?", "atom", "await", "await-for", "bases", "bean", "bigdec", "bigint", "binding", "bit-and", "bit-and-not", "bit-clear", "bit-flip", "bit-not", "bit-or", "bit-set", "bit-shift-left", "bit-shift-right", "bit-test", "bit-xor", "boolean", "boolean-array", "booleans", "bound-fn", "bound-fn*", "bound?", "butlast", "byte", "byte-array", "bytes", "case", "cast", "char", "char-array", "char-escape-string", "char-name-string", "char?", "chars", "class", "class?", "clear-agent-errors", "clojure-version", "coll?", "comment", "commute", "comp", "comparator", "compare", "compare-and-set!", "compile", "complement", "concat", "cond", "condp", "conj", "conj!", "cons", "constantly", "construct-proxy", "contains?", "count", "counted?", "create-ns", "create-struct", "cycle", "dec", "decimal?", "declare", "definline", "defmacro", "defmethod", "defmulti", "defn", "defn-", "defonce", "defprotocol", "defrecord", "defstruct", "deftype", "delay", "delay?", "deliver", "denominator", "deref", "derive", "descendants", "disj", "disj!", "dissoc", "dissoc!", "distinct", "distinct?", "doall", "doc", "dorun", "doseq", "dosync", "dotimes", "doto", "double", "double-array", "doubles", "drop", "drop-last", "drop-while", "empty", "empty?", "ensure", "enumeration-seq", "error-handler", "error-mode", "eval", "even?", "every?", "extend", "extend-protocol", "extend-type", "extenders", "extends?", "false?", "ffirst", "file-seq", "filter", "find", "find-doc", "find-ns", "find-var", "first", "float", "float-array", "float?", "floats", "flush", "fn", "fn?", "fnext", "for", "force", "format", "future", "future-call", "future-cancel", "future-cancelled?", "future-done?", "future?", "gen-class", "gen-interface", "gensym", "get", "get-in", "get-method", "get-proxy-class", "get-thread-bindings", "get-validator", "hash", "hash-map", "hash-set", "identical?", "identity", "if-let", "if-not", "ifn?", "import", "in-ns", "inc", "init-proxy", "instance?", "int", "int-array", "integer?", "interleave", "intern", "interpose", "into", "into-array", "ints", "io!", "isa?", "iterate", "iterator-seq", "juxt", "key", "keys", "keyword", "keyword?", "last", "lazy-cat", "lazy-seq", "let", "letfn", "line-seq", "list", "list*", "list?", "load", "load-file", "load-reader", "load-string", "loaded-libs", "locking", "long", "long-array", "longs", "loop", "macroexpand", "macroexpand-1", "make-array", "make-hierarchy", "map", "map?", "mapcat", "max", "max-key", "memfn", "memoize", "merge", "merge-with", "meta", "methods", "min", "min-key", "mod", "name", "namespace", "neg?", "newline", "next", "nfirst", "nil?", "nnext", "not", "not-any?", "not-empty", "not-every?", "not=", "ns", "ns-aliases", "ns-imports", "ns-interns", "ns-map", "ns-name", "ns-publics", "ns-refers", "ns-resolve", "ns-unalias", "ns-unmap", "nth", "nthnext", "num", "number?", "numerator", "object-array", "odd?", "or", "parents", "partial", "partition", "pcalls", "peek", "persistent!", "pmap", "pop", "pop!", "pop-thread-bindings", "pos?", "pr", "pr-str", "prefer-method", "prefers", "print", "print-namespace-doc", "print-str", "printf", "println", "println-str", "prn", "prn-str", "promise", "proxy", "proxy-mappings", "proxy-super", "push-thread-bindings", "pvalues", "quot", "rand", "rand-int", "range", "ratio?", "rationalize", "re-find", "re-groups", "re-matcher", "re-matches", "re-pattern", "re-seq", "read", "read-line", "read-string", "reduce", "ref", "ref-history-count", "ref-max-history", "ref-min-history", "ref-set", "refer", "refer-clojure", "reify", "release-pending-sends", "rem", "remove", "remove-all-methods", "remove-method", "remove-ns", "remove-watch", "repeat", "repeatedly", "replace", "replicate", "require", "reset!", "reset-meta!", "resolve", "rest", "restart-agent", "resultset-seq", "reverse", "reversible?", "rseq", "rsubseq", "satisfies?", "second", "select-keys", "send", "send-off", "seq", "seq?", "seque", "sequence", "sequential?", "set", "set-error-handler!", "set-error-mode!", "set-validator!", "set?", "short", "short-array", "shorts", "shutdown-agents", "slurp", "some", "sort", "sort-by", "sorted-map", "sorted-map-by", "sorted-set", "sorted-set-by", "sorted?", "special-form-anchor", "special-symbol?", "split-at", "split-with", "str", "string?", "struct", "struct-map", "subs", "subseq", "subvec", "supers", "swap!", "symbol", "symbol?", "sync", "syntax-symbol-anchor", "take", "take-last", "take-nth", "take-while", "test", "the-ns", "thread-bound?", "time", "to-array", "to-array-2d", "trampoline", "transient", "tree-seq", "true?", "type", "unchecked-add", "unchecked-dec", "unchecked-divide", "unchecked-inc", "unchecked-multiply", "unchecked-negate", "unchecked-remainder", "unchecked-subtract", "underive", "update-in", "update-proxy", "use", "val", "vals", "var-get", "var-set", "var?", "vary-meta", "vec", "vector", "vector-of", "vector?", "when", "when-first", "when-let", "when-not", "while", "with-bindings", "with-bindings*", "with-in-str", "with-local-vars", "with-meta", "with-open", "with-out-str", "with-precision", "xml-seq", "zero?", "zipmap"
"true", "false", "*", "*1", "*2", "*3", "*agent*", "*clojure-version*", "*command-line-args*", "*compile-files*", "*compile-path*", "*e", "*err*", "*file*", "*flush-on-newline*", "*in*", "*ns*", "*out*", "*print-dup*", "*print-length*", "*print-level*", "*print-meta*", "*print-readably*", "*read-eval*", "*warn-on-reflection*"}
	:initial
	[
	 [(re-token identifier*) :identifier]
	 [(re-token symbol*) :symbol]
	 [(re-token #"\"(?:\\.|[^\"])*\"") :string]
	 [(re-token #"[\s,\n]+") :space]
	 [(re-token #"[\(\)\[\]{}]") :paren]
	 [(re-token #"[\(\)\[\]{}]") :paren]
	 [(re-token #";.*\n?") :comment]
	 [(re-token #"\\.") :string]
	 [(re-token #"[#.'`]") :operator]
	 [number-token :number]
	 ]}))


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

(defn tokenizer [syntax]
  (let [tkn
    	(fn tokenizer*
	  ([string state]
	     (token-seq* string syntax (get syntax state) state (list state)))
	  ([string]
	  (tokenizer* string :initial)))]
    (if-let [keywords (:keywords syntax)]
      (mangle-tokens :identifier (fn [k t s] (if (keywords t) [:keyword t s] [k t s])) tkn)
      tkn)))

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

(defn- htmlify-tokens [style-map tokens last last-cl]
  (lazy-seq 
   (if (empty? tokens)
     (list (if last-cl [:span {:class last-cl} last] last))
     (let [[k t _] (first tokens)
	   cl (style-map k)]
       (cond
	(nil? last)
	(htmlify-tokens style-map (rest tokens) t cl)
	(= last-cl cl)
	(htmlify-tokens style-map (rest tokens) (str last t) cl)
	:else
	(conj (htmlify-tokens style-map (rest tokens) t cl) (if last-cl [:span {:class last-cl} last] last)))))))

  
(defn to-html [style-map root-class tokens]
  (vec (concat [:span {:class root-class}] (htmlify-tokens style-map tokens nil nil))))

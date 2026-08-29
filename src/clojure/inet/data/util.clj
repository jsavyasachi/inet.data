(ns inet.data.util
  "Utility functions.")

(defmacro ignore-errors
  "Evaluate `body` and return its result, or `nil` if it throws a
  `java.lang.Exception` (errors are not caught). The macro accepts any body
  forms. It is lenient for `Exception` subclasses; any `java.lang.Error` or
  other `Throwable` propagates unchanged."
  [& body] `(try ~@body (catch java.lang.Exception _# nil)))

;; Copy from clojure/core.clj.
(defmacro assert-args
  "Assert each predicate/message pair, where predicates are arbitrary
  expressions and messages are printable values. Evaluation stops at the
  first falsey predicate and strictly throws `java.lang.IllegalArgumentException`;
  exceptions from predicate expressions propagate unchanged. Expanding `p`
  predicate/message pairs takes O(p) time and emits O(p) code."
  [& pairs]
  `(do (when-not ~(first pairs)
         (throw (IllegalArgumentException.
                 (str (first ~'&form) " requires " ~(second pairs) " in "
                      ~'*ns* ":" (:line (meta ~'&form))))))
       ~(let [more (nnext pairs)]
          (when more
            (list* `assert-args more)))))

(defmacro doto-let
  "bindings => binding-form expr

Evaluates expr, and evaluates body with its result bound to the binding-form.
Returns the result of expr. The binding vector must contain exactly one binding
and expression; otherwise macro expansion throws
`java.lang.IllegalArgumentException`. `body` accepts arbitrary forms; exceptions
from `expr` or `body` propagate unchanged. Expansion is O(b) in the number of
body forms and emits O(b) code."
  ([bindings & body]
     (assert-args
       (vector? bindings) "a vector for its binding"
       (= 2 (count bindings)) "exactly 2 forms in binding vector")
     (let [[bf expr] bindings]
       `(let [value# ~expr]
          (let [~bf value#]
            ~@body value#)))))

(defn ffilter
  "Return the first item in `coll` for which `(pred item)` is true. `coll` is a
  seqable value or `nil`, and `pred` implements `clojure.lang.IFn`. This is
  lenient when no item matches and returns `nil`. A non-seqable `coll` throws
  `java.lang.IllegalArgumentException`; a non-function `pred` throws
  `java.lang.ClassCastException` when invoked. Exceptions from `pred` propagate
  unchanged. It takes O(k) time after examining `k` items and O(1) space."
  ([pred coll]
     (when-let [coll (seq coll)]
       (let [item (first coll)]
         (if (pred item) item (recur pred (rest coll)))))))

(defmacro case-expr
  "Like `case`, but only supports individual test expressions, which are
  evaluated at macro-expansion time. `e`, result forms, and the optional final
  default are arbitrary expressions. Exceptions from evaluating a test
  expression propagate unchanged; invalid or duplicate `case` constants throw
  `java.lang.IllegalArgumentException` during macro expansion. Expanding `c`
  clauses takes O(c) time and emits O(c) code."
  [e & clauses]
  `(case ~e
     ~@(concat
        (mapcat (fn [[test result]]
                  [(eval `(let [test# ~test] test#)) result])
                (partition 2 clauses))
        (when (odd? (count clauses))
          (list (last clauses))))))

(defn longest-run
  "Find the longest run of the value x in the collection coll. Returns the
  pair of the starting index and length on success and nil on failure.  When
  multiple runs have the same maximum length, returns the first such run.
  `coll` is any finite seqable value or `nil`. Absence is lenient and returns
  `nil`; a non-seqable `coll` throws `java.lang.IllegalArgumentException`. The
  operation is O(n) time and O(r) space for `n` items and longest-run length
  `r`."
  [x coll]
  (let [runs (->> (partition-by identity coll)
                  (reductions (fn [[_ n pos] s]
                                [(first s) (count s) (+ pos n)])
                              [nil 0 0])
                  (drop 1)
                  (filter #(= x (first %))))]
    (when (seq runs)
      (let [[_ n pos] (reduce (fn [best run]
                                (if (> (second run) (second best))
                                  run
                                  best))
                              runs)]
        [pos n]))))

(defn ubyte
  "Return the unsigned number represented by the low eight bits of `x`. `x`
  accepts any `java.lang.Number` coercible to `long`; fractional values are
  truncated. This is strict about type: `nil` throws
  `java.lang.NullPointerException` and a non-number throws
  `java.lang.ClassCastException`; a numeric value outside the long range throws
  `java.lang.IllegalArgumentException`. This is O(1)."
  {:inline (fn [x] `(bit-and 0xff (long ~x)))}
  (^long [^long x] (bit-and 0xff x)))

(defn sbyte
  "Return the signed byte that numeric value `x` represents, subtracting 256
  once when `x` is greater than 127. `x` accepts any `java.lang.Number`;
  fractional values are truncated by byte conversion. `nil` throws
  `java.lang.NullPointerException`, a non-number throws
  `java.lang.ClassCastException`, and an adjusted value outside the signed-byte
  range throws `java.lang.IllegalArgumentException`. This is O(1)."
  {:inline (fn [x] `(byte (let [x# ~x] (if (> x# 127) (- x# 256) x#))))}
  ([x] (byte (if (> x 127) (- x 256) x))))

(defn bytes-hash-code
  "Calculate a hash code for part of primitive byte array `bytes`. The
  two-argument form takes an initial hash; the three-argument form takes
  `offset` and `length`; the four-argument form takes all three numeric values.
  Numeric arguments accept `java.lang.Number` values coercible to `long`.

  This is strict about types and bounds. A nil array or numeric argument throws
  `java.lang.NullPointerException`; a wrong array or non-number type throws
  `java.lang.ClassCastException`; accessing a range outside the array throws
  `java.lang.ArrayIndexOutOfBoundsException`; overflow in `offset + length`
  throws `java.lang.ArithmeticException`; and a numeric argument outside the
  long range throws `java.lang.IllegalArgumentException`. A non-positive
  `length` reads no bytes and returns the initial hash. The operation is
  O(max(length, 0)) time and O(1) additional space."
  (^long [^bytes bytes]
     (bytes-hash-code bytes 0 (alength bytes) 0))
  (^long [^bytes bytes ^long initial]
     (bytes-hash-code bytes 0 (alength bytes) initial))
  (^long [^bytes bytes ^long offset ^long length]
     (bytes-hash-code bytes offset length 0))
  (^long [^bytes bytes ^long offset ^long length ^long initial]
     (let [terminal (+ offset length)]
       (loop [result (int initial), i offset]
         (if (>= i terminal)
           result
           (let [x (int (aget bytes i))]
             (recur (->> result
                         (unchecked-multiply-int (int 31))
                         (unchecked-add-int x))
                    (inc i))))))))

;; Without this, reader literals for inet.data types do not support
;; code-embedding. The cause is not known yet.
(defmethod clojure.core/print-dup #=(java.lang.Class/forName "[B")
  ([bytes ^java.io.Writer w]
     (.write w "#=(byte-array ")
     (.write w (-> bytes vec str))
     (.write w ")")))

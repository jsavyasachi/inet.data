(ns inet.data.dns
  "Functions for interacting with DNS domain names.

This namespace represents domain names internally in a normalized
byte-oriented form. The normalized form has these properties:

- IDN labels use IDNA encoding.
- Domain labels go from top-level to bottom-level. This is the reverse of the
  usual order.
- A byte comes before each label. The byte gives the count of bytes in the
  label.

This form has these benefits:

- It can represent any binary data accurately, the same as the DNS wire form.
- Lexicographic byte order is also hierarchical order.
- For a given child domain and ancestor domain, you can find the next longer
  child of the ancestor.

The main disadvantage is that this form makes it more difficult to find the
immediate parent of a given domain."
  (:require [clojure.string :as str]
            [inet.data.util :refer [ignore-errors ffilter ubyte sbyte
                                    bytes-hash-code]]
            [hier-set.core :refer [hier-set-by]])
  (:import [clojure.lang IFn ILookup IObj]
           [inet.data.dns DNSDomainParser DNSDomainComparison DNSDomainException]
           [java.io Serializable]
           [java.util Arrays]
           [java.net IDN]))

(defprotocol ^:no-doc DNSDomainConstruction
  "Construct a domain object."
  (^:private -domain [dom]
    "Produce a DNSDomain from `dom`."))

(declare domain)

(defprotocol ^:no-doc DNSDomainOperations
  "Operations on objects which may be treated as domains."
  (^:private -domain? [dom]
    "Return true if the value represents a valid domain.")
  (^bytes domain-bytes [dom]
    "Retrieve the internal normalized byte form of the domain as a byte array.
Only the first `domain-length` bytes will actually contain the domain.")
  (^long domain-length [dom]
    "The length in bytes of this domain."))

(defn domain-byte-seq
  "Return the internal normalized byte form of the domain `dom` as a
sequence of bytes."
  [dom] (take (domain-length dom) (domain-bytes dom)))

(defn domain?
  "Determine if `dom` represents a DNS domain."
  [dom] (and (satisfies? DNSDomainOperations dom)
             (boolean (-domain? dom))))

(declare ->domain)

(defn- domain-compare*
  [stable left right]
  (let [bytes1 (domain-bytes left), len1 (domain-length left)
        bytes2 (domain-bytes right), len2 (domain-length right)]
    (DNSDomainComparison/domainCompare stable bytes1 len1 bytes2 len2)))

(defn domain-compare
  "Compare two domains. The result semantics are the same as `compare`. When
`stable` is true (the default), the result is 0 only when the domains are
value-identical. When `stable` is false, the result is 0 when the domains are
identical up to their minimum common full-label length. Domain comparison
always ignores case."
  (^long [left right] (domain-compare true left right))
  (^long [stable left right]
     (let [left (->domain left)
           right (->domain right)]
       (domain-compare* stable left right))))

(defn domain-contains?
  "Determine if the domain `child` is a subdomain of or identical to the domain
`parent`."
  [parent child]
  (let [parent (->domain parent)
        child (->domain child)]
    (and (<= (domain-length parent) (domain-length child))
       (zero? (domain-compare false parent child)))))

(defn domain-subdomain?
  "Determine if `child` is a subdomain of `parent`."
  [parent child]
  (let [parent (->domain parent)
        child (->domain child)]
    (and (< (domain-length parent) (domain-length child))
       (zero? (domain-compare false parent child)))))

(defn ->domain-set
  "Create a hierarchical set from the domains in `coll`."
  [coll]
  (letfn [(domain-contains-for-set
            [parent child]
            (let [parent (domain parent), child (domain child)]
              (and (<= (domain-length parent) (domain-length child))
                   (zero? (domain-compare* false parent child)))))
          (domain-compare-for-set
            ([left right] (domain-compare-for-set true left right))
            ([stable left right]
             (domain-compare* stable (domain left) (domain right))))]
    (-> (apply hier-set-by domain-contains-for-set domain-compare-for-set
             (map domain coll))
        (vary-meta assoc :type ::domain-set))))

(defn domain-set
  "Create a hierarchical set from domains `doms`."
  [& doms] (->domain-set doms))

(defmethod clojure.core/print-method ::domain-set
  [doms ^java.io.Writer w]
  (.write w "#dns/domain-set #{")
  (loop [first? true, doms (seq doms)]
    (when doms
      (when-not first? (.write w " "))
      (print-method (first doms) w)
      (recur false (next doms))))
  (.write w "}"))

(defn domain-hostname?
  "Determine if the domain `dom` is a valid hostname.  Let hostnames contain
underscores if `underscores` is true (default false)."
  ([dom] (domain-hostname? dom false))
  ([dom underscores]
     (DNSDomainParser/isValidHostname
      (domain-bytes dom) (domain-length dom) underscores)))

(def ^:private ^:const empty-bytes
  "Empty byte array."
  (byte-array []))

(defn ^:private name->bytes
  "Convert a string domain name into an internal normalized byte form.  Returns
an arbitrary invalid result if the name cannot be encoded."
  ^bytes [^String name]
  (if-let [name (ignore-errors (IDN/toASCII name))]
    (->> name (#(str/split % #"\." -1)) reverse
         (mapcat #(let [bytes (.getBytes ^String % "US-ASCII")]
                    (cons (sbyte (count bytes)) bytes)))
         byte-array)
    empty-bytes))

(defn ^:private wire->bytes
  "Convert a DNS wire-form domain name to an internal normalized byte form."
  (^bytes [wire]
     (->> [nil wire]
          (iterate (fn [[state data]]
                     (let [n (inc (first data))]
                       [(conj state (take n data))
                        (drop n data)])))
          (ffilter (comp empty? second)) first
          (drop 1) (apply concat) byte-array))
  (^bytes [wire ^long offset ^long length]
     (->> wire (drop offset) (take length) wire->bytes)))

(defn ^:private bytes->labels
  "Convert the internal normalized byte form of the domain in bytes into a
sequence of label strings."
  [bytes] (->> [nil bytes]
               (iterate (fn [[state data]]
                          (let [n (inc (first data))]
                            [(conj state (drop 1 (take n data)))
                             (drop n data)])))
               (ffilter (comp empty? second)) first
               (#(if (empty? (first %)) (reverse %) %))
               (map #(String. (byte-array %) "US-ASCII"))))

(defn ^:private bytes->name
  "Convert the internal normalized byte form of the domain in bytes into its
standard string form."
  [bytes] (str/join "." (bytes->labels bytes)))

(defn domain-labels
  "Seq of labels in the domain `dom`."
  [dom] (bytes->labels (domain-byte-seq dom)))

(defn idn-str
  "Convert `dom` to IDN string form. Interpret Punycode."
  [dom] (-> dom domain-byte-seq bytes->name IDN/toUnicode))

(deftype DNSDomain [meta, ^bytes bytes, ^long length]
  Serializable

  Object
  (toString [this] (bytes->name (take length bytes)))
  (hashCode [this] (bytes-hash-code bytes 0 length))
  (equals [this other]
    (or (identical? this other)
        (and (instance? DNSDomain other)
             (= length (domain-length other))
             (DNSDomainComparison/domainEquals
              bytes (domain-bytes other) length))))

  IObj
  (meta [this] meta)
  (withMeta [this new-meta] (DNSDomain. new-meta bytes length))

  Comparable
  (compareTo [this other]
    (let [^bytes obytes (domain-bytes other),
          olength (long (domain-length other))]
      (DNSDomainComparison/domainCompare bytes length obytes olength)))

  ILookup
  (valAt [this key]
    (when (domain-contains? this key) key))
  (valAt [this key default]
    (if (domain-contains? this key) key default))

  IFn
  (invoke [this key]
    (when (domain-contains? this key) key))
  (invoke [this key default]
    (if (domain-contains? this key) key default))

  DNSDomainConstruction
  (-domain [this] this)

  DNSDomainOperations
  (-domain? [this] true)
  (domain-bytes [this] bytes)
  (domain-length [this] length))

(ns-unmap *ns* '->DNSDomain)

(def ^:private root-domain
  "The singleton empty root domain."
  (DNSDomain. nil empty-bytes 0))

(defn domain
  "Return the DNS domain for representation `dom`."
  {:tag `DNSDomain}
  [dom] (-domain dom))

(defn ->domain
  "Coerce `dom` to a DNSDomain. Throws when it cannot interpret `dom`."
  {:tag `DNSDomain}
  [dom]
  (try
    (if (nil? dom)
      (throw (DNSDomainException. "Cannot interpret nil as a DNS domain."))
      (or (domain dom)
          (throw (DNSDomainException.
                  (format "Cannot interpret %s as a DNS domain." (pr-str dom))))))
    (catch DNSDomainException e
      (throw e))
    (catch Exception e
      (throw (DNSDomainException.
              (format "Cannot interpret %s as a DNS domain." (pr-str dom)) e)))))

(defn ^:private domain*
  "Private bytes->domain factory."
  [orig ^bytes bytes]
  (when (-domain? bytes)
    (DNSDomain. nil bytes (alength bytes))))

(defn domain-next
  "For the domain `child` which is a subdomain of the domain `parent`, return
the immediate child domain of `parent`. This domain is identical to `child`,
or it is a parent domain of `child`. Returns `nil` if there is no such domain.
Uses the implied empty root domain as `parent` if you do not supply one."
  ([child] (domain-next child root-domain))
  ([child parent]
     (let [child (->domain child)
           parent (->domain parent)
           ^bytes bytes (domain-bytes child), length (domain-length parent)]
       (when (and (domain-contains? parent child)
                  (< length (domain-length child)))
         (DNSDomain. nil bytes (+ length (ubyte (aget bytes length)) 1))))))

(defn domain-ancestors
  "Generate a seq of all the domains for which the domain `child` is a proper
subdomain. The seq starts with the domain after `parent` and ends with the
domain itself. Uses the implied empty root domain as `parent` if you do not
supply one."
  ([child] (domain-ancestors child root-domain))
  ([child parent]
     (let [child (->domain child)
           parent (->domain parent)]
       (->> (iterate #(domain-next child %) parent) (drop 1)
          (take-while identity)))))

(defn domain-parent
  "Return the domain of which `dom` is an immediate subdomain."
  [dom]
  (let [dom (->domain dom)
        bytes (domain-bytes dom), total (domain-length dom)]
    (when (pos? total)
      (let [length (loop [length (long 0)]
                     (let [length' (+ 1 length (long (aget ^bytes bytes length)))]
                       (if (>= length' total) length (recur length'))))]
        (when (pos? length)
          (DNSDomain. nil bytes length))))))

(extend-type (java.lang.Class/forName "[B")
  DNSDomainConstruction
  (-domain [this] (domain* this this))

  DNSDomainOperations
  (-domain? [this] (DNSDomainParser/isValid ^bytes this))
  (domain-bytes [this] this)
  (domain-length [this] (alength ^bytes this)))

(extend-type String
  DNSDomainConstruction
  (-domain [this] (domain* this (name->bytes this)))

  DNSDomainOperations
  (-domain? [this] (-domain? (name->bytes this)))
  (domain-bytes [this] (name->bytes this))
  (domain-length [this] (alength (name->bytes this))))

(extend-type nil
  DNSDomainConstruction
  (-domain [this] nil)

  DNSDomainOperations
  (-domain? [this] true)
  (domain-bytes [this] (domain-bytes root-domain))
  (domain-length [this] 0))

(defmethod clojure.core/print-method DNSDomain
  ([^DNSDomain dom ^java.io.Writer w]
     (.write w "#dns/domain \"")
     (.write w (str dom))
     (.write w "\"")))

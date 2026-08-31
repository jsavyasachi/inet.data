(ns inet.data.ip
  "Functions for interacting with IP addresses and networks."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [inet.data.util :refer [ignore-errors case-expr ubyte sbyte longest-run
                                    bytes-hash-code doto-let]]
            [hier-set.core :refer [hier-set-by]])
  (:import [clojure.lang IFn IObj ILookup BigInt Indexed Seqable]
           [inet.data.ip IPParser IPNetworkComparison IPException IPAddressException IPNetworkException]
           [java.io Serializable]
           [java.util Arrays]
           [java.net InetAddress]))

(set! *warn-on-reflection* true)

(defprotocol ^:no-doc IPAddressConstruction
  "Construct an address object."
  (^:private -address [addr]
    "Produce an IPAddress from `addr`."))

(defprotocol ^:no-doc IPAddressOperations
  "Operations on objects which may be treated as addresses."
  (^:private -address? [addr]
    "Return true if the value represents a valid address.")
  (^bytes address-bytes [addr]
    "Return the byte representation of this address.")
  (^long address-length [addr]
    "The length in bits of this address."))

(defprotocol ^:no-doc IPNetworkConstruction
  "Construct a network object."
  (^:private -network [net] [prefix length]
    "Produce an IPNetwork from `net` or `prefix` & `length`."))

(defprotocol ^:no-doc IPNetworkOperations
  "Operations on objects which may be treated as networks."
  (^:private network?* [net] [addr length]
    "Return true if the value represents a valid network.")
  (network-length [net]
    "The length in bits of the network prefix."))

(defn ^:private string-address-ipv4
  [^bytes bytes]
  (->> bytes (map ubyte) (str/join ".")))

(letfn [(->short [[m x]] (-> m (bit-shift-left 8) (bit-or x)))
        (->str [xs] (->> xs (map #(format "%x" %)) (str/join ":")))]
  (defn ^:private string-address-ipv6
    [^bytes bytes]
    (let [octets (map ubyte bytes)
          shorts (->> octets (partition 2) (map ->short))]
      (if (and (every? zero? (take 10 octets))
               (= [255 255] (vec (take 2 (drop 10 octets)))))
        (str "::ffff:" (string-address-ipv4
                        (byte-array (map sbyte (take-last 4 octets)))))
        (if-let [[nt nd] (when-let [[nt nd] (longest-run 0 shorts)]
                           (when (> nd 1) [nt nd]))]
          (str (->str (take nt shorts)) "::" (->str (drop (+ nt nd) shorts)))
          (->str shorts))))))

(defn ^:private string-address
  [^bytes bytes]
  (case-expr (alength bytes)
    IPParser/IPV4_BYTE_LEN (string-address-ipv4 bytes)
    IPParser/IPV6_BYTE_LEN (string-address-ipv6 bytes)))

(deftype IPAddress [meta, ^bytes bytes]
  Serializable

  Object
  (toString [_]
    (str (string-address bytes)
         (when-let [zone (:zone meta)]
           (str "%" zone))))
  (hashCode [_] (bytes-hash-code bytes))
  (equals [this other]
    (or (identical? this other)
        (and (instance? IPAddress other)
             (Arrays/equals bytes ^bytes (address-bytes other)))))

  IObj
  (meta [_] meta)
  (withMeta [_ new-meta] (IPAddress. new-meta bytes))

  Comparable
  (compareTo [_ other]
    (let [plen1 (long (address-length bytes))
          ^bytes prefix2 (address-bytes other),
          plen2 (long (network-length other))]
      (IPNetworkComparison/networkCompare bytes plen1 prefix2 plen2)))

  IPAddressOperations
  (-address? [_] true)
  (address-bytes [_] bytes)
  (address-length [_] (address-length bytes))

  IPNetworkOperations
  (network?* [_] false)
  (network-length [_] (address-length bytes)))

(ns-unmap *ns* '->IPAddress)

(defn address
  "Return the IP address for representation `addr`.

  This function accepts IPv6 zone IDs. It keeps a zone in metadata for text
  round-tripping only. A zone is not part of address identity, equality,
  hashing, comparison, or network ordering. Serialization does not keep the
  zone. This function rejects zones on IPv4 addresses and on network literals.

  Accepted values are strings, byte arrays, `java.net.InetAddress` values,
  `java.math.BigInteger` values, and existing address or network values. This
  is lenient for those supported representations: malformed input returns
  `nil`. Nonnegative BigInteger values through 2^32-1 are IPv4; larger values
  through 2^128-1 are IPv6, and values outside those ranges throw
  `java.lang.ExceptionInfo` rather than being truncated. An unsupported type
  throws `java.lang.IllegalArgumentException`.
  Parsing is O(1) for the fixed maximum address width."
  {:tag `IPAddress}
  [addr] (-address addr))

(defn ->address
  "Coerce `addr` to an IPAddress. Accepted values are the same strings, byte
  arrays, `java.net.InetAddress`, `java.math.BigInteger`, and existing address
  or network values accepted by `address`. This is strict: malformed or
  unsupported input throws `inet.data.ip.IPAddressException`."
  {:tag `IPAddress}
  [addr]
  (try
    (or (address addr)
        (throw (IPAddressException.
                (format "Cannot interpret %s as an IP address." (pr-str addr)))))
    (catch IPException e
      (throw e))
    (catch Exception e
      (throw (IPAddressException.
              (format "Cannot interpret %s as an IP address." (pr-str addr)) e)))))

;; The BigInteger mapping is internal only.  BigInteger does not keep the input
;; byte-array size.  We add a pseudo-magic prefix at the start to keep the
;; address length.
(defn ^:private address->BigInteger
  "Convert `addr` to an internal-format BigInteger."
  {:tag `BigInteger}
  [addr] (->> addr address-bytes (cons (byte 63)) byte-array BigInteger.))

(defn address-add
  "Return the `n`th address after `addr` in numeric order. `addr` is any value
  accepted by `->address`; `n` may be a number coercible to `long`, a
  `clojure.lang.BigInt`, or a `java.math.BigInteger`. Invalid `addr` throws
  `inet.data.ip.IPAddressException`. Nil `n` throws
  `java.lang.NullPointerException`; a non-number throws
  `java.lang.ClassCastException`. Numeric values other than `BigInt` and
  `BigInteger` are converted to `long`, truncating fractions; an out-of-range
  conversion throws `java.lang.IllegalArgumentException`. This is O(1)."
  {:tag `IPAddress}
  [addr n]
  (let [n (condp instance? n
            BigInteger n
            BigInt     (.toBigInteger ^BigInt n)
            ,,,,,,     (BigInteger/valueOf (long n)))
        addr (->address addr)
        bits (address-length addr)
        lower (.shiftLeft (BigInteger/valueOf 63) bits)
        upper (.subtract (.shiftLeft BigInteger/ONE bits) BigInteger/ONE)
        result (.add (address->BigInteger addr) n)]
    (when (or (neg? (.compareTo result lower))
              (pos? (.compareTo result (.add lower upper))))
      (throw (ex-info (format "Address addition leaves the %d-bit address range." bits)
                      {:address addr :offset n :bits bits})))
    (->address result)))

(defn address-range
  "Return a lazy sequence of addresses from `start` to `stop`, *inclusive*.
  Both endpoints accept the values accepted by `->address`. Invalid input
  throws `inet.data.ip.IPAddressException`. Producing `k` addresses takes
  O(k) time and O(1) additional space per realized step."
  [start stop]
  (let [start (->address start)
        stop (address->BigInteger (->address stop))]
    ((fn step [^BigInteger addr]
       (lazy-seq
        (when-not (pos? (.compareTo addr stop))
          (cons (address addr) (step (.add addr BigInteger/ONE))))))
     (address->BigInteger start))))

(declare ->network)

(defn network-compare
  "Compare the prefixes of networks `left` and `right`. The result semantics
  are the same as `compare`. When `stable` is true (the default), the result is 0
  only when the networks are value-identical. When `stable` is false, the result
  is 0 when the networks are identical up to their minimum common prefix length.
  Arguments accept values accepted by `->network`. This is strict and throws
  `inet.data.ip.IPNetworkException` for invalid input. Comparison is O(1) for
  the fixed maximum IP address width. `stable` must be Boolean; nil throws
  `java.lang.NullPointerException` and a non-Boolean value throws
  `java.lang.ClassCastException`."
  (^long [left right] (network-compare true left right))
  (^long [stable left right]
     (let [left (->network left)
           right (->network right)
           ^bytes prefix1 (address-bytes left), plen1 (network-length left)
           ^bytes prefix2 (address-bytes right), plen2 (network-length right)]
       (IPNetworkComparison/networkCompare stable prefix1 plen1 prefix2 plen2))))

(defn network-contains?
  "Determine if network `net` contains the address or network `addr`. Both
  arguments accept values accepted by `->network`. This is strict and throws
  `inet.data.ip.IPNetworkException` when either value cannot be interpreted.
  Comparison is O(1) for the fixed maximum IP address width."
  [net addr]
  (let [net (->network net)
        addr (->network addr)
        length (network-length net)]
    (and (<= length (network-length addr))
         (zero? (network-compare false net addr)))))

(defn network-count
  "Count of addresses in network `net`. `count` on an IPNetwork throws an
  IPNetworkException when this value exceeds Integer/MAX_VALUE; use this
  function for the exact count. `net` accepts values accepted by `->network`.
  Invalid input throws `inet.data.ip.IPNetworkException`. This is O(1) for the
  fixed maximum IP address width."
  [net]
  (let [net (->network net)
        nbits (- (address-length net) (network-length net))]
    (if (> 63 nbits)
      (bit-shift-left 1 nbits)
      (BigInt/fromBigInteger (.shiftLeft BigInteger/ONE nbits)))))

(defn network-nth
  "Return the `n`th address in the network `net`. Negative `n`s count backward
  from the final address at -1. `net` accepts values accepted by `->network`.
  Invalid networks throw `inet.data.ip.IPNetworkException`; an out-of-range
  index throws `java.lang.IndexOutOfBoundsException`. `n` accepts numeric values;
  nil throws `java.lang.NullPointerException` and a non-number throws
  `java.lang.ClassCastException`. The operation is O(1) for the fixed maximum
  IP address width."
  [net n]
  (let [net (->network net)
        count (network-count net)
        index (if (neg? n) (+ n count) n)]
    (when (or (neg? index) (not (< index count)))
      (throw (IndexOutOfBoundsException.
              (format "Network index %s is out of bounds for %s." n net))))
    (address-add net index)))

(deftype IPNetwork [meta, ^bytes prefix, ^long length]
  Serializable

  Object
  (toString [_] (str (string-address prefix) "/" length))
  (hashCode [_] (bytes-hash-code prefix length))
  (equals [this other]
    (or (identical? this other)
        (and (instance? IPNetwork other)
             (= length (network-length other))
             (Arrays/equals prefix ^bytes (address-bytes other)))))

  IObj
  (meta [_] meta)
  (withMeta [_ new-meta] (IPNetwork. new-meta prefix length))

  Comparable
  (compareTo [_ other]
    (let [^bytes prefix2 (address-bytes other),
          plen2 (long (network-length other))]
      (IPNetworkComparison/networkCompare prefix length prefix2 plen2)))

  ILookup
  (valAt [this key]
    (when (network-contains? this key) key))
  (valAt [this key default]
    (if (network-contains? this key) key default))

  IFn
  (invoke [this key]
    (when (network-contains? this key) key))
  (invoke [this key default]
    (if (network-contains? this key) key default))

  Indexed
  (count [this]
    (let [n (network-count this)]
      (if (> n Integer/MAX_VALUE)
        (throw (IPNetworkException.
                "Network count exceeds Integer/MAX_VALUE; use network-count for the exact count."))
        (int n))))
  (nth [this n] (network-nth this n))

  Seqable
  (seq [this]
    (address-range (nth this 0) (nth this -1)))

  IPAddressOperations
  (-address? [_] false)
  (address-bytes [_] prefix)
  (address-length [_] (address-length prefix))

  IPNetworkOperations
  (network?* [_] true)
  (network-length [_] length))

(ns-unmap *ns* '->IPNetwork)

(defn ^:private address*
  [^bytes bytes]
  (when (-address? bytes)
    (IPAddress. nil bytes)))

(defn ^:private big-integer-width
  [^BigInteger addr]
  (cond
    (neg? (.signum addr))
    (throw (ex-info "BigInteger address must be nonnegative."
                    {:address addr}))
    (> (.bitLength addr) IPParser/IPV6_BIT_LEN)
    (throw (ex-info "BigInteger address exceeds the IPv6 address range."
                    {:address addr :max-bits IPParser/IPV6_BIT_LEN}))
    (> (.bitLength addr) IPParser/IPV4_BIT_LEN)
    IPParser/IPV6_BYTE_LEN
    :else
    IPParser/IPV4_BYTE_LEN))

(defn network
  "Return the IP network for representation `net` or `prefix` and `length`.
  Accepted representations are strings, byte arrays, `java.net.InetAddress`,
  `java.math.BigInteger`, and existing address or network values. This is
  lenient for those supported representations: malformed input returns `nil`.
  An unsupported type throws `java.lang.IllegalArgumentException`. In the
  two-argument form, `length` accepts a number coercible to `long`; nil throws
  `java.lang.NullPointerException`, a non-number throws
  `java.lang.ClassCastException`, and a numeric value outside the long range
  throws `java.lang.IllegalArgumentException`. A long-range prefix outside the
  address width or not network-aligned returns `nil`. Parsing is O(1) for the
  fixed address width."
  {:tag `IPNetwork}
  ([net] (-network net))
  ([prefix length] (-network prefix length)))

(defn ->network
  "Coerce `net` to an IPNetwork. Accepted values are the representations
  accepted by `network`. This is strict: malformed or unsupported input throws
  `inet.data.ip.IPNetworkException`."
  {:tag `IPNetwork}
  [net]
  (try
    (or (network net)
        (throw (IPNetworkException.
                (format "Cannot interpret %s as an IP network." (pr-str net)))))
    (catch IPException e
      (throw e))
    (catch Exception e
      (throw (IPNetworkException.
              (format "Cannot interpret %s as an IP network." (pr-str net)) e)))))

(defn ^:private network*
  [^bytes bytes ^long length]
  (when (network?* bytes length)
    (IPNetwork. nil bytes length)))

(defn address?
  "Determine if `addr` represents an IP address. It accepts strings, byte
  arrays, `java.net.InetAddress`, `java.math.BigInteger`, and existing IP
  values. This is lenient and returns `false` for unsupported or malformed
  input without throwing. Validation is O(1) for the fixed address width."
  [addr] (and (satisfies? IPAddressOperations addr)
              (boolean (-address? addr))))

(defn network?
  "Determine if `net` represents an IP network. It accepts the same values as
  `network`; the two-argument form also accepts a number coercible to `long` as
  the prefix length. This is lenient and returns `false` for unsupported or
  malformed address/network input. With a supported address, nil `length`
  throws `java.lang.NullPointerException` and a non-number throws
  `java.lang.ClassCastException`; a numeric value outside the long range throws
  `java.lang.IllegalArgumentException`. Validation is O(1)."
  ([net]
     (and (satisfies? IPNetworkOperations net)
          (boolean (network?* net))))
  ([addr length]
     (and (satisfies? IPNetworkOperations addr)
          (boolean (network?* addr length)))))

(defn inet-address
  "Generate a `java.net.InetAddress` from `addr`, which may be any value
  accepted by `->address`. This is strict and throws
  `inet.data.ip.IPAddressException` when `addr` cannot be interpreted."
  {:tag `InetAddress}
  [addr] (let [addr (->address addr)]
            (InetAddress/getByAddress (address-bytes addr))))

(defn network-trunc
  "Create a network. Its prefix is the first `length` bits of `prefix`, and its
  length is `length`. `prefix` accepts values accepted by `->address`; `length`
  accepts a number coercible to `long`. In the one-argument form, a malformed
  string or nil prefix throws `inet.data.ip.IPNetworkException`, while a
  malformed primitive byte array throws `inet.data.ip.IPAddressException`; in
  the two-argument form any invalid prefix throws
  `inet.data.ip.IPAddressException`. Nil length throws
  `java.lang.NullPointerException`, a non-number throws
  `java.lang.ClassCastException`, a negative length throws
  `java.lang.ArrayIndexOutOfBoundsException`, and a length wider than the
  address returns `nil` when it is representable as a long. A numeric value
  outside the long range throws `java.lang.IllegalArgumentException`. In the
  one-argument form, an unsupported prefix type also throws
  `java.lang.IllegalArgumentException`. The operation is O(1)."
  {:tag `IPNetwork}
  ([prefix]
     (network-trunc prefix (network-length prefix)))
  ([prefix length]
     (let [prefix (->address prefix)]
       (network (doto-let [prefix (byte-array (address-bytes prefix))]
                (loop [zbits (long (- (address-length prefix) length)),
                       i (->> prefix alength dec long)]
                  (cond (>= zbits 8) (do (aset prefix i (byte 0))
                                         (recur (- zbits 8) (dec i)))
                        (pos? zbits) (->> (bit-shift-left -1 zbits)
                                          (bit-and (long (aget prefix i)))
                                          byte (aset prefix i)))))
                length))))

(defn ->network-set
  "Create a hierarchical set from networks in finite seqable `coll`, or an
  empty set when `coll` is nil. Members accept the representations supported by
  `network`. A malformed supported member becomes nil; a one-member nil set is
  retained, while comparing nil with another member throws
  `inet.data.ip.IPNetworkException`. An unsupported member or non-seqable
  `coll` throws `java.lang.IllegalArgumentException`. Building `n` members takes
  O(n^2) worst-case time and O(n) space."
  [coll]
  (-> (apply hier-set-by network-contains? network-compare
             (map network coll))
      (vary-meta assoc :type ::network-set)))

(defn network-set
  "Create a hierarchical set from variadic networks `nets`; each member accepts
  the representations supported by `network`. A malformed supported member
  becomes nil; a one-member nil set is retained, while comparing nil with
  another member throws `inet.data.ip.IPNetworkException`. An unsupported
  member throws `java.lang.IllegalArgumentException`. Building `n` members
  takes O(n^2) worst-case time and O(n) space."
  [& nets] (->network-set nets))

(defmethod clojure.core/print-method ::network-set
  [nets ^java.io.Writer w]
  (.write w "#ip/network-set #{")
  (loop [first? true, nets (seq nets)]
    (when nets
      (when-not first? (.write w " "))
      (print-method (first nets) w)
      (recur false (next nets))))
  (.write w "}"))

(defn network-supernet
  "Return a network that contains `net`. Its prefix is `n` bits shorter. The
  default is 1. `net` accepts values accepted by `->network`; `n` is a numeric
  integer prefix decrement. Invalid `net` throws `inet.data.ip.IPNetworkException`; nil
  `n` throws `java.lang.NullPointerException`, and a non-number throws
  `java.lang.ClassCastException`. A decrement larger than the prefix returns
  `nil`. Negative `n` is not rejected: it requests a more-specific prefix and
  returns that network when aligned and in range, otherwise nil. This is O(1)."
  ([net] (network-supernet net 1))
  ([net n]
     (let [net (->network net)
           pbits (- (network-length net) n)]
       (when-not (neg? pbits)
         (network-trunc net pbits)))))

(defn network-subnets
  "Set of networks in the network `net` which have `n` more bits of network
  prefix, default 1. `net` accepts values accepted by `->network`; `n` is a
  non-negative integer prefix increment. Invalid `net` throws
  `inet.data.ip.IPNetworkException`; nil `n` throws
  `java.lang.NullPointerException`, a non-number throws
  `java.lang.ClassCastException`, and a negative `n` can throw
  `java.lang.IllegalArgumentException` unless `network-length + n` is
  non-negative and the input prefix is aligned to that shorter length; in that
  case it returns that one more-general network. Negative values are outside
  this function's subnet contract. If `n` exceeds the available host bits, the
  result is an empty set.
  For `m = 2^n` valid subnets, construction takes O(m log m) time and O(m)
  output space."
  ([net] (network-subnets net 1))
  ([net n]
     (let [net (->network net)
           pbits (+ (network-length net) n)
           nbits (- (address-length net) pbits)
           one (.shiftLeft BigInteger/ONE nbits)
           lower (address->BigInteger net)
           over (.add lower (.shiftLeft one n))
           step (fn step [^BigInteger addr]
                  (lazy-seq
                   (when (neg? (.compareTo addr over))
                     (cons (network addr pbits) (step (.add addr one))))))]
       (apply network-set (step lower)))))

(defn address-zero?
  "True if and only if the address `addr` is the zero address. `addr` must be a
  string, primitive byte array, `java.net.InetAddress`, `java.math.BigInteger`,
  or existing IP address/network value. This low-level predicate does not
  validate supported values; malformed strings can be treated as empty and
  return true. Nil or an unsupported type throws
  `java.lang.IllegalArgumentException`. The scan is O(w), where `w` is the
  supplied byte width."
  [addr] (every? zero? (address-bytes addr)))

(defn address-networks
  "Return the minimal set of networks that contains only the addresses from
`start` to `stop`, *inclusive*. Both endpoints accept values accepted by
`->address`; invalid input throws `inet.data.ip.IPAddressException`. For `k`
output networks, the operation is O(k log k) time and O(k) output space; a
reversed range returns an empty set."
  [start stop]
  (let [start (->address start)
        stop (->address stop)
        nnet (fn [net]
               (let [net' (network-supernet net)]
                 (if (or (nil? net')
                         (pos? (network-compare start (network-nth net' 0)))
                         (neg? (network-compare stop (network-nth net' -1))))
                   net
                   (recur net'))))
        step (fn step [start]
               (lazy-seq
                (when-not (pos? (network-compare start stop))
                  (let [net (nnet (network start))
                        start' (address-add net (network-count net))]
                    (cons net (when-not (address-zero? start')
                                (step start')))))))]
    (apply network-set (step start))))

(defn- aggregate-input-network
  [value]
  (try
    (network value)
    (catch IllegalArgumentException _
      nil)))

(defn- absorb-networks
  [nets]
  (loop [remaining (seq (sort network-compare nets))
         kept []
         kept-set #{}]
    (if-let [net (first remaining)]
      (if (loop [supernet (network-supernet net)]
            (when supernet
              (or (contains? kept-set supernet)
                  (recur (network-supernet supernet)))))
        (recur (next remaining) kept kept-set)
        (recur (next remaining) (conj kept net) (conj kept-set net)))
      kept)))

(defn- merge-network-siblings
  [nets]
  (mapcat (fn [[supernet siblings]]
            (if (and supernet (= 2 (count siblings)))
              [(network-supernet (first siblings))]
              siblings))
          (group-by #(when (pos? (network-length %))
                       (network-supernet %))
                    nets)))

(defn aggregate-networks
  "Return a minimal, ascending `network-set` covering `values` exactly.

  Values can be addresses or networks. Bare addresses become host networks.
  This function aggregates IPv4 and IPv6 values independently. It skips
  invalid or unsupported values, the same as the invalid values that this
  namespace's address and network predicates reject.

  `values` is a collection or seqable of address/network representations.
  Invalid members are skipped and an empty input returns an empty set. For `n`
  input values, the worst-case running time is O(n^2) and additional space is
  O(n); the number of fixed-point passes is bounded by the 128-bit maximum
  address width. A non-seqable `values` throws
  `java.lang.IllegalArgumentException`."
  [values]
  (let [nets (->> values
                  (keep aggregate-input-network)
                  distinct
                  absorb-networks)]
    (loop [nets nets]
      (let [merged (-> nets merge-network-siblings absorb-networks)]
        (if (= (set nets) (set merged))
          (apply network-set nets)
          (recur merged))))))

(defn network-intersect
  "Return the intersection of CIDR networks `left` and `right`, or nil when
  they are disjoint. Since CIDR networks can only overlap by containment, the
  result is the narrower network."
  [left right]
  (let [left (->network left)
        right (->network right)]
    (when (= (address-length left) (address-length right))
      (cond
        (network-contains? left right) right
        (network-contains? right left) left))))

(defn network-subtract
  "Return a minimal `network-set` covering network `left` minus `right`."
  [left right]
  (let [left (->network left)
        right (->network right)]
    (cond
      (not= (address-length left) (address-length right))
      (network-set left)

      (nil? (network-intersect left right))
      (network-set left)

      (= left right)
      (network-set)

      (network-contains? right left)
      (network-set)

      :else
      (apply network-set
             (mapcat #(network-subtract % right)
                     (network-subnets left))))))

(extend-type IPAddress
  IPAddressConstruction
  (-address [this] this)

  IPNetworkConstruction
  (-network
    ([this] (IPNetwork. nil (address-bytes this) (address-length this)))
    ([this length] (network* (address-bytes this) length))))

(extend-type IPNetwork
  IPAddressConstruction
  (-address [this] (IPAddress. nil (address-bytes this)))

  IPNetworkConstruction
  (-network
    ([this] this)
    ([this length] (network* (address-bytes this) length))))

#_:clj-kondo/ignore
(extend-type (java.lang.Class/forName "[B")
  IPAddressConstruction
  (-address [this] (address* this))

  IPAddressOperations
  (-address? [this]
    (let [len (alength ^bytes this)]
      (or (= len IPParser/IPV4_BYTE_LEN)
          (= len IPParser/IPV6_BYTE_LEN))))
  (address-bytes [this] this)
  (address-length [this] (* 8 (alength ^bytes this)))

  IPNetworkConstruction
  (-network
    ([this] (network* this (address-length this)))
    ([this length] (network* this length)))

  IPNetworkOperations
  (network?*
    ([_] false)
    ([this length]
       (and (-address? this)
            (>= length 0)
            (<= length (address-length this))
            (->> (iterate #(if (pos? %) (- % 8) 0) length)
                 (map (fn [b rem]
                        (let [mask (if (<= 8 rem) 0 (bit-shift-right 0xff rem))]
                          (bit-and b mask)))
                      this)
                 (every? zero?)))))
  (network-length [this] (address-length this)))

(defn ^:private string-network-split
  [net] (str/split net #"/" 2))

(defn ^:private string-address-parts
  [addr]
  (let [percent (str/last-index-of addr "%")]
    (if (nil? percent)
      [addr nil]
      [(subs addr 0 percent) (subs addr (inc percent))])))

(defn ^:private string-network-parts
  [net] (let [[prefix length] (string-network-split net)
              [prefix zone] (string-address-parts prefix)
              length (when length
                       (or (ignore-errors (Long/parseLong length)) -1))]
          [(when (and (nil? zone) (IPParser/isValid prefix))
             (IPParser/parse prefix)) length]))

(extend-type String
  IPAddressConstruction
  (-address [addr]
    (let [[literal zone] (string-address-parts (first (string-network-split addr)))
          ^bytes bytes (when (IPParser/isValid literal)
                         (IPParser/parse literal))]
      (when (and bytes
                 (or (nil? zone) (seq zone))
                 (or (nil? zone) (= IPParser/IPV6_BYTE_LEN (alength bytes))))
        (IPAddress. (when zone {:zone zone}) bytes))))

  IPAddressOperations
  (-address? [this]
    (boolean (-address this)))
  (address-bytes [this]
    (let [[literal _] (string-address-parts (first (string-network-split this)))]
      (IPParser/parse literal)))
  (address-length [this]
    (let [[literal _] (string-address-parts (first (string-network-split this)))]
      (IPParser/length literal)))

  IPNetworkConstruction
  (-network
    ([this]
       (let [[prefix length] (string-network-parts this)]
         (when prefix
           (if length
             (network* prefix length)
             (network* prefix (address-length prefix))))))
    ([this length]
       (let [[prefix _] (string-network-parts this)]
         (when prefix
           (network* prefix length)))))

  IPNetworkOperations
  (network?*
    ([this]
       (let [[prefix length] (string-network-parts this)]
         (when length
           (network?* prefix length))))
    ([this length]
       (let [[prefix _] (string-network-parts this)]
         (network?* prefix length))))
  (network-length [this]
    (let [[_ length] (string-network-parts this)]
      (if-let [prefix (first (string-network-parts this))]
        (let [address-length (address-length prefix)
              length (or length address-length)]
          (if (<= 0 length address-length)
            length
            (throw (IPNetworkException.
                    (format "Cannot interpret %s as an IP network." (pr-str this))))))
        (throw (IPNetworkException.
                (format "Cannot interpret %s as an IP network." (pr-str this))))))))

(extend-type InetAddress
  IPAddressConstruction
  (-address [addr]
    (address* (.getAddress addr)))

  IPAddressOperations
  (-address? [_] true)
  (address-bytes [addr] (.getAddress addr))
  (address-length [addr]
    (case-expr (class addr)
      java.net.Inet4Address IPParser/IPV4_BIT_LEN
      java.net.Inet6Address IPParser/IPV6_BIT_LEN
      -1))

  IPNetworkConstruction
  (-network
    ([this] (IPNetwork. nil (address-bytes this) (address-length this)))
    ([this length] (network* (address-bytes this) length)))

  IPNetworkOperations
  (network?*
    ([_] false)
    ([this length] (network?* (address-bytes this) length)))
  (network-length [this] (address-length this)))

(extend-type BigInteger
  IPAddressConstruction
  (-address [addr] (address* (address-bytes addr)))

  IPAddressOperations
  (-address? [_] true)
  (address-bytes [addr]
    (let [b (.toByteArray addr)
          n (big-integer-width addr)]
      (byte-array (concat (repeat (max 0 (- n (alength b))))
                          (take-last n b)))))
  (address-length [addr]
    (* 8 (big-integer-width addr)))

  IPNetworkConstruction
  (-network
    ([this] (IPNetwork. nil (address-bytes this) (address-length this)))
    ([this length] (network* (address-bytes this) length)))

  IPNetworkOperations
  (network?*
    ([_] false)
    ([this length] (network?* (address-bytes this) length)))
  (network-length [this] (address-length this)))

(extend-type nil
  IPNetworkOperations
  (network?*
    ([_] false)
    ([_ _] false))
  (network-length [_]
    (throw (IPNetworkException. "Cannot interpret nil as an IP network."))))

(def ^:private special-use-registry
  (with-open [reader (io/reader
                      (or (io/resource "inet/data/special-use-registry.edn")
                          (throw (ex-info "Special-use registry resource not found."
                                          {:resource "inet/data/special-use-registry.edn"}))))]
    (edn/read (java.io.PushbackReader. reader))))

(def ^:private special-use-blocks
  (into {}
        (map (fn [{:keys [name] network-literal :network}]
               [name (network network-literal)]))
        (:blocks special-use-registry)))

(def ^:private special-use-block-order
  (sort-by (comp - network-length val) special-use-blocks))

(def ^:private special-use-concepts
  {:loopback-v6 :loopback
   :link-local-v6 :link-local
   :multicast-v6 :multicast
   :documentation-v6 :documentation})

(defn ^:private special-use-network
  [value]
  (cond (address? value) (network value)
        (network? value) (network value)))

(defn ^:private matching-special-use-blocks
  [value]
  (when-let [value (special-use-network value)]
          (keep (fn [[name block]]
            (when (or (network-contains? block value)
                      (network-contains? value block))
              name))
          special-use-block-order)))

(defn special-use
  "Return the most-specific special-use concept keyword for `value`, or `nil`.

  The function accepts address-like and network-like values. For a network
  value, the result is non-`nil` only when the full network is inside one
  special-use block. IPv4-mapped IPv6 addresses have the `:ipv4-mapped`
  classification. This function does not unwrap them as IPv4 addresses. The
  input may be an address or network representation accepted by this namespace.
  Concretely, it accepts strings, primitive byte arrays,
  `java.net.InetAddress`, `java.math.BigInteger`, and existing IP values. This
  is lenient: malformed, nil, or unsupported input returns `nil` without
  throwing. Classification is O(1) because address width and the block table
  are fixed."
  [value]
  (let [block (first (matching-special-use-blocks value))]
    (get special-use-concepts block block)))

(defn ^:private special-use-in?
  [value names]
  (boolean (some names (matching-special-use-blocks value))))

(defn private?
  "Return true when `value` is in an RFC 1918 private block.

  For a network value, the result is true only when the full network is
  inside one private block. This function does not unwrap IPv4-mapped IPv6
  addresses. `value` accepts strings, primitive byte arrays,
  `java.net.InetAddress`, `java.math.BigInteger`, and existing IP values. This
  is lenient: malformed, nil, or unsupported input returns false without
  throwing. Classification is O(1)."
  [value]
  (special-use-in? value #{:private :private-172 :private-192}))

(defn loopback?
  "Return true when `value` is in an IPv4 or IPv6 loopback block.

  For a network value, the result is true only when the full network is
  inside a loopback block. This function does not unwrap IPv4-mapped IPv6
  addresses. `value` accepts strings, primitive byte arrays,
  `java.net.InetAddress`, `java.math.BigInteger`, and existing IP values. This
  is lenient: malformed, nil, or unsupported input returns false without
  throwing. Classification is O(1)."
  [value]
  (special-use-in? value #{:loopback :loopback-v6}))

(defn link-local?
  "Return true when `value` is in an IPv4 or IPv6 link-local block.

  For a network value, the result is true only when the full network is
  inside a link-local block. This function does not unwrap IPv4-mapped IPv6
  addresses. `value` accepts strings, primitive byte arrays,
  `java.net.InetAddress`, `java.math.BigInteger`, and existing IP values. This
  is lenient: malformed, nil, or unsupported input returns false without
  throwing. Classification is O(1)."
  [value]
  (special-use-in? value #{:link-local :link-local-v6}))

(defn multicast?
  "Return true when `value` is in an IPv4 or IPv6 multicast block.

  For a network value, the result is true only when the full network is
  inside a multicast block. This function does not unwrap IPv4-mapped IPv6
  addresses. `value` accepts strings, primitive byte arrays,
  `java.net.InetAddress`, `java.math.BigInteger`, and existing IP values. This
  is lenient: malformed, nil, or unsupported input returns false without
  throwing. Classification is O(1)."
  [value]
  (special-use-in? value #{:multicast :multicast-v6}))

(defn unique-local?
  "Return true when `value` is in the IPv6 unique-local block.

  For a network value, the result is true only when the full network is
  inside the unique-local block. This function does not unwrap IPv4-mapped
  IPv6 addresses. `value` accepts strings, primitive byte arrays,
  `java.net.InetAddress`, `java.math.BigInteger`, and existing IP values. This
  is lenient: malformed, nil, or unsupported input returns false without
  throwing. Classification is O(1)."
  [value]
  (special-use-in? value #{:unique-local}))

(defn shared-address-space?
  "Return true when `value` is in the IPv4 shared address space block.

  For a network value, the result is true only when the full network is
  inside the shared address space block. This function does not unwrap
  IPv4-mapped IPv6 addresses. `value` accepts strings, primitive byte arrays,
  `java.net.InetAddress`, `java.math.BigInteger`, and existing IP values. This
  is lenient: malformed, nil, or unsupported input returns false without
  throwing. Classification is O(1)."
  [value]
  (special-use-in? value #{:shared-address-space}))

(defn documentation?
  "Return true when `value` is in an IPv4 or IPv6 documentation block.

  For a network value, the result is true only when the full network is
  inside a documentation block. This function does not unwrap IPv4-mapped
  IPv6 addresses. `value` accepts strings, primitive byte arrays,
  `java.net.InetAddress`, `java.math.BigInteger`, and existing IP values. This
  is lenient: malformed, nil, or unsupported input returns false without
  throwing. Classification is O(1)."
  [value]
  (special-use-in? value #{:documentation :documentation-2 :documentation-3
                            :documentation-v6}))

(defn benchmarking?
  "Return true when `value` is in the IPv4 benchmarking block.

  For a network value, the result is true only when the full network is
  inside the benchmarking block. This function does not unwrap IPv4-mapped
  IPv6 addresses. `value` accepts strings, primitive byte arrays,
  `java.net.InetAddress`, `java.math.BigInteger`, and existing IP values. This
  is lenient: malformed, nil, or unsupported input returns false without
  throwing. Classification is O(1)."
  [value]
  (special-use-in? value #{:benchmarking}))

(defn unspecified?
  "Return true when `value` is the IPv6 unspecified address.

  For a network value, the result is true only when the full network is
  `::/128`. This function does not unwrap IPv4-mapped IPv6 addresses. `value`
  accepts strings, primitive byte arrays, `java.net.InetAddress`,
  `java.math.BigInteger`, and existing IP values. This is lenient: malformed,
  nil, or unsupported input returns false without throwing. Classification is
  O(1)."
  [value]
  (special-use-in? value #{:unspecified}))

(defn broadcast?
  "Return true when `value` is the IPv4 limited broadcast address.

  For a network value, the result is true only when the full network is
  `255.255.255.255/32`. This function does not unwrap IPv4-mapped IPv6
  addresses. `value` accepts strings, primitive byte arrays,
  `java.net.InetAddress`, `java.math.BigInteger`, and existing IP values. This
  is lenient: malformed, nil, or unsupported input returns false without
  throwing. Classification is O(1)."
  [value]
  (special-use-in? value #{:limited-broadcast}))

(defn reserved?
  "Return true when `value` is in the IPv4 reserved-for-future-use block.

  For a network value, the result is true only when the full network is
  inside the reserved block. This function does not unwrap IPv4-mapped IPv6
  addresses. `value` accepts strings, primitive byte arrays,
  `java.net.InetAddress`, `java.math.BigInteger`, and existing IP values. This
  is lenient: malformed, nil, or unsupported input returns false without
  throwing. Classification is O(1)."
  [value]
  (special-use-in? value #{:reserved}))

(defn global?
  "Return true when `value` is valid and matches no special-use block.

  For a network value, the result is true only when the full network is
  outside every special-use block. This function does not unwrap IPv4-mapped
  IPv6 addresses. `value` accepts strings, primitive byte arrays,
  `java.net.InetAddress`, `java.math.BigInteger`, and existing IP values. This
  is lenient: malformed, nil, or unsupported input returns false without
  throwing. Classification is O(1)."
  [value]
  (boolean (and (special-use-network value)
                (not (seq (matching-special-use-blocks value))))))

(defmethod clojure.core/print-method IPAddress
  ([^IPAddress addr ^java.io.Writer w]
     (.write w "#ip/address \"")
     (.write w (str addr))
     (.write w "\"")))

(defmethod clojure.core/print-method IPNetwork
  ([^IPNetwork net ^java.io.Writer w]
     (.write w "#ip/network \"")
     (.write w (str net))
     (.write w "\"")))

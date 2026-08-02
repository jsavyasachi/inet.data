(ns inet.data.ip
  "Functions for interacting with IP addresses and networks."
  (:require [clojure.string :as str])
  (:use [inet.data.util :only [ignore-errors case-expr ubyte sbyte longest-run
                               bytes-hash-code doto-let]]
        [hier-set.core :only [hier-set-by]])
  (:import [clojure.lang IFn IObj ILookup BigInt Indexed Seqable]
           [inet.data.ip IPParser IPNetworkComparison]
           [java.io Serializable]
           [java.util Arrays]
           [java.net InetAddress]))

(defprotocol ^:no-doc IPAddressConstruction
  "Construct a full address object."
  (^:private -address [addr]
    "Produce an IPAddress from `addr`."))

(defprotocol ^:no-doc IPAddressOperations
  "Operations on objects which may be treated as addresses."
  (^:private -address? [addr]
    "Returns whether or not the value represents a valid address.")
  (^bytes address-bytes [addr]
    "Retrieve the bytes representation of this address.")
  (^long address-length [addr]
    "The length in bits of this address."))

(defprotocol ^:no-doc IPNetworkConstruction
  "Construct a full network object."
  (^:private -network [net] [prefix length]
    "Produce an IPNetwork from `net` or `prefix` & `length`."))

(defprotocol ^:no-doc IPNetworkOperations
  "Operations on objects which may be treated as networks."
  (^:private network?* [net] [addr length]
    "Returns whether or not the value represents a valid network.")
  (network-length [net]
    "The length in bits of the network prefix."))

(defn ^:private string-address-ipv4
  [^bytes bytes]
  (->> bytes (map ubyte) (str/join ".")))

(letfn [(->short [[m x]] (-> m (bit-shift-left 8) (bit-or x)))
        (->str [xs] (->> xs (map #(format "%x" %)) (str/join ":")))]
  (defn ^:private string-address-ipv6
    [^bytes bytes]
    (let [shorts (->> bytes (map ubyte) (partition 2) (map ->short))]
      (if-let [[nt nd] (longest-run 0 shorts)]
        (str (->str (take nt shorts)) "::" (->str (drop (+ nt nd) shorts)))
        (->str shorts)))))

(defn ^:private string-address
  [^bytes bytes]
  (case-expr (alength bytes)
    IPParser/IPV4_BYTE_LEN (string-address-ipv4 bytes)
    IPParser/IPV6_BYTE_LEN (string-address-ipv6 bytes)))

(deftype IPAddress [meta, ^bytes bytes]
  Serializable

  Object
  (toString [this] (string-address bytes))
  (hashCode [this] (bytes-hash-code bytes))
  (equals [this other]
    (or (identical? this other)
        (and (instance? IPAddress other)
             (Arrays/equals bytes ^bytes (address-bytes other)))))

  IObj
  (meta [this] meta)
  (withMeta [this new-meta] (IPAddress. new-meta bytes))

  Comparable
  (compareTo [this other]
    (let [plen1 (long (address-length bytes))
          ^bytes prefix2 (address-bytes other),
          plen2 (long (network-length other))]
      (IPNetworkComparison/networkCompare bytes plen1 prefix2 plen2)))

  IPAddressOperations
  (-address? [this] true)
  (address-bytes [this] bytes)
  (address-length [this] (address-length bytes))

  IPNetworkOperations
  (network?* [this] false)
  (network-length [this] (address-length bytes)))

(ns-unmap *ns* '->IPAddress)

(defn address
  "The IP address for representation `addr`."
  {:tag `IPAddress}
  [addr] (-address addr))

;; BigInteger mapping is internal-only.  BigInteger doesn't preserve the input
;; byte-array size, so we need to prepend a pseudo-magic prefix to retain the
;; address length.
(defn ^:private address->BigInteger
  "Convert `addr` to an internal-format BigInteger."
  {:tag `BigInteger}
  [addr] (->> addr address-bytes (cons (byte 63)) byte-array BigInteger.))

(defn address-add
  "The `n`th address following `addr` numerically."
  {:tag `IPAddress}
  [addr n]
  (->> (condp instance? n
         BigInteger n
         BigInt     (.toBigInteger ^BigInt n)
         ,,,,,,     (BigInteger/valueOf (long n)))
       (.add (address->BigInteger addr))
       address))

(defn address-range
  "Sequence of addresses from `start` to `stop` *inclusive*."
  [start stop]
  (let [stop (address->BigInteger stop)]
    ((fn step [^BigInteger addr]
       (lazy-seq
        (when-not (pos? (.compareTo addr stop))
          (cons (address addr) (step (.add addr BigInteger/ONE))))))
     (address->BigInteger start))))

(defn network-compare
  "Compare the prefixes of networks `left` and `right`, with the same result
semantics as `compare`.  When `stable` is true (the default), 0 will only be
returned when the networks are value-identical; when `stable` is false, 0 will
be returned as long as the networks are identical up to their minimum common
prefix length."
  (^long [left right] (network-compare true left right))
  (^long [stable left right]
     (let [^bytes prefix1 (address-bytes left), plen1 (network-length left)
           ^bytes prefix2 (address-bytes right), plen2 (network-length right)]
       (IPNetworkComparison/networkCompare stable prefix1 plen1 prefix2 plen2))))

(defn network-contains?
  "Determine if network `net` contains the address/network `addr`."
  [net addr]
  (let [length (network-length net)]
    (and (<= length (network-length addr))
         (zero? (network-compare false net addr)))))

(defn network-count
  "Count of addresses in network `net`."
  [net]
  (let [nbits (- (address-length net) (network-length net))]
    (if (> 63 nbits)
      (bit-shift-left 1 nbits)
      (BigInt/fromBigInteger (.shiftLeft BigInteger/ONE nbits)))))

(defn network-nth
  "The `n`th address in the network `net`.  Negative `n`s count backwards
from the final address at -1."
  [net n] (address-add net (if (neg? n) (+ n (network-count net)) n)))

(deftype IPNetwork [meta, ^bytes prefix, ^long length]
  Serializable

  Object
  (toString [this] (str (string-address prefix) "/" length))
  (hashCode [this] (bytes-hash-code prefix length))
  (equals [this other]
    (or (identical? this other)
        (and (instance? IPNetwork other)
             (= length (network-length other))
             (Arrays/equals prefix ^bytes (address-bytes other)))))

  IObj
  (meta [this] meta)
  (withMeta [this new-meta] (IPNetwork. new-meta prefix length))

  Comparable
  (compareTo [this other]
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
  (count [this] (network-count this))
  (nth [this n] (network-nth this n))

  Seqable
  (seq [this]
    (address-range (nth this 0) (nth this -1)))

  IPAddressOperations
  (-address? [this] false)
  (address-bytes [this] prefix)
  (address-length [this] (address-length prefix))

  IPNetworkOperations
  (network?* [this] true)
  (network-length [this] length))

(ns-unmap *ns* '->IPNetwork)

(defn ^:private address*
  [orig ^bytes bytes]
  (when (-address? bytes)
    (IPAddress. nil bytes)))

(defn network
  "The IP network for representation `net` or `prefix` & `length`."
  {:tag `IPNetwork}
  ([net] (-network net))
  ([prefix length] (-network prefix length)))

(defn ^:private network*
  [orig ^bytes bytes ^long length]
  (when (network?* bytes length)
    (IPNetwork. nil bytes length)))

(defn address?
  "Determine if `addr` is a value which represents an IP address."
  [addr] (and (satisfies? IPAddressOperations addr)
              (boolean (-address? addr))))

(defn network?
  "Determine if `net` is a value which represents an IP network."
  ([net]
     (and (satisfies? IPNetworkOperations net)
          (boolean (network?* net))))
  ([addr length]
     (and (satisfies? IPNetworkOperations addr)
          (boolean (network?* addr length)))))

(defn inet-address
  "Generate a java.net.InetAddress from the provided value."
  {:tag `InetAddress}
  [addr] (InetAddress/getByAddress (address-bytes addr)))

(defn network-trunc
  "Create a network with a prefix consisting of the first `length` bits of
`prefix` and a length of `length`."
  {:tag `IPNetwork}
  ([prefix]
     (network-trunc prefix (network-length prefix)))
  ([prefix length]
     (network (doto-let [prefix (byte-array (address-bytes prefix))]
                (loop [zbits (long (- (address-length prefix) length)),
                       i (->> prefix alength dec long)]
                  (cond (>= zbits 8) (do (aset prefix i (byte 0))
                                         (recur (- zbits 8) (dec i)))
                        (pos? zbits) (->> (bit-shift-left -1 zbits)
                                          (bit-and (long (aget prefix i)))
                                          byte (aset prefix i)))))
              length)))

(defn ->network-set
  "Create a hierarchical set from networks in `coll`."
  [coll]
  (-> (apply hier-set-by network-contains? network-compare
             (map network coll))
      (vary-meta assoc :type ::network-set)))

(defn network-set
  "Create a hierarchical set from networks `nets`."
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
  "Network containing the network `net` with a prefix shorter by `n` bits,
default 1."
  ([net] (network-supernet net 1))
  ([net n]
     (let [pbits (- (network-length net) n)]
       (when-not (neg? pbits)
         (network-trunc net pbits)))))

(defn network-subnets
  "Set of networks within the network `net` which have `n` additional bits of
network prefix, default 1."
  ([net] (network-subnets net 1))
  ([net n]
     (let [pbits (+ (network-length net) n)
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
  "True iff address `addr` is the zero address."
  [addr] (every? zero? (address-bytes addr)))

(defn address-networks
  "Minimal set of networks containing only the addresses in the range from
`start` to `stop` *inclusive*."
  [start stop]
  (let [stop (address stop)
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
    (apply network-set (step (address start)))))

(extend-type IPAddress
  IPAddressConstruction
  (-address [this] this)

  IPNetworkConstruction
  (-network
    ([this] (IPNetwork. nil (address-bytes this) (address-length this)))
    ([this length] (network* this (address-bytes this) length))))

(extend-type IPNetwork
  IPAddressConstruction
  (-address [this] (IPAddress. nil (address-bytes this)))

  IPNetworkConstruction
  (-network
    ([this] this)
    ([this length] (network* this (address-bytes this) length))))

(extend-type (java.lang.Class/forName "[B")
  IPAddressConstruction
  (-address [this] (address* this this))

  IPAddressOperations
  (-address? [this]
    (let [len (alength ^bytes this)]
      (or (= len IPParser/IPV4_BYTE_LEN)
          (= len IPParser/IPV6_BYTE_LEN))))
  (address-bytes [this] this)
  (address-length [this] (* 8 (alength ^bytes this)))

  IPNetworkConstruction
  (-network
    ([this] (network* this this (address-length this)))
    ([this length] (network* this this length)))

  IPNetworkOperations
  (network?*
    ([this] false)
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

(defn ^:private string-network-parts
  [net] (let [[prefix length] (string-network-split net)
              length (when length
                       (or (ignore-errors (Long/parseLong length)) -1))]
          [(IPParser/parse prefix) length]))

(extend-type String
  IPAddressConstruction
  (-address [addr] (address* addr (address-bytes addr)))

  IPAddressOperations
  (-address? [this]
    (IPParser/isValid (first (string-network-split this))))
  (address-bytes [this]
    (IPParser/parse (first (string-network-split this))))
  (address-length [this]
    (IPParser/length (first (string-network-split this))))

  IPNetworkConstruction
  (-network
    ([this]
       (let [[prefix length] (string-network-parts this)]
         (if length
           (network* this prefix length)
           (network* this prefix (address-length prefix)))))
    ([this length]
       (network* this (address-bytes this) length)))

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
      (or length (address-length this)))))

(extend-type InetAddress
  IPAddressConstruction
  (-address [addr]
    (address* (.getHostAddress addr) (.getAddress addr)))

  IPAddressOperations
  (-address? [addr] true)
  (address-bytes [addr] (.getAddress addr))
  (address-length [addr]
    (case-expr (class addr)
      java.net.Inet4Address IPParser/IPV4_BIT_LEN
      java.net.Inet6Address IPParser/IPV6_BIT_LEN
      -1))

  IPNetworkConstruction
  (-network
    ([this] (IPNetwork. nil (address-bytes this) (address-length this)))
    ([this length] (network* this (address-bytes this) length)))

  IPNetworkOperations
  (network?*
    ([this] false)
    ([this length] (network?* (address-bytes this) length)))
  (network-length [this] (address-length this)))

(extend-type BigInteger
  IPAddressConstruction
  (-address [addr] (address* addr (address-bytes addr)))

  IPAddressOperations
  (-address? [addr] true)
  (address-bytes [addr]
    (let [b (.toByteArray addr),
          n (if (> (alength b) IPParser/IPV6_BYTE_LEN)
              IPParser/IPV6_BYTE_LEN
              IPParser/IPV4_BYTE_LEN)]
      (byte-array (take-last n b))))
  (address-length [addr]
    (if (> (.bitLength addr) IPParser/IPV6_BIT_LEN)
      IPParser/IPV6_BIT_LEN
      IPParser/IPV4_BIT_LEN))

  IPNetworkConstruction
  (-network
    ([this] (IPNetwork. nil (address-bytes this) (address-length this)))
    ([this length] (network* this (address-bytes this) length)))

  IPNetworkOperations
  (network?*
    ([this] false)
    ([this length] (network?* (address-bytes this) length)))
  (network-length [this] (address-length this)))

(def ^:private special-use-blocks
  {:this-network (network "0.0.0.0/8")
   :private (network "10.0.0.0/8")
   :shared-address-space (network "100.64.0.0/10")
   :loopback (network "127.0.0.0/8")
   :link-local (network "169.254.0.0/16")
   :private-172 (network "172.16.0.0/12")
   :ietf-protocol-assignments (network "192.0.0.0/24")
   :documentation (network "192.0.2.0/24")
   :six-to-four-relay-anycast (network "192.88.99.0/24")
   :private-192 (network "192.168.0.0/16")
   :benchmarking (network "198.18.0.0/15")
   :documentation-2 (network "198.51.100.0/24")
   :documentation-3 (network "203.0.113.0/24")
   :multicast (network "224.0.0.0/4")
   :reserved (network "240.0.0.0/4")
   :limited-broadcast (network "255.255.255.255/32")
   :unspecified (network "::/128")
   :loopback-v6 (network "::1/128")
   :ipv4-mapped (network "::ffff:0:0/96")
   :ipv4-ipv6-translation (network "64:ff9b::/96")
   :discard-only (network "100::/64")
   :teredo (network "2001::/32")
   :documentation-v6 (network "2001:db8::/32")
   :six-to-four (network "2002::/16")
   :unique-local (network "fc00::/7")
   :link-local-v6 (network "fe80::/10")
   :multicast-v6 (network "ff00::/8")})

(def ^:private special-use-block-order
  (sort-by (comp - network-length val) special-use-blocks))

(defn ^:private special-use-network
  [value]
  (cond (address? value) (network value)
        (network? value) (network value)))

(defn ^:private matching-special-use-blocks
  [value]
  (when-let [value (special-use-network value)]
    (keep (fn [[name block]]
            (when (network-contains? block value) name))
          special-use-block-order)))

(defn special-use
  "Return the most-specific special-use block keyword for `value`, or nil.

  Address-like and network-like values are accepted.  For a network value, the
  result is non-nil only when the entire network is inside one special-use
  block.  IPv4-mapped IPv6 addresses are classified as `:ipv4-mapped`; they
  are not unwrapped and classified as IPv4 addresses."
  [value]
  (first (matching-special-use-blocks value)))

(defn ^:private special-use-in?
  [value names]
  (some names (matching-special-use-blocks value)))

(defn private?
  "Return true when `value` is in an RFC 1918 private block.

  For a network value, true requires the entire network to be inside one
  private block.  IPv4-mapped IPv6 addresses are not unwrapped."
  [value]
  (special-use-in? value #{:private :private-172 :private-192}))

(defn loopback?
  "Return true when `value` is in an IPv4 or IPv6 loopback block.

  For a network value, true requires the entire network to be inside a
  loopback block.  IPv4-mapped IPv6 addresses are not unwrapped."
  [value]
  (special-use-in? value #{:loopback :loopback-v6}))

(defn link-local?
  "Return true when `value` is in an IPv4 or IPv6 link-local block.

  For a network value, true requires the entire network to be inside a
  link-local block.  IPv4-mapped IPv6 addresses are not unwrapped."
  [value]
  (special-use-in? value #{:link-local :link-local-v6}))

(defn multicast?
  "Return true when `value` is in an IPv4 or IPv6 multicast block.

  For a network value, true requires the entire network to be inside a
  multicast block.  IPv4-mapped IPv6 addresses are not unwrapped."
  [value]
  (special-use-in? value #{:multicast :multicast-v6}))

(defn unique-local?
  "Return true when `value` is in the IPv6 unique-local block.

  For a network value, true requires the entire network to be inside the
  unique-local block.  IPv4-mapped IPv6 addresses are not unwrapped."
  [value]
  (special-use-in? value #{:unique-local}))

(defn shared-address-space?
  "Return true when `value` is in the IPv4 shared address space block.

  For a network value, true requires the entire network to be inside the
  shared address space block.  IPv4-mapped IPv6 addresses are not unwrapped."
  [value]
  (special-use-in? value #{:shared-address-space}))

(defn documentation?
  "Return true when `value` is in an IPv4 or IPv6 documentation block.

  For a network value, true requires the entire network to be inside a
  documentation block.  IPv4-mapped IPv6 addresses are not unwrapped."
  [value]
  (special-use-in? value #{:documentation :documentation-2 :documentation-3
                            :documentation-v6}))

(defn benchmarking?
  "Return true when `value` is in the IPv4 benchmarking block.

  For a network value, true requires the entire network to be inside the
  benchmarking block.  IPv4-mapped IPv6 addresses are not unwrapped."
  [value]
  (special-use-in? value #{:benchmarking}))

(defn unspecified?
  "Return true when `value` is the IPv6 unspecified address.

  For a network value, true requires the entire network to be `::/128`.
  IPv4-mapped IPv6 addresses are not unwrapped."
  [value]
  (special-use-in? value #{:unspecified}))

(defn broadcast?
  "Return true when `value` is the IPv4 limited broadcast address.

  For a network value, true requires the entire network to be
  `255.255.255.255/32`. IPv4-mapped IPv6 addresses are not unwrapped."
  [value]
  (special-use-in? value #{:limited-broadcast}))

(defn reserved?
  "Return true when `value` is in the IPv4 reserved-for-future-use block.

  For a network value, true requires the entire network to be inside the
  reserved block.  IPv4-mapped IPv6 addresses are not unwrapped."
  [value]
  (special-use-in? value #{:reserved}))

(defn global?
  "Return true when `value` is valid and matches no special-use block.

  For a network value, true requires the entire network to be outside every
  special-use block.  IPv4-mapped IPv6 addresses are not unwrapped."
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

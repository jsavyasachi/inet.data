(ns inet.data.arpa
  "Conversion between IP addresses or networks and reverse-DNS domains.

This namespace connects `inet.data.ip` and `inet.data.dns`. Neither namespace
depends on the other."
  (:require [clojure.string :as str]
            [inet.data.dns :as dns]
            [inet.data.ip :as ip]))

(set! *warn-on-reflection* true)

(defn- byte-unsigned
  [^bytes bytes index]
  (bit-and 0xff (aget bytes index)))

(defn- ipv4-domain-name
  [^bytes bytes prefix-length]
  (str (->> (range (/ prefix-length 8))
            reverse
            (map #(byte-unsigned bytes %))
            (str/join "."))
       (when (pos? prefix-length) ".")
       "in-addr.arpa"))

(defn- ipv6-domain-name
  [^bytes bytes prefix-length]
  (let [nibbles (->> (range (alength bytes))
                     (map #(format "%02x" (byte-unsigned bytes %)))
                     (apply str))]
    (str (->> nibbles
              (take (/ prefix-length 4))
              reverse
              (str/join "."))
         (when (pos? prefix-length) ".")
         "ip6.arpa")))

(defn ip->domain
  "Return the reverse-DNS `dns/domain` for IP address or network `value`.

  This function supports IPv4 networks only at octet-aligned prefixes, and
  IPv6 networks only at nibble-aligned prefixes. Non-aligned prefixes return
  `nil`. This function deliberately does not implement RFC 2317 classless IPv4
  delegation. It treats IPv4-mapped IPv6 values as IPv6 values and returns
  `ip6.arpa` names. Malformed input returns `nil`, the same as the
  non-exceptional behavior of `ip/address?` and `dns/domain?`.
  The accepted values are strings, byte arrays, `java.net.InetAddress`,
  `java.math.BigInteger`, and existing IP address or network values. This is
  lenient and returns `nil` for malformed, nil, unsupported, or non-aligned
  input; it catches every `java.lang.Exception` and does not throw for those
  failures. Conversion is O(1) for the fixed maximum IP address width."
  [value]
  (try
    (let [network? (ip/network? value)
          address? (ip/address? value)]
      (when (or network? address?)
        (let [^bytes bytes (ip/address-bytes value)
              address-length (* 8 (alength bytes))
              prefix-length (if network? (ip/network-length value) address-length)]
          (when (and (<= 0 prefix-length address-length)
                     (or (not network?)
                         (zero? (mod prefix-length (if (= 4 (alength bytes)) 8 4)))))
            (dns/domain
             (if (= 4 (alength bytes))
               (ipv4-domain-name bytes prefix-length)
               (ipv6-domain-name bytes prefix-length)))))))
    (catch Exception _ nil)))

(defn- classless-ipv4-domain-name
  [^bytes bytes prefix-length]
  (let [octets (map #(byte-unsigned bytes %) (range 4))]
    (str (first (reverse octets)) "/" prefix-length "."
         (->> octets (drop-last) reverse (str/join "."))
         ".in-addr.arpa")))

(defn classless-ip->domain
  "Return the RFC 2317 reverse-DNS `dns/domain` for an IPv4 network.

  This opt-in conversion accepts classless prefixes from `/25` through `/31`
  beneath a `/24` reverse zone. It returns `nil` for IPv4 addresses, IPv6
  values, prefixes outside that range, and malformed input. The existing
  `ip->domain` behavior is unchanged."
  [value]
  (try
    (when (and (ip/network? value)
               (= 4 (alength ^bytes (ip/address-bytes value)))
               (<= 25 (ip/network-length value) 31))
      (dns/domain
       (classless-ipv4-domain-name (ip/address-bytes value)
                                   (ip/network-length value))))
    (catch Exception _ nil)))

(defn- parse-decimal-octet
  [label]
  (when (re-matches #"(?:0|[1-9][0-9]{0,2})" label)
    (let [value (Long/parseLong label)]
      (when (<= value 255) value))))

(defn- parse-hex-nibble
  [label]
  (when (re-matches #"[0-9a-f]" label)
    label))

(defn- reverse-domain-parts
  [labels suffix parser max-parts]
  (let [labels (map str/lower-case labels)
        suffix-labels (take-last 2 labels)]
    (when (= suffix suffix-labels)
      (let [parts (vec (drop-last 2 labels))]
        (when (<= (count parts) max-parts)
          (let [parsed (map parser parts)]
            (when (every? some? parsed)
              (vec parsed))))))))

(defn- ipv4-domain->ip
  [labels]
  (when-let [parts (reverse-domain-parts labels ["in-addr" "arpa"]
                                        parse-decimal-octet 4)]
    (let [address (->> (concat (reverse parts) (repeat (- 4 (count parts)) 0))
                       (str/join "."))]
      (if (= 4 (count parts))
        (ip/address address)
        (ip/network address (* 8 (count parts)))))))

(defn- classless-label
  [label]
  (let [[octet prefix] (str/split label #"/" -1)]
    (when (and octet prefix
               (parse-decimal-octet octet)
               (re-matches #"2[5-9]|3[01]" prefix))
      [(parse-decimal-octet octet) (Long/parseLong prefix)])))

(defn classless-domain->ip
  "Return the IPv4 network represented by an RFC 2317 reverse-DNS `value`.

  The classless label must contain the network's final octet and a prefix
  from `/25` through `/31`, followed by the other three reversed octets and
  `in-addr.arpa`. One trailing dot is accepted. Malformed input returns `nil`."
  [value]
  (try
    (when (or (string? value) (dns/domain? value))
      (let [name (str/replace (str value) #"\.$" "")]
        (when (dns/domain? name)
          (let [labels (map str/lower-case (dns/domain-labels name))
                suffix-labels (take-last 2 labels)
                parts (vec (drop-last 2 labels))]
            (when (and (= ["in-addr" "arpa"] suffix-labels)
                       (= 4 (count parts)))
              (when-let [[octet prefix] (classless-label (first parts))]
                (when (every? parse-decimal-octet (rest parts))
                    (ip/network (str/join "." (concat (reverse (rest parts))
                                                        [octet])) prefix))))))))
    (catch Exception _ nil)))

(defn- ipv6-domain->ip
  [labels]
  (when-let [parts (reverse-domain-parts labels ["ip6" "arpa"]
                                        parse-hex-nibble 32)]
    (let [nibbles (apply str (concat (reverse parts)
                                     (repeat (- 32 (count parts)) "0")))
          address (->> nibbles (partition 4) (map #(apply str %))
                       (str/join ":"))]
      (if (= 32 (count parts))
        (ip/address address)
        (ip/network address (* 4 (count parts)))))))

(defn domain->ip
  "Return an IP address or network represented by reverse-DNS `value`.

  This function reads `in-addr.arpa` names at octet granularity and `ip6.arpa`
  names at nibble granularity. Suffix matching ignores case. The function
  accepts one trailing dot. Malformed input returns `nil`. IPv4-mapped IPv6
  names stay IPv6 and therefore give an `ip6.arpa` result. Accepted values are
  strings, primitive byte arrays, existing DNS domain values, and nil. This is
  lenient and returns `nil` for malformed or unsupported input; it catches every
  `java.lang.Exception` and does not throw for those failures. Conversion is
  O(b) in the encoded domain length."
  [value]
  (try
    (when (or (string? value) (dns/domain? value))
      (let [name (str/replace (str value) #"\.$" "")]
        (when (dns/domain? name)
          (let [labels (dns/domain-labels name)]
            (or (ipv4-domain->ip labels)
                (ipv6-domain->ip labels))))))
    (catch Exception _ nil)))

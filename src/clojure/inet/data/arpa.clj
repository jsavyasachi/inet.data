(ns inet.data.arpa
  "Conversion between IP addresses or networks and reverse-DNS domains.

This namespace is the bridge between `inet.data.ip` and `inet.data.dns`;
neither namespace depends on the other."
  (:require [clojure.string :as str]
            [inet.data.dns :as dns]
            [inet.data.ip :as ip]))

(defn- byte-unsigned
  [^bytes bytes index]
  (bit-and 0xff (aget bytes index)))

(defn- address-string->bytes
  [address]
  (if (str/includes? address ".")
    (byte-array (map #(unchecked-byte (Long/parseLong %)) (str/split address #"\.")))
    (let [[left right] (str/split address #"::" -1)
          left (if (seq left) (str/split left #":") [])
          right (if (seq right) (str/split right #":") [])
          groups (concat left (repeat (- 8 (count left) (count right)) "0") right)]
      (byte-array (mapcat (fn [group]
                            (let [value (Long/parseLong group 16)]
                              [(unchecked-byte (bit-shift-right value 8)) (unchecked-byte value)]))
                          groups)))))

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

  IPv4 networks are supported only at octet-aligned prefixes, and IPv6
  networks only at nibble-aligned prefixes. Non-aligned prefixes return `nil`;
  RFC 2317 classless IPv4 delegation is intentionally not implemented here.
  IPv4-mapped IPv6 values are treated as IPv6 values and produce `ip6.arpa`
  names. Malformed input returns `nil`, following the non-exceptional behavior
  of `ip/address?` and `dns/domain?`."
  [value]
  (try
    (let [network? (ip/network? value)
          address? (ip/address? value)]
      (when (or network? address?)
        (let [address-string (if network?
                               (first (str/split (str value) #"/" 2))
                               (str (ip/address value)))
              ^bytes bytes (address-string->bytes address-string)
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

  `in-addr.arpa` names are interpreted at octet granularity and `ip6.arpa`
  names at nibble granularity. Suffix matching is case-insensitive and one
  trailing dot is accepted. Malformed input returns `nil`. IPv4-mapped IPv6
  names remain IPv6 and therefore return an `ip6.arpa` result."
  [value]
  (try
    (when (or (string? value) (dns/domain? value))
      (let [name (str/replace (str value) #"\.$" "")]
        (when (dns/domain? name)
          (let [labels (dns/domain-labels name)]
            (or (ipv4-domain->ip labels)
                (ipv6-domain->ip labels))))))
    (catch Exception _ nil)))

(ns inet.data.ip-test
  (:require [inet.data.ip :as ip])
  (:use [clojure.test])
  (:import [java.net InetAddress]))

(deftest test-address-validation
  (testing "Validation"
    (testing "of IPv4 addresses which are"
      (is (= true (ip/address? "192.168.1.1")) "valid")
      (is (= false (ip/address? "8.x.17.y")) "invalid, non-numeric")
      (is (= false (ip/address? "8.8.256.7")) "invalid, numeric"))
    (testing "of IPv6 addresses which are"
      (is (= true (ip/address? "17:fe77::1899:12")) "valid")
      (is (= false (ip/address? "17::qq")) "invalid, non-numeric")
      (is (= false (ip/address? "17::18::ae")) "invalid, numeric"))))

(deftest test-address-roundtrip
  (testing "Round-tripping"
    (let [addr "172.12.16.1"]
      (is (= addr (-> addr ip/address str)) "IPv4 address strings"))
    (let [addr "fe:1100::1"]
      (is (= addr (-> addr ip/address str)) "IPv6 address strings"))
    (let [addr "::2:2:1:1:1"]
      (is (= addr (-> addr ip/address str)) "No IPv6 elision-stomping"))))

(deftest test-rfc-5952-ipv6-formatting
  (testing "The first zero run is compressed when longest runs tie"
    (is (= "2001:db8::1:0:0:1"
           (str (ip/address "2001:db8:0:0:1:0:0:1")))))
  (testing "A single zero group is not compressed"
    (is (= "2001:db8:0:1:1:1:1:1"
           (str (ip/address "2001:db8:0:1:1:1:1:1")))))
  (testing "IPv4-mapped IPv6 uses dotted-quad formatting"
    (is (= "::ffff:192.168.1.1"
           (str (ip/address "::ffff:192.168.1.1"))))
    (is (= "::ffff:192.168.1.1/128"
           (str (ip/network "::ffff:192.168.1.1")))))
  (testing "Existing canonical forms remain unchanged"
    (is (= "2001:0:0:1::1"
           (str (ip/address "2001:0:0:1:0:0:0:1"))))
    (let [addr "2001:db8:0:0:1:0:0:1"]
      (is (= (ip/address addr)
             (ip/address (str (ip/address addr))))))
    (is (= "::"
           (str (ip/address "0:0:0:0:0:0:0:0"))))
    (is (= "2001:db8::1"
           (str (ip/address "2001:db8::1"))))
    (is (= "2001:db8:0:1:1:1:1:1/128"
           (str (ip/network "2001:db8:0:1:1:1:1:1"))))))

(deftest test-ipv6-zone-ids
  (let [zoned (ip/address "fe80::1%eth0")
        unzoned (ip/address "fe80::1")]
    (is (ip/address? "fe80::1%eth0"))
    (is (= "fe80::1%eth0" (str zoned)))
    (is (= {:zone "eth0"} (meta zoned)))
    (is (= unzoned zoned))
    (is (= (hash unzoned) (hash zoned)))
    (is (zero? (compare unzoned zoned)))
    (is (nil? (ip/address "192.168.1.1%eth0")))
    (is (nil? (ip/network "fe80::1%eth0")))
    (is (nil? (ip/network "fe80::1%eth0/64"))))
  (doseq [literal ["0.0.0.0" "192.168.1.1" "::" "2001:db8::1"
                   "fe80::1%eth0" "::ffff:192.168.1.1"]]
    (let [addr (ip/address literal)]
      (is (= addr (ip/address (str addr))) literal))))

(deftest test-network
  (testing "Creating networks"
    (testing "from IPv4 addresses"
      (let [addr-str   "192.168.0.0"
            addr-obj   (InetAddress/getByName addr-str)
            addr-bytes (.getAddress addr-obj)
            test-data [[addr-str "string"]
                       [addr-obj "InetAddress"]
                       [addr-bytes "bytes"]]]
        (doseq [[addr src] test-data]
          (is (= "192.168.0.0/32" (-> addr ip/network str))
              (format "From %s with implied prefix-length." src))
          (is (= "192.168.0.0/16" (-> addr (ip/network 16) str))
              (format "From %s with explicit prefix-length." src)))))
    (testing "from IPv6 addresses"
      (let [addr-str   "fe:11::"
            addr-obj   (InetAddress/getByName addr-str)
            addr-bytes (.getAddress addr-obj)
            test-data  [[addr-str "string"]
                        [addr-obj "InetAddress"]
                        [addr-bytes "bytes"]]]
        (doseq [[addr src] test-data]
          (is (= "fe:11::/128" (-> addr ip/network str))
              (format "From %s with implied prefix-length." src))
          (is (= "fe:11::/32" (-> addr (ip/network 32) str))
              (format "From %s with explicit prefix-length." src)))))))

(deftest test-network-trunc
  (testing "Creating networks, truncating prefixes"
    (is (= "192.168.0.128/25" (-> "192.168.0.255/25" ip/network-trunc str)))
    (is (= "192.168.0.128/25" (-> "192.168.0.255" (ip/network-trunc 25) str)))))

(deftest test-compare
  (testing "Identical addresses compare as identical"
    (is (zero? (ip/network-compare "8.8.8.8" "8.8.8.8")))
    (is (zero? (compare (ip/address "8.8.8.8") (ip/address "8.8.8.8")))))
  (testing "Identical networks compare as identical"
    (is (zero? (ip/network-compare "8.8.8.0/28" "8.8.8.0/28")))
    (is (zero? (compare (ip/network "8.8.8.0/28") (ip/network "8.8.8.0/28")))))
  (testing "Differing addresses compare in proper order"
    (is (neg? (ip/network-compare "8.8.8.7" "8.8.8.8")))
    (is (pos? (ip/network-compare "8.8.8.7" "7.8.8.8")))
    (is (neg? (compare (ip/address "8.8.8.7") (ip/address "8.8.8.8"))))
    (is (pos? (compare (ip/address "8.8.8.7") (ip/address "7.8.8.8"))))))

(deftest test-network-contains
  (testing "Network does contain address"
    (is (ip/network-contains? "192.168.0.0/16" "192.168.13.37"))
    (is (ip/network-contains? "192.168.0.0/17" "192.168.127.1")))
  (testing "Network doesn't contain address"
    (is (not (ip/network-contains? "192.168.0.0/16" "8.8.8.8")))
    (is (not (ip/network-contains? "192.168.0.0/17" "192.168.128.1")))))

(deftest test-network-set
  (testing "Sets of networks"
    (let [networks (->> (range 0 256) (map #(ip/network (str "10.0.0." %)))
                        (apply ip/network-set))]
      (is (contains? networks "10.0.0.1"))
      (is (not (contains? networks "10.0.1.1"))))
    (is (= (mapcat seq (ip/network-set "10.0.0.0/24" "10.0.1.0/31"))
           (ip/address-range "10.0.0.0" "10.0.1.1"))
        "Network seqs convert correctly to address range.")
    (is (= (ip/network-set "10.0.0.0/24" "10.0.1.0/31")
           (ip/address-networks "10.0.0.0" "10.0.1.1"))
        "Address range converted correctly to set of networks")))

(deftest test-network-set-to-array-jdk-11
  (let [expected [(ip/network "10.0.0.0/24")
                 (ip/network "10.0.1.0/24")]
        networks (ip/network-set "10.0.0.0/24" "10.0.1.0/24")]
    (is (= expected (vec (.toArray networks))))
    (is (= expected (vec (.toArray networks (into-array expected)))))
    (is (= expected (vec networks)))
    (is (= expected (into [] networks)))))

(deftest test-network-set-edn
  (let [nets (ip/network-set "10.0.0.0/8" "192.168.0.0/16")]
    (is (= nets (-> nets pr-str read-string)))))

;; IPv6 parity for the range/subnet helpers (previously untested).
(deftest test-network-nth
  (testing "IPv4"
    (is (= (ip/address "192.168.0.0") (ip/network-nth "192.168.0.0/30" 0)))
    (is (= (ip/address "192.168.0.2") (ip/network-nth "192.168.0.0/30" 2)))
    (is (= (ip/address "192.168.0.3") (ip/network-nth "192.168.0.0/30" -1))))
  (testing "IPv6"
    (is (= (ip/address "2001:db8::")  (ip/network-nth "2001:db8::/126" 0)))
    (is (= (ip/address "2001:db8::3") (ip/network-nth "2001:db8::/126" 3)))
    (is (= (ip/address "2001:db8::3") (ip/network-nth "2001:db8::/126" -1)))))

(deftest test-network-subnets
  (testing "IPv4"
    (is (= (ip/network-set "10.0.0.0/9" "10.128.0.0/9")
           (ip/network-subnets "10.0.0.0/8")))
    (is (= (ip/network-set "10.0.0.0/10" "10.64.0.0/10"
                           "10.128.0.0/10" "10.192.0.0/10")
           (ip/network-subnets "10.0.0.0/8" 2))))
  (testing "IPv6"
    (is (= (ip/network-set "2001:db8::/33" "2001:db8:8000::/33")
           (ip/network-subnets "2001:db8::/32")))))

(deftest test-address-networks-v6
  (testing "IPv4 single aligned block"
    (is (= (ip/network-set "192.168.0.0/30")
           (ip/address-networks "192.168.0.0" "192.168.0.3"))))
  (testing "IPv6 single aligned block"
    (is (= (ip/network-set "2001:db8::/126")
           (ip/address-networks "2001:db8::" "2001:db8::3")))))

(defn aggregate-networks
  [nets]
  (if-let [aggregate (ns-resolve 'inet.data.ip 'aggregate-networks)]
    (aggregate nets)
    ::missing))

(defn covered-addresses
  [nets]
  (set (mapcat seq nets)))

(deftest test-aggregate-networks
  (testing "absorbs networks contained by another"
    (is (= (ip/network-set "10.0.0.0/8")
           (aggregate-networks ["10.0.0.0/8" "10.1.0.0/16"]))))
  (testing "merges adjacent siblings"
    (is (= (ip/network-set "10.0.0.0/23")
           (aggregate-networks ["10.0.0.0/24" "10.0.1.0/24"]))))
  (testing "repeats sibling merges to a fixpoint"
    (is (= (ip/network-set "10.0.0.0/22")
           (aggregate-networks ["10.0.0.0/24" "10.0.1.0/24"
                                "10.0.2.0/24" "10.0.3.0/24"]))))
  (testing "does not merge non-adjacent networks"
    (is (= (ip/network-set "10.0.0.0/24" "10.0.2.0/24")
           (aggregate-networks ["10.0.2.0/24" "10.0.0.0/24"]))))
  (testing "aggregates IPv4 and IPv6 independently"
    (is (= (ip/network-set "10.0.0.0/23" "2001:db8::/127")
           (aggregate-networks ["2001:db8::/128" "10.0.0.0/24"
                                "10.0.1.0/24" "2001:db8::1/128"]))))
  (testing "treats bare addresses as host networks"
    (is (= (ip/network-set "192.0.2.0/31")
           (aggregate-networks ["192.0.2.1" "192.0.2.0"])))
    (is (= (ip/network-set "2001:db8::/127")
           (aggregate-networks [(ip/address "2001:db8::")
                                (ip/address "2001:db8::1")]))))
  (testing "deduplicates and accepts unsorted input"
    (is (= (ip/network-set "10.0.0.0/23")
           (aggregate-networks ["10.0.1.0/24" "10.0.0.0/24"
                                "10.0.0.0/24"]))))
  (testing "empty input returns an empty network set"
    (is (= (ip/network-set) (aggregate-networks []))))
  (testing "skips malformed entries"
    (is (= (ip/network-set "10.0.0.0/24")
           (aggregate-networks ["not-an-address" nil "10.0.0.0/24"]))))
  (testing "output covers exactly the input space"
    (doseq [inputs [["192.0.2.0/30" "192.0.2.4/30" "192.0.2.8/32"]
                    ["192.0.2.1" "192.0.2.2" "192.0.2.7"]
                    ["2001:db8::/126" "2001:db8::4/127" "2001:db8::7"]
                    ["10.0.0.0/28" "10.0.0.16/29" "10.0.0.24/30"]]]
      (is (= (covered-addresses (map ip/network inputs))
             (covered-addresses (aggregate-networks inputs)))
          (str "coverage for " inputs)))))

(deftest test-special-use-blocks
  (let [blocks {:this-network ["0.0.0.0" "0.0.0.0" "0.255.255.255"]
                :private ["10.0.0.1" "10.0.0.0" "10.255.255.255"]
                :shared-address-space ["100.64.0.1" "100.64.0.0" "100.127.255.255"]
                :loopback ["127.0.0.1" "127.0.0.0" "127.255.255.255"]
                :link-local ["169.254.1.1" "169.254.0.0" "169.254.255.255"]
                :private-172 ["172.16.1.1" "172.16.0.0" "172.31.255.255"]
                :ietf-protocol-assignments ["192.0.0.1" "192.0.0.0" "192.0.0.255"]
                :documentation ["192.0.2.1" "192.0.2.0" "192.0.2.255"]
                :six-to-four-relay-anycast ["192.88.99.1" "192.88.99.0" "192.88.99.255"]
                :private-192 ["192.168.1.1" "192.168.0.0" "192.168.255.255"]
                :benchmarking ["198.18.0.1" "198.18.0.0" "198.19.255.255"]
                :documentation-2 ["198.51.100.1" "198.51.100.0" "198.51.100.255"]
                :documentation-3 ["203.0.113.1" "203.0.113.0" "203.0.113.255"]
                :multicast ["224.0.0.1" "224.0.0.0" "239.255.255.255"]
                :reserved ["240.0.0.1" "240.0.0.0" "255.255.255.254"]
                :limited-broadcast ["255.255.255.255" "255.255.255.255" "255.255.255.255"]
                :unspecified ["::" "::" "::"]
                :loopback-v6 ["::1" "::1" "::1"]
                :ipv4-mapped ["::ffff:10.0.0.1" "::ffff:0:0" "::ffff:ffff:ffff"]
                :ipv4-ipv6-translation ["64:ff9b::1" "64:ff9b::" "64:ff9b::ffff:ffff"]
                :discard-only ["100::1" "100::" "100::ffff:ffff:ffff:ffff"]
                :teredo ["2001::1" "2001::" "2001:0:ffff:ffff:ffff:ffff:ffff:ffff"]
                :documentation-v6 ["2001:db8::1" "2001:db8::" "2001:db8:ffff:ffff:ffff:ffff:ffff:ffff"]
                :six-to-four ["2002::1" "2002::" "2002:ffff:ffff:ffff:ffff:ffff:ffff:ffff"]
                :unique-local ["fd00::1" "fc00::" "fdff:ffff:ffff:ffff:ffff:ffff:ffff:ffff"]
                :link-local-v6 ["fe80::1" "fe80::" "febf:ffff:ffff:ffff:ffff:ffff:ffff:ffff"]
                :multicast-v6 ["ff00::1" "ff00::" "ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff"]}
        shared-concepts {:loopback-v6 :loopback
                         :link-local-v6 :link-local
                         :multicast-v6 :multicast
                         :documentation-v6 :documentation}]
    (doseq [[block [representative first last]] blocks]
      (let [expected (get shared-concepts block block)]
        (is (= expected (ip/special-use representative)) representative)
        (is (= expected (ip/special-use first)) first)
        (is (= expected (ip/special-use last)) last)))))

(deftest test-special-use-predicates
  (testing "category predicates"
    (doseq [[predicate positive negative]
            [[ip/private? "10.1.2.3" "8.8.8.8"]
             [ip/loopback? "::1" "8.8.8.8"]
             [ip/link-local? "fe80::1" "8.8.8.8"]
             [ip/multicast? "ff02::1" "8.8.8.8"]
             [ip/unique-local? "fc00::1" "8.8.8.8"]
             [ip/shared-address-space? "100.64.0.1" "8.8.8.8"]
             [ip/documentation? "2001:db8::1" "8.8.8.8"]
             [ip/benchmarking? "198.18.0.1" "8.8.8.8"]
             [ip/unspecified? "::" "8.8.8.8"]
             [ip/broadcast? "255.255.255.255" "8.8.8.8"]
             [ip/reserved? "240.0.0.1" "8.8.8.8"]]]
      (is (true? (predicate positive)) [predicate positive])
      (is (false? (predicate negative)) [predicate negative])))
  (testing "global addresses"
    (is (ip/global? "8.8.8.8"))
    (is (ip/global? "2606:4700:4700::1111"))
    (is (not (ip/global? "192.168.1.1"))))
  (testing "IPv4-mapped IPv6 is not unwrapped"
    (is (= :ipv4-mapped (ip/special-use "::ffff:10.0.0.1")))
    (is (not (ip/private? "::ffff:10.0.0.1"))))
  (testing "malformed values are non-exceptional"
    (is (nil? (ip/special-use "not-an-address")))
    (is (false? (ip/global? "not-an-address")))))

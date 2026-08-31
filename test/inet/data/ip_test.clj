(ns inet.data.ip-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [inet.data.ip :as ip]
            [clojure.test :refer [deftest is testing]])
  (:import [java.net InetAddress]
           [inet.data.ip IPException IPNetworkException]))

(set! *warn-on-reflection* true)

(deftest test-strict-network-operations-reject-invalid-input
  (is (thrown? IPException
               (ip/network-contains? "abx-@" "abx-@"))))

(deftest test-strict-ip-coercion-and-operations
  (doseq [[f args]
          [[ip/->address ["abx-@"]]
           [ip/->address [nil]]
           [ip/->network ["abx-@"]]
           [ip/->network [nil]]
           [ip/network-compare ["abx-@" "10.0.0.1"]]
           [ip/network-compare [nil "10.0.0.1"]]
           [ip/network-contains? ["10.0.0.0/8" "abx-@"]]
           [ip/network-contains? [nil "10.0.0.1"]]
           [ip/network-count ["abx-@"]]
           [ip/network-count [nil]]
           [ip/network-nth ["abx-@" 0]]
           [ip/network-length ["abx-@"]]
           [ip/network-length [nil]]
           [ip/network-trunc ["abx-@"]]
           [ip/network-supernet ["abx-@"]]
           [ip/network-subnets ["abx-@"]]
           [ip/address-add ["abx-@" 1]]
           [ip/address-add [nil 1]]
           [ip/address-range ["abx-@" "10.0.0.1"]]
           [ip/address-networks ["abx-@" "10.0.0.1"]]
           [ip/inet-address ["abx-@"]]]]
    (is (thrown? IPException (apply f args))
        (str f " rejects invalid input"))))

(deftest test-strict-ip-operations-accept-direct-values
  (let [address (ip/address "10.0.0.1")
        network (ip/network "10.0.0.0/24")
        inet (InetAddress/getByName "10.0.0.1")
        bytes (.getAddress inet)]
    (doseq [value [address network inet bytes "10.0.0.1"]]
      (is (ip/address? (ip/->address value)))
      (is (ip/network? (ip/->network value))))
    (is (ip/network-contains? network address))
    (is (zero? (ip/network-compare false network (ip/->network bytes))))
    (is (= address (ip/address-add bytes 0)))
    (is (= address (ip/address-add inet 0)))
    (is (= 24 (ip/network-length network)))
    (is (= 32 (ip/network-length bytes)))
    (is (= inet (ip/inet-address address)))))

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
  (testing "Round trip"
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
  (testing "Create networks"
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
  (testing "Create networks with truncated prefixes"
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

(deftest test-network-count
  (testing "Counts that fit in an int"
    (is (= 16777216 (ip/network-count "10.0.0.0/8")))
    (is (= 256 (ip/network-count "2001:db8::/120")))
    (is (= 16777216 (count (ip/network "10.0.0.0/8"))))
    (is (= 256 (count (ip/network "2001:db8::/120")))))
  (testing "Exact counts remain available for larger networks"
    (doseq [[literal expected]
            [["0.0.0.0/0" 4294967296]
             ["::/96" 4294967296]
             ["::/64" 18446744073709551616N]
             ["::/0" 340282366920938463463374607431768211456N]]]
      (is (= expected (ip/network-count literal)) literal)))
  (testing "Count reports networks larger than an int"
    (doseq [literal ["0.0.0.0/0" "::/96" "::/64" "::/0"]]
      (is (thrown? IPNetworkException (count (ip/network literal)))
          literal))))

(deftest test-large-network-nth-and-seq
  (doseq [[literal first-address last-address]
          [["0.0.0.0/0" "0.0.0.0" "255.255.255.255"]
           ["::/96" "::" "::ffff:ffff"]
           ["::/0" "::" "ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff"]]]
    (let [net (ip/network literal)]
      (is (= (ip/address first-address)
             (ip/network-nth net 0))
          literal)
      (is (= (ip/address last-address)
             (ip/network-nth net -1))
          literal)
      (is (= (ip/address first-address)
             (first (seq net)))
          literal))))

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
        networks (ip/network-set "10.0.0.0/24" "10.0.1.0/24")
        ^objects target (into-array Object expected)]
    (is (= expected (vec (.toArray ^java.util.Collection networks))))
    (is (= expected (vec (.toArray ^java.util.Collection networks target))))
    (is (= expected (vec networks)))
    (is (= expected (into [] networks)))))

(deftest test-network-set-edn
  (let [nets (ip/network-set "10.0.0.0/8" "192.168.0.0/16")]
    (is (= nets (-> nets pr-str read-string)))))

;; Test IPv6 parity for the range and subnet helpers.
(deftest test-network-nth
  (testing "IPv4"
    (is (= (ip/address "192.168.0.0") (ip/network-nth "192.168.0.0/30" 0)))
    (is (= (ip/address "192.168.0.2") (ip/network-nth "192.168.0.0/30" 2)))
    (is (= (ip/address "192.168.0.3") (ip/network-nth "192.168.0.0/30" -1)))
    (is (thrown? IndexOutOfBoundsException
                 (ip/network-nth "192.168.0.0/30" 4)))
    (is (thrown? IndexOutOfBoundsException
                 (ip/network-nth "192.168.0.0/30" -5))))
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

(defn network-subtract
  [a b]
  (if-let [subtract (ns-resolve 'inet.data.ip 'network-subtract)]
    (subtract a b)
    ::missing))

(defn network-intersect
  [a b]
  (if-let [intersect (ns-resolve 'inet.data.ip 'network-intersect)]
    (intersect a b)
    ::missing))

(deftest test-network-intersect
  (testing "returns the narrower network for overlapping CIDRs"
    (is (= (ip/network "10.1.0.0/16")
           (network-intersect "10.0.0.0/8" "10.1.0.0/16")))
    (is (= (ip/network "2001:db8::/128")
           (network-intersect "2001:db8::/126" "2001:db8::/128"))))
  (testing "returns the identical network for equality"
    (is (= (ip/network "10.0.0.0/24")
           (network-intersect "10.0.0.0/24" "10.0.0.0/24"))))
  (testing "returns nil for disjoint and mixed-family networks"
    (is (nil? (network-intersect "10.0.0.0/24" "10.0.1.0/24")))
    (is (nil? (network-intersect "10.0.0.0/24" "2001:db8::/64")))))

(deftest test-network-subtract
  (testing "subtracts a contained IPv4 network into minimal complement blocks"
    (is (= (ip/network-set "10.0.0.0/16" "10.2.0.0/15" "10.4.0.0/14"
                           "10.8.0.0/13" "10.16.0.0/12" "10.32.0.0/11"
                           "10.64.0.0/10" "10.128.0.0/9")
           (network-subtract "10.0.0.0/8" "10.1.0.0/16"))))
  (testing "works for IPv6"
    (is (= (ip/network-set "2001:db8::/127")
           (network-subtract "2001:db8::/126" "2001:db8::2/127"))))
  (testing "handles disjoint, containing, and equal networks"
    (is (= (ip/network-set "10.0.0.0/24")
           (network-subtract "10.0.0.0/24" "10.0.1.0/24")))
    (is (= (ip/network-set)
           (network-subtract "10.0.0.0/24" "10.0.0.0/16")))
    (is (= (ip/network-set)
           (network-subtract "10.0.0.0/24" "10.0.0.0/24"))))
  (testing "treats mixed-family networks as disjoint"
    (is (= (ip/network-set "10.0.0.0/24")
           (network-subtract "10.0.0.0/24" "2001:db8::/64")))))

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
  (testing "Aggregates IPv4 and IPv6 independently"
    (is (= (ip/network-set "10.0.0.0/23" "2001:db8::/127")
           (aggregate-networks ["2001:db8::/128" "10.0.0.0/24"
                                "10.0.1.0/24" "2001:db8::1/128"]))))
  (testing "Treats bare addresses as host networks"
    (is (= (ip/network-set "192.0.2.0/31")
           (aggregate-networks ["192.0.2.1" "192.0.2.0"])))
    (is (= (ip/network-set "2001:db8::/127")
           (aggregate-networks [(ip/address "2001:db8::")
                                (ip/address "2001:db8::1")]))))
  (testing "Removes duplicates and accepts unsorted input"
    (is (= (ip/network-set "10.0.0.0/23")
           (aggregate-networks ["10.0.1.0/24" "10.0.0.0/24"
                                "10.0.0.0/24"]))))
  (testing "Empty input returns an empty network set"
    (is (= (ip/network-set) (aggregate-networks []))))
  (testing "Skips malformed entries"
    (is (= (ip/network-set "10.0.0.0/24")
           (aggregate-networks ["not-an-address" nil "10.0.0.0/24"]))))
  (testing "The output covers exactly the input space"
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

(deftest test-special-use-registry-resource
  (let [resource (io/resource "inet/data/special-use-registry.edn")]
    (is resource "special-use registry is on the classpath")
    (when resource
      (let [registry (edn/read (java.io.PushbackReader. (io/reader resource)))]
        (is (= "repository-snapshot-2026-08-23" (:registry-version registry)))
        (is (= ["IPv4 Special-Purpose Address Space"
                "IPv6 Special-Purpose Address Space"]
               (:source-registries registry)))
        (is (some #(= {:name :private :network "10.0.0.0/8"} %) (:blocks registry)))
        (is (= :private (ip/special-use "10.1.2.3")))
        (is (= :documentation (ip/special-use "2001:db8::1")))))))

(deftest test-special-use-predicates
  (testing "Category predicates"
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
  (testing "Global addresses"
    (is (ip/global? "8.8.8.8"))
    (is (ip/global? "2606:4700:4700::1111"))
    (is (not (ip/global? "192.168.1.1"))))
  (testing "Global networks must not overlap special-use space"
    (is (not (ip/global? "0.0.0.0/0")))
    (is (not (ip/global? "9.0.0.0/7"))))
  (testing "IPv4-mapped IPv6 is not unwrapped"
    (is (= :ipv4-mapped (ip/special-use "::ffff:10.0.0.1")))
    (is (not (ip/private? "::ffff:10.0.0.1"))))
  (testing "Malformed values do not cause an exception"
    (is (nil? (ip/special-use "not-an-address")))
    (is (false? (ip/global? "not-an-address")))))

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

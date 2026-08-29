(ns inet.data.arpa-test
  (:require [clojure.string :as str]
            [inet.data.arpa :as arpa]
            [inet.data.dns :as dns]
            [inet.data.ip :as ip]
            [clojure.test :refer [deftest is]]))

(deftest test-ipv4-address-roundtrip
  (let [addr (ip/address "10.0.2.1")
        dom (arpa/ip->domain addr)]
    (is (dns/domain? dom))
    (is (= "1.2.0.10.in-addr.arpa" (str dom)))
    (is (= addr (arpa/domain->ip dom)))))

(deftest test-ipv6-address-roundtrip
  (let [addr (ip/address "2001:db8::1")
        dom (arpa/ip->domain addr)]
    (is (= "1.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.8.b.d.0.1.0.0.2.ip6.arpa"
           (str dom)))
    (is (= addr (arpa/domain->ip dom)))))

(deftest test-network-zone-roundtrip
  (doseq [[net zone]
          [["10.0.2.0/24" "2.0.10.in-addr.arpa"]
           ["2001:db8::/32"
            "8.b.d.0.1.0.0.2.ip6.arpa"]]]
    (let [net (ip/network net)
          dom (arpa/ip->domain net)]
      (is (= zone (str dom)))
      (is (= net (arpa/domain->ip dom))))))

(deftest test-ipv4-network-alignment
  (is (nil? (arpa/ip->domain "10.0.2.0/25")))
  (is (nil? (arpa/ip->domain "2001:db8::/33"))))

(deftest test-domain-input-normalization
  (is (= (ip/address "10.0.2.1")
         (arpa/domain->ip "1.2.0.10.IN-ADDR.ARPA.")))
  (is (= (ip/network "10.0.2.0/24")
         (arpa/domain->ip "2.0.10.in-addr.arpa.")))
  (is (= (ip/address "2001:db8::1")
         (arpa/domain->ip
          (str (str/upper-case
                "1.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.8.b.d.0.1.0.0.2.ip6.arpa")
               ".")))))

(deftest test-ipv4-mapped-ipv6
  (let [dom (arpa/ip->domain "::ffff:10.0.0.1")]
    (is (str/ends-with? (str dom) ".ip6.arpa"))
    (is (= (ip/address "::ffff:10.0.0.1") (arpa/domain->ip dom)))))

(deftest test-malformed-input
  (doseq [value [nil "garbage" "1.2.0.10.in-addr.arpa.bad" "1.2.0.10.in-addr.arpa.."]]
    (is (nil? (arpa/domain->ip value)) value))
  (is (nil? (arpa/ip->domain "not-an-address"))))

(deftest test-classless-ipv4-network-roundtrip
  (doseq [[network expected-domain]
          [["192.0.2.0/26" "0/26.2.0.192.in-addr.arpa"]
           ["198.51.100.128/25" "128/25.100.51.198.in-addr.arpa"]
           ["203.0.113.64/27" "64/27.113.0.203.in-addr.arpa"]]]
    (let [network (ip/network network)
          domain (arpa/classless-ip->domain network)]
      (is (= expected-domain (str domain)))
      (is (= network (arpa/classless-domain->ip domain))))))

(deftest test-classless-prefix-range
  (doseq [prefix (range 25 32)]
    (let [network (ip/network "192.0.2.0" prefix)
          domain (arpa/classless-ip->domain network)]
      (is (dns/domain? domain) prefix)
      (is (= network (arpa/classless-domain->ip domain)) prefix)))
  (doseq [prefix (concat (range 0 25) [32])]
    (is (nil? (arpa/classless-ip->domain
               (str "192.0.2.0/" prefix))) prefix)
    (is (nil? (arpa/classless-domain->ip
               (format "0/%d.2.0.192.in-addr.arpa" prefix))) prefix)))

(deftest test-classless-invalid-input
  (doseq [value [nil (ip/address "192.0.2.0")
                (ip/network "2001:db8::/56") "not-an-ip"]]
    (is (nil? (arpa/classless-ip->domain value)) value))
  (doseq [value [nil "garbage" "0/0.2.0.192.in-addr.arpa"
                "0/32.2.0.192.in-addr.arpa"
                "0/x.2.0.192.in-addr.arpa"
                "0/26.2.0.192.example"]]
    (is (nil? (arpa/classless-domain->ip value)) value)))

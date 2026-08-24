(ns inet.data.s11n.transit-test
  (:require [clojure.test :refer :all]
            [cognitect.transit :as transit]
            [inet.data.dns :as dns]
            [inet.data.ip :as ip]
            [inet.data.s11n.transit :as s11n])
  (:import [java.io ByteArrayInputStream ByteArrayOutputStream]))

(set! *warn-on-reflection* true)

(defn- round-trip [value]
  (let [output (ByteArrayOutputStream.)
        writer (transit/writer output :json {:handlers s11n/write-handlers})]
    (transit/write writer value)
    (transit/read (transit/reader (ByteArrayInputStream. (.toByteArray output))
                                  :json
                                  {:handlers s11n/read-handlers}))))

(deftest ip-address-round-trips
  (is (= (ip/address "192.168.1.1") (round-trip (ip/address "192.168.1.1"))))
  (is (= (ip/address "2001:db8::1") (round-trip (ip/address "2001:db8::1"))))
  (is (= (ip/address "fe80::1%eth0")
         (round-trip (ip/address "fe80::1%eth0")))))

(deftest ip-network-round-trips
  (is (= (ip/network "192.168.0.0/16") (round-trip (ip/network "192.168.0.0/16"))))
  (is (= (ip/network "2001:db8::/32") (round-trip (ip/network "2001:db8::/32"))))
  (is (= (ip/network "2001:db8::1/128")
         (round-trip (ip/network "2001:db8::1/128")))))

(deftest domain-round-trips
  (is (= (dns/domain "www.example.com")
         (round-trip (dns/domain "www.example.com"))))
  (is (= (dns/domain-next (dns/domain "www.example.com"))
         (round-trip (dns/domain-next (dns/domain "www.example.com"))))))

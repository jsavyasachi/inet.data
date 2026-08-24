(ns inet.data.property-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as string]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [inet.data.dns :as dns]
            [inet.data.ip :as ip])
  (:import [inet.data.dns DNSDomainException]
           [inet.data.ip IPAddressException IPException IPNetworkException]))

(set! *warn-on-reflection* true)

(def ipv4-address-string-gen
  (gen/fmap #(string/join "." %)
            (gen/vector (gen/choose 0 255) 4)))

(def ipv6-address-string-gen
  (gen/fmap #(string/join ":" (map (partial format "%x") %))
            (gen/vector (gen/choose 0 65535) 8)))

(def domain-string-gen
  (gen/fmap #(string/join "." %)
            (gen/vector
             (gen/fmap #(apply str %)
                       (gen/vector (gen/one-of [(gen/choose (int \a) (int \z))
                                                (gen/choose (int \A) (int \Z))])
                                   1 12))
             1 5)))

(defn check-property
  [property]
  (tc/quick-check 100 property :seed 424242))

(deftest ipv4-address-parse-string-parse
  (let [result (check-property
                (prop/for-all [literal ipv4-address-string-gen]
                  (let [address (ip/address literal)]
                    (= address (ip/address (str address))))))]
    (is (:pass? result) result)))

(deftest ipv6-address-parse-string-parse
  (let [result (check-property
                (prop/for-all [literal ipv6-address-string-gen]
                  (let [address (ip/address literal)]
                    (= address (ip/address (str address))))))]
    (is (:pass? result) result)))

(deftest canonical-address-strings-are-stable
  (let [result (check-property
                (prop/for-all [literal (gen/one-of [ipv4-address-string-gen
                                                    ipv6-address-string-gen])]
                  (let [canonical (str (ip/address literal))]
                    (= canonical (str (ip/address canonical))))))]
    (is (:pass? result) result)))

(deftest domain-normalization-is-idempotent
  (let [result (check-property
                (prop/for-all [literal domain-string-gen]
                  (let [canonical (str (dns/domain literal))]
                    (= canonical (str (dns/domain canonical))))))]
    (is (:pass? result) result)))

(defn aggregate-networks
  [nets]
  ((ns-resolve 'inet.data.ip 'aggregate-networks) nets))

(def ipv4-network-string-gen
  (gen/fmap (fn [[literal prefix]]
              (str (ip/network-trunc (ip/address literal) prefix)))
            (gen/tuple ipv4-address-string-gen (gen/choose 24 32))))

(def ipv6-network-string-gen
  (gen/fmap (fn [[literal prefix]]
              (str (ip/network-trunc (ip/address literal) prefix)))
            (gen/tuple ipv6-address-string-gen (gen/choose 124 128))))

(defn covered-addresses
  [nets]
  (set (mapcat seq nets)))

(deftest aggregate-networks-preserves-exact-address-space
  (let [result (check-property
                (prop/for-all [inputs (gen/vector
                                       (gen/one-of [ipv4-network-string-gen
                                                    ipv6-network-string-gen])
                                       1 8)]
                  (= (covered-addresses (map ip/network inputs))
                     (covered-addresses (aggregate-networks inputs)))))]
    (is (:pass? result) result)))

(def malformed-input-gen
  (gen/one-of
   [(gen/return nil)
    (gen/fmap #(str "bad.." % ".com") gen/string-alphanumeric)]))

(deftest malformed-inputs-use-typed-exceptions
  (let [result (check-property
                (prop/for-all [value malformed-input-gen]
                  (and (try (ip/->address value)
                            false
                            (catch IPAddressException _ true)
                            (catch IPException _ false)
                            (catch Throwable _ false))
                       (try (ip/->network value)
                            false
                            (catch IPNetworkException _ true)
                            (catch IPException _ false)
                            (catch Throwable _ false))
                       (try (dns/->domain value)
                            false
                            (catch DNSDomainException _ true)
                            (catch Throwable _ false)))))]
    (is (:pass? result) result)))

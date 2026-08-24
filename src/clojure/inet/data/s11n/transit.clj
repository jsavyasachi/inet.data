(ns inet.data.s11n.transit
  "Transit handlers for inet.data types."
  (:require [cognitect.transit :as transit]
            [inet.data.dns :as dns]
            [inet.data.ip :as ip])
  (:import [inet.data.dns DNSDomain]
           [inet.data.ip IPAddress IPNetwork]))

(set! *warn-on-reflection* true)

(def ip-address-tag "inet.data/ip-address")
(def ip-network-tag "inet.data/ip-network")
(def domain-tag "inet.data/domain")

(defn- handler [tag rep]
  (transit/write-handler (constantly tag) rep))

(def write-handlers
  {IPAddress (handler ip-address-tag ip/address-bytes)
   IPNetwork (handler ip-network-tag
                      (fn [net]
                        {:prefix (ip/address-bytes net)
                         :length (ip/network-length net)}))
   DNSDomain (handler domain-tag
                      (fn [dom]
                        (let [length (dns/domain-length dom)]
                          {:bytes (java.util.Arrays/copyOf
                                   (dns/domain-bytes dom) (int length))})))})

(def read-handlers
  {ip-address-tag (transit/read-handler ip/address)
   ip-network-tag (transit/read-handler
                   (fn [{:keys [prefix length]}]
                     (ip/network prefix length)))
   domain-tag (transit/read-handler (comp dns/domain :bytes))})

(ns inet.data.s11n.nippy
  "Nippy serialization implementations for inet.data types."
  (:require [inet.data.dns :as dns]
            [inet.data.ip :as ip]
            [taoensso.nippy :as nippy])
  (:import [inet.data.dns DNSDomain]
           [inet.data.ip IPAddress IPNetwork]))

(set! *warn-on-reflection* true)

(defn- write-bytes [^bytes bytes ^java.io.DataOutput out]
  (.writeInt out (alength bytes))
  (.write out bytes))

(defn- read-bytes [^java.io.DataInput in]
  (let [length (.readInt in)
        bytes (byte-array length)]
    (.readFully in bytes)
    bytes))

#_:clj-kondo/ignore
(nippy/extend-freeze IPAddress :inet.data/ip-address [x out]
  (write-bytes (ip/address-bytes x) out))
#_:clj-kondo/ignore
(nippy/extend-thaw :inet.data/ip-address [in]
  (ip/address (read-bytes in)))

(nippy/extend-freeze IPNetwork :inet.data/ip-network [x out]
  (write-bytes (ip/address-bytes x) out)
  (.writeByte ^java.io.DataOutput out (ip/network-length x)))
(nippy/extend-thaw :inet.data/ip-network [in]
  (ip/network (read-bytes in) (.readUnsignedByte ^java.io.DataInput in)))

(nippy/extend-freeze DNSDomain :inet.data/domain [x out]
  (write-bytes (java.util.Arrays/copyOf (dns/domain-bytes x)
                                       (int (dns/domain-length x))) out))
(nippy/extend-thaw :inet.data/domain [in]
  (dns/domain (read-bytes in)))

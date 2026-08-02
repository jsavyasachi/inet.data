# inet.data

[![Clojars Project](https://img.shields.io/clojars/v/net.clojars.savya/inet.data.svg)](https://clojars.org/net.clojars.savya/inet.data)
[![cljdoc](https://cljdoc.org/badge/net.clojars.savya/inet.data)](https://cljdoc.org/d/net.clojars.savya/inet.data/CURRENT)
[![test](https://github.com/jsavyasachi/inet.data/actions/workflows/test.yml/badge.svg)](https://github.com/jsavyasachi/inet.data/actions/workflows/test.yml)

Inet.data is a library for modeling various Internet-related conceptual
entities as *data*, supporting applications which are *about* the modeled
entities versus *interfacing* with them.

## Stack

<a href="https://clojure.org"><img src="https://img.shields.io/badge/Clojure-5881D8?style=flat&logo=clojure&logoColor=fff" alt="Clojure" /></a>
<a href="https://clojure.org/guides/deps_and_cli"><img src="https://img.shields.io/badge/deps.edn-5881D8?style=flat&logo=clojure&logoColor=fff" alt="deps.edn" /></a>
<a href="https://clojure.github.io/tools.build/"><img src="https://img.shields.io/badge/tools.build-5881D8?style=flat&logo=clojure&logoColor=fff" alt="tools.build" /></a>

## Installation

Inet.data is available on Clojars.

Leiningen (`project.clj`):

```clj
[net.clojars.savya/inet.data "0.7.5"]
```

Clojure CLI (`deps.edn`):

```clj
net.clojars.savya/inet.data {:mvn/version "0.7.5"}
```

## Usage

Currently inet.data includes support for IP addresses and networks and for DNS
domain names.  Example usage follows; [detailed API
documentation](https://cljdoc.org/d/net.clojars.savya/inet.data) available.

### inet.data.ip

The `inet.data.ip` namespace defines types for IP addresses and networks and
associated functions.  All public functions work in terms of a protocol which
is also implemented for strings, byte arrays, and `java.net.InetAddress`.

```clj
(require '[inet.data.ip :as ip])

(ip/network-contains? "192.168.1.0/24" "192.168.1.1") ;;=> true

(ip/private? "192.168.1.1") ;;=> true
(ip/special-use "2001:db8::1") ;;=> :documentation-v6
(ip/global? "8.8.8.8") ;;=> true

(ip/address? "600d::") ;;=> true
(ip/address? "::bad::") ;;=> false

(let [rfc1918 (ip/network-set "10.0.0.0/8" "172.16.0.0/12" "192.168.0.0/16")]
  (get rfc1918 "10.31.33.7") ;;=> (#ip/network "10.0.0.0/8")
  (get rfc1918 "8.8.8.8") ;;=> nil
  )

(seq (ip/network "192.168.0.0/30"))
;;=> (#ip/address "192.168.0.0"
;;    #ip/address "192.168.0.1"
;;    #ip/address "192.168.0.2"
;;    #ip/address "192.168.0.3")

(ip/network-nth "192.168.0.0/30" -1)
;;=> #ip/address "192.168.0.3"

(ip/address-networks "192.168.0.0" "192.168.0.4")
;;=> #{#ip/network "192.168.0.0/30"
;;     #ip/network "192.168.0.4/32"}
```

### inet.data.dns

The `inet.data.dns` namespace defines a type for representing DNS domain names
and associated functions.  All public functions work in terms of a protocol
which is also implemented for strings and byte arrays.

```clj
(require '[inet.data.dns :as dns])

(dns/domain-contains? "com" "example.com") ;;=> true

(dns/domain? "example.com") ;;=> true
(dns/domain? "bad..com") ;;=> false

(dns/domain-parent "www.example.com") ;;=> #dns/domain "example.com"

(let [gtlds (dns/domain-set "com" "net" "org")]
  (get gtlds "example.com") ;;=> (#dns/domain "com")
  (get gtlds "does.not.exist") ;;=> nil
  )
```

### inet.data.format.psl

The `inet.data.format.psl` namespace defines functions for working with files
in the Mozilla Public Suffix List format.  It can automatically use the current
version of the list as maintained by the Mozilla project.  The format is
generally useful for domain suffix applications, but most applications will
need to provide their own list(s) customized for their particular use cases.

```clj
(require '[inet.data.format.psl :as psl])

(psl/lookup "www.example.co.uk") ;;=> #dns/domain "example.co.uk"
```

## License

Copyright © 2012-2015 Marshall Bockrath-Vandegrift & Damballa, Inc.

Maintenance fork (2026) by Savyasachi, original: https://github.com/damballa/inet.data.
Distributed under the [Eclipse Public License 1.0](https://www.eclipse.org/legal/epl-v10.html), preserving the original license.

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.

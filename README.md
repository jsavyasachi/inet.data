# inet.data

[![Clojars Project](https://img.shields.io/clojars/v/net.clojars.savya/inet.data.svg)](https://clojars.org/net.clojars.savya/inet.data)
[![cljdoc](https://cljdoc.org/badge/net.clojars.savya/inet.data)](https://cljdoc.org/d/net.clojars.savya/inet.data/CURRENT)
[![test](https://github.com/jsavyasachi/inet.data/actions/workflows/test.yml/badge.svg)](https://github.com/jsavyasachi/inet.data/actions/workflows/test.yml)

Inet.data is a library that models Internet-related entities as *data*. It
supports applications that are *about* the modeled entities, not applications
that *interface* with them.

## Stack

<a href="https://clojure.org"><img src="https://img.shields.io/badge/Clojure-5881D8?style=flat&logo=clojure&logoColor=fff" alt="Clojure" /></a>
<a href="https://clojure.org/guides/deps_and_cli"><img src="https://img.shields.io/badge/deps.edn-5881D8?style=flat&logo=clojure&logoColor=fff" alt="deps.edn" /></a>
<a href="https://clojure.github.io/tools.build/"><img src="https://img.shields.io/badge/tools.build-5881D8?style=flat&logo=clojure&logoColor=fff" alt="tools.build" /></a>

## Installation

Inet.data is available on Clojars.

Leiningen (`project.clj`):

```clj
[net.clojars.savya/inet.data "2.0.1"]
```

Clojure CLI (`deps.edn`):

```clj
net.clojars.savya/inet.data {:mvn/version "2.0.1"}
```

## Building

The build generates the Java parsers from the Ragel grammars under
`src/ragel`. Run `clojure -T:build ragel` to generate them again. You need
Ragel only when you change the grammars. Do not edit the generated Java files.

The large synthetic IP performance benchmark is opt-in because it is slow:
`clojure -T:build compile-java && clojure -M:test --profile integration --focus-meta :integration`.
Normal test runs skip tests tagged `^:integration`.

## Usage

Inet.data supports IP addresses and networks, DNS domain names, and reverse
DNS domains. Examples follow. See the [detailed API
documentation](https://cljdoc.org/d/net.clojars.savya/inet.data).

### inet.data.ip

The `inet.data.ip` namespace defines types for IP addresses and networks, and
the related functions. All public functions work in terms of a protocol.
Strings, byte arrays, and `java.net.InetAddress` also implement this protocol.

```clj
(require '[inet.data.ip :as ip])

(ip/network-contains? "192.168.1.0/24" "192.168.1.1") ;;=> true

(ip/private? "192.168.1.1") ;;=> true
(ip/special-use "2001:db8::1") ;;=> :documentation
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

(ip/aggregate-networks ["10.0.0.0/24" "10.0.1.0/24"])
;;=> #{#ip/network "10.0.0.0/23"}
```

Parsing has two deliberate modes. The `address`, `network`, and `domain`
constructors and their predicate forms are lenient: malformed input returns
`nil` or `false`. Operations that need a parsed value use strict coercion.
They throw `IPAddressException`, `IPNetworkException`, or `DNSDomainException`
when they cannot interpret the input.

Use `address?`, `network?`, or `domain?` to test input. Do not assert that the
input is valid. `aggregate-networks` and the ARPA conversion functions stay
lenient. They filter and convert, and they return their documented empty or
`nil` results for malformed input.

### inet.data.dns

The `inet.data.dns` namespace defines a type for DNS domain names, and the
related functions. All public functions work in terms of a protocol. Strings
and byte arrays also implement this protocol.

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

### inet.data.arpa

The `inet.data.arpa` namespace converts between IP addresses or networks and
reverse DNS domains. IPv4 zones use octet-aligned prefixes and IPv6 zones use
nibble-aligned prefixes.

```clj
(require '[inet.data.arpa :as arpa])

(arpa/ip->domain "10.0.2.1")
;;=> #dns/domain "1.2.0.10.in-addr.arpa"

(arpa/domain->ip "2.0.10.in-addr.arpa")
;;=> #ip/network "10.0.2.0/24"
```

### inet.data.format.psl

The `inet.data.format.psl` namespace defines functions for files in the
Mozilla Public Suffix List format. The default lookup uses the bundled snapshot
without a network call. The snapshot is the repository's existing
`effective_tld_names.dat`, first committed on 2012-06-15; its upstream release
date is not recorded. Most applications should supply their own list for their
use case.

```clj
(require '[inet.data.format.psl :as psl])

(psl/lookup "www.example.co.uk") ;;=> #dns/domain "example.co.uk"
```

`psl/refresh!` fetches the current list from
`https://publicsuffix.org/list/public_suffix_list.dat` and replaces the cached
list only after a successful parse. A failed refresh returns the last known-good
list, or the bundled snapshot, and does not discard the cache. Pass
`{:timeout-ms n}` to `psl/refresh!` to set the connection and read timeout for
that request. The `*network-timeout-ms*` dynamic var sets the default timeout.

## License

Copyright © 2012-2015 Marshall Bockrath-Vandegrift & Damballa, Inc.

Maintenance fork (2026) by Savyasachi, original: https://github.com/damballa/inet.data.
Distributed under the [Eclipse Public License 1.0](https://www.eclipse.org/legal/epl-v10.html), preserving the original license.

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.

# Changelog

## [1.0.0] - 2026-08-01

### BREAKING
- Operations now throw typed exceptions when input cannot be interpreted as
  an address, network, or domain, where they previously could produce
  meaningless results. Use the lenient `address?`, `network?`, and `domain?`
  predicates when callers need to test input rather than assert validity.
- Added strict `->address`, `->network`, and `->domain` coercion functions.
- `aggregate-networks` and ARPA conversions remain lenient: malformed values
  are skipped or return `nil` according to their existing contracts.

## [0.8.0] - 2026-08-01

### Added
- Classify special-use IPv4 and IPv6 addresses with `special-use` and the
  `private?`, `loopback?`, `link-local?`, `multicast?`, `unique-local?`,
  `shared-address-space?`, `documentation?`, `benchmarking?`, `unspecified?`,
  `broadcast?`, `reserved?`, and `global?` predicates.
- Add `inet.data.arpa` with `ip->domain` and `domain->ip` for `in-addr.arpa`
  and `ip6.arpa` reverse DNS zones.
- Add `aggregate-networks` for reducing collections of networks and addresses
  to a minimal exactly-equivalent set.
- Accept IPv6 zone identifiers such as `fe80::1%eth0`, preserving them for
  round-tripping in metadata while excluding them from identity, comparison,
  and serialization. Zone identifiers previously returned `nil`.

### Changed
- IPv6 text formatting now conforms to RFC 5952. String output differs from
  0.7.5 in three cases: a single 16-bit zero field is no longer compressed to
  `::`, IPv4-mapped addresses use the embedded dotted quad such as
  `::ffff:192.168.1.1`, and the first equal-length zero run is compressed
  instead of the last. Re-check consumers that compare formatted strings.
- Replace deprecated `(:use ...)` namespace forms with namespace declarations
  using `:require` and `:refer`.
- Add `clojure -T:build ragel` to regenerate Java parsers from the grammars in
  `src/ragel`.

### Fixed
- Pin `toArray` behavior for `network-set` and `domain-set`, fixing the JDK 11
  `Collection.toArray` compatibility issue through the hier-set fork.

## [0.7.5] - 2026-08-01

### Changed
- Declare the optional serialization dependencies in the pom so cljdoc can analyze their integration namespaces. They remain non-transitive for consumers.

## [0.7.4] - 2026-07-12

### Changed
- Migrate the build to deps.edn and tools.build (Java compiled via `clojure -T:build compile-java`), with Leiningen supported via lein-tools-deps.

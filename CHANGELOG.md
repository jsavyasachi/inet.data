# Changelog

## [2.1.0] - 2026-08-23

### Added

- `network-subtract` and `network-intersect` in `inet.data.ip` for exact CIDR
  set arithmetic, alongside the existing `aggregate-networks`.
- `inet.data.arpa/classless-ip->domain` and `classless-domain->ip`: opt-in
  RFC 2317 classless IPv4 reverse-DNS delegation. The existing `ip->domain`/
  `domain->ip` default behavior is unchanged.
- IDN conversion in `inet.data.dns` (`idn->ascii`/`ascii->idn`, IDNA2003 via
  `java.net.IDN`) and a fix so trailing-dot absolute domains round-trip
  distinctly from their bare form.
- `inet.data.format.psl` bundles a vendored PSL snapshot for offline/startup
  use, plus `refresh!`, a configurable fetch timeout, and fallback to the
  last-known-good list when a refresh fails.
- Optional Transit (`inet.data.s11n.transit`) and Nippy
  (`inet.data.s11n.nippy`) serialization for `IPAddress`/`IPNetwork`/
  `Domain`, following the existing optional-dependency pattern used by the
  byteable/abracad backends.
- `test.check` generative coverage for address parsing round-trips, domain
  normalization, and `aggregate-networks` invariants.
- Docstrings across `ip`, `dns`, `arpa`, and `psl` now state accepted input
  types, strict-vs-lenient behavior, the exact exception thrown on failure,
  and complexity notes where relevant.

### Changed

- The hardcoded special-use address-block map moved to a versioned EDN
  resource (`resources/inet/data/special-use-registry.edn`), citing its
  source IANA registries/RFCs, so it's auditable/diffable without a code
  change.

### Fixed

- `aggregate-networks`' absorb-networks scan was quadratic in the number of
  input networks; it now walks supernets directly, verified via a checked-in
  `^:integration` benchmark.

## [2.0.1] - 2026-08-17

### Fixed

- `network-nth` throws `IndexOutOfBoundsException` for an out-of-range index
  instead of returning an address from a different network.
- `domain-next` verifies containment before deriving children, so it no longer
  produces children for an unrelated parent of matching length.

## [2.0.0] - 2026-08-14

### BREAKING
- `count` on an `IPNetwork` throws `IPNetworkException` when the count of the
  network exceeds `Integer/MAX_VALUE`. The `count` method of `Indexed` returns a
  Java `int`, so a larger count became `0`: `0.0.0.0/0` and every IPv6 network
  of `/96` or shorter reported as empty. Use `network-count`, which returns the
  exact count for a network of any size.

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

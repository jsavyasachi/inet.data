# Changes

This changelog covers the self-published maintenance fork
(`net.clojars.savya/inet.data`). For the original release history up to 0.5.7,
see the upstream project at https://github.com/damballa/inet.data.

## 0.7.2 (2026-06-14)

- Docs-only release: standardize the README to the canonical skeleton and unify
  the status badges and CI workflow name. No code changes.

## 0.7.1 (2026-06-11)

- Docstring the serialization helpers; add the cljdoc badge.

## 0.7.0 (2026-06-05)

- **Public Suffix List section filtering.** Support filtering the PSL by its
  ICANN and PRIVATE sections, and switch to the canonical published list URL.
- Add IPv6 test coverage for `network-nth`, `network-subnets`, and
  `address-networks`.

## 0.6.0 (2026-06-05)

First self-published maintenance fork, as `net.clojars.savya/inet.data`.

- **Drop the build-time ragel dependency.** Commit the ragel-generated IP/DNS
  parser sources directly so the project builds without ragel installed.
- Modernize the toolchain: Clojure 1.12, JDK 8 target, `hier-set` 1.2.0, fork
  coordinate; drop the stale ragel/codox build config.
- Replace the dead Travis config with a GitHub Actions JDK x Clojure matrix.
- Refresh the README badges, install coordinate, and doc link; add fork
  attribution.

# Changelog

## [0.7.5] - 2026-08-01

### Changed
- Declare the optional serialization dependencies in the pom so cljdoc can analyze their integration namespaces. They remain non-transitive for consumers.

## [0.7.4] - 2026-07-12

### Changed
- Migrate the build to deps.edn and tools.build (Java compiled via `clojure -T:build compile-java`), with Leiningen supported via lein-tools-deps.

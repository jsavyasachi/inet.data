Independent verification found two reflection warnings in the touched `test/inet/data/ip_test.clj` when every touched namespace was forcibly recompiled under `(binding [*warn-on-reflection* true] ...)`:

`ip_test.clj:219:26 - reference to field toArray can't be resolved`
`ip_test.clj:220:26 - call to method toArray can't be resolved (target class is unknown)`

Make only the small mechanical repair needed in this worktree: ensure the touched test namespace itself enables `(set! *warn-on-reflection* true)` and type-hint the existing `test-network-set-to-array-jdk-11` calls/arrays correctly, without weakening assertions or changing product behavior. Then run `clojure -T:build compile-java && clojure -M:test`, and also force-require both `inet.data.ip` and `inet.data.ip-test` under `*warn-on-reflection* true` to prove zero warnings. Do not commit. End with the same required JSON object, including integer `reflection_warnings`, exact files changed, tests, dependencies, and blockers.

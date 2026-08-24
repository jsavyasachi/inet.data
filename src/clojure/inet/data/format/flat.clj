(ns inet.data.format.flat
  "Functions to load inet.data entities from flat, line-oriented files."
  (:refer-clojure :exclude [load])
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [inet.data.ip :as ip]
            [inet.data.dns :as dns]
            [inet.data.util :refer [ignore-errors ffilter]]))

(defn ^:private parse-lines [f & readers]
  (->> (mapcat line-seq readers)
       (map #(-> % (str/split #"\s+#" 2) first str/trim))
       (remove (fn [^String s] (or (.isEmpty s) (.startsWith s "#"))))
       (map #(f (str/split % #"\t")))))

(defn load
  "Read the non-`nil` members of `paths`. The files contain line-oriented,
  '#'-commented, tab-delimited entries. Parse each entry with `entryf`. Parse
  the sequence of entries with `collf`. Both callbacks must implement
  `clojure.lang.IFn`; each path may be any source accepted by
  `clojure.java.io/reader`, or nil, which is skipped. With no non-nil paths this
  leniently returns nil without invoking `collf`.

  A missing file throws `java.io.FileNotFoundException`; other open, read, or
  close failures throw `java.io.IOException`; an unsupported path throws
  `java.lang.IllegalArgumentException`; and a non-function callback throws
  `java.lang.ClassCastException` when invoked. Exceptions from either callback
  propagate unchanged. For `p` paths and `n` retained input lines, traversal is
  O(p + n) plus callback work; loader overhead is O(p), excluding `collf`'s
  result."
  [entryf collf & paths]
  (letfn [(step [readers paths]
            (if (seq paths)
              (if-let [path (first paths)]
                (with-open [r (io/reader path)]
                  (step (conj readers r) (rest paths)))
                (step readers (rest paths)))
              (when (seq readers)
                (collf (apply parse-lines entryf readers)))))]
    (step [] paths)))

(defn load-domain-set
  "Create a `dns/domain-set` from the entries in `paths`. Paths are readable
  sources accepted by `clojure.java.io/reader`, or nil. This is lenient for
  malformed domain entries: `dns/domain` converts them to nil and the set
  retains that root representation. A missing file throws
  `java.io.FileNotFoundException`, other I/O failures throw
  `java.io.IOException`, and an unsupported path throws
  `java.lang.IllegalArgumentException`. For `n` entries, longest encoded domain
  `b`, and encoded byte total `B`, loading takes O(n^2 * b + B) worst-case time
  and O(n + B) result space."
  [& paths]
  (apply load (comp dns/domain first) (partial apply dns/domain-set) paths))

(defn domain-etld
  "Return the effective TLD of `dom` from the hierarchical domain set `etlds`.
  `etlds` must support associative lookup and is normally a `dns/domain-set`;
  `dom` accepts a string, primitive byte array, existing DNS domain, or nil.
  This is lenient for malformed supported domains and returns nil, as it does
  when no proper suffix is present. An unsupported `dom` type throws
  `java.lang.IllegalArgumentException`. For a domain set of `n` entries, `k`
  matching ancestors, and encoded length `b`, lookup is O(b * (log n + k))."
  [etlds dom]
  (let [dom (dns/domain dom), dlen (dns/domain-length dom)]
    (ffilter #(not= dlen (dns/domain-length %)) (get etlds dom))))

(defn domain-e2ld
  "Return the effective 2LD, zone, or bailiwick of `dom`. Use `etlds` as the
  set of ETLDs. `etlds` must support associative lookup and is normally a
  `dns/domain-set`; `dom` accepts a string, primitive byte array, existing DNS
  domain, or nil. This is lenient for malformed supported domains and returns
  nil, as it does when no ETLD matches. An unsupported `dom` type throws
  `java.lang.IllegalArgumentException`. For a domain set of `n` entries, `k`
  matching ancestors, and encoded length `b`, lookup is O(b * (log n + k))."
  [etlds dom]
  (let [dom (dns/domain dom), tld (domain-etld etlds dom)]
    (when tld (dns/domain-next dom tld))))

(defn load-network-set
  "Create an `ip/network-set` from the entries in `paths`. Paths are readable
  sources accepted by `clojure.java.io/reader`, or nil. This is strict for
  malformed entries: `ip/network` first converts one to nil, then set
  construction throws `java.lang.IllegalArgumentException` when it attempts to
  convert that nil again. A missing file throws
  `java.io.FileNotFoundException`, other I/O failures throw
  `java.io.IOException`, and an unsupported path throws
  `java.lang.IllegalArgumentException`. Loading `n` valid entries takes O(n^2)
  worst-case time and O(n) result space."
  [& paths]
  (apply load (comp ip/network first) (partial apply ip/network-set) paths))

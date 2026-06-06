(ns inet.data.format.psl
  "Functions for interfacing with Mozilla Public Suffix List format files.

The supported format is an extended version of the PSL format.  There are two
differences:

 - Lines beginning with `#` are considered to be comments in addition to lines
   beginning with `//`.

 - Supports a new \"dynamic\" rule type, indicated by prefixing a domain with a
   `+` character.  Dynamic rules act as per normal rules, unless the lookup
   domain is identical to the suffix domain.  In that that case, the lookup
   falls through to the next matching rule instead of terminating with a null
   result.

See the tests for examples."
  (:refer-clojure :exclude [load])
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [inet.data.dns :as dns])
  (:use [inet.data.util :only [ffilter]]))

(def ^:dynamic *default-psl-url*
  "URL of the default Mozilla Public Suffix List file."
  "https://publicsuffix.org/list/public_suffix_list.dat")

(defn load
  "Load a Mozilla Public Suffix List format file from the Reader `source`.

  The optional `opts` map supports:

   - `:sections` - a set drawn from `#{:icann :private}` selecting which
     sections of the list to include. The PSL is partitioned by the
     `// ===BEGIN ICANN DOMAINS===` and `// ===BEGIN PRIVATE DOMAINS===` marker
     comments; pass `#{:icann}` to ignore the user-contributed PRIVATE section.
     Defaults to both sections (the historical behavior)."
  ([source] (load source nil))
  ([source {:keys [sections] :or {sections #{:icann :private}}}]
   (letfn [(prefix? [^String s1 ^String s2] (.startsWith s2 s1))
           (marker [s]
             (cond (prefix? "// ===BEGIN ICANN" s)   :icann
                   (prefix? "// ===BEGIN PRIVATE" s) :private))
           (ignorable? [s] (or (empty? s) (prefix? "//" s) (prefix? "#" s)))
           (convert [entry n rule]
             (let [prefix (-> entry (subs n) dns/domain)]
               [prefix rule]))
           (parse [entry]
             (condp prefix? entry
               "*." (convert entry 2 :wildcard)
               "!"  (convert entry 1 :exception)
               "."  (convert entry 1 :normal)
               "+"  (convert entry 1 :dynamic)
               ,,,  (convert entry 0 :normal)))
           ;; Track the current section; entries before any marker count as
           ;; ICANN (the list opens with the ICANN section after its license).
           (step [{:keys [section prefixes rules] :as acc} entry]
             (if-let [m (marker entry)]
               (assoc acc :section m)
               (if (or (ignorable? entry) (not (contains? sections section)))
                 acc
                 (let [[prefix rule] (parse entry)]
                   (assoc acc
                          :prefixes (conj prefixes prefix)
                          :rules    (assoc rules prefix rule))))))]
     (let [{:keys [prefixes rules]}
           (->> (line-seq source) (map str/trim)
                (reduce step {:section :icann
                              :prefixes (dns/domain-set), :rules {}}))]
       [prefixes rules]))))

(def ^:private memo-load
  (memoize
   (fn [url]
     (with-open [source (io/reader url)]
       (load source)))))

(defn lookup
  "Determine the E2LD of `domain` as specified by the PSL loaded in `psl`.
Uses the default PSL loaded from `*default-psl-url*` if `psl` is not provided.
Returns `nil` if the domain does not match the provided PSL."
  ([dom] (lookup (memo-load *default-psl-url*) dom))
  ([psl dom]
     (let [dom (dns/domain dom), [prefixes rules] psl,
           matching? (fn [[prefix rule]]
                       (or (not (identical? :dynamic rule))
                           (not= (dns/domain-length dom)
                                 (dns/domain-length prefix)))),
           [prefix rule] (->> dom (get prefixes) (map (juxt identity rules))
                              (ffilter matching?))]
       (when prefix
         (case rule
           :exception         prefix
           (:dynamic :normal) (dns/domain-next dom prefix)
           :wildcard          (second (dns/domain-ancestors dom prefix)))))))

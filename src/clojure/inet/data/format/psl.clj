(ns inet.data.format.psl
  "Functions to use Mozilla Public Suffix List format files.

The supported format extends the PSL format. It has these differences:

- Lines that begin with `#` are comments. Lines that begin with `//` are also
  comments.

- A `+` before a domain indicates a \"dynamic\" rule. Dynamic rules act as
  normal rules unless the lookup domain is identical to the suffix domain. In
  this case, the lookup continues with the next matching rule.

See the tests for examples."
  (:refer-clojure :exclude [load])
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [inet.data.dns :as dns]
            [inet.data.util :refer [ffilter]]))

(set! *warn-on-reflection* true)

(def ^:dynamic *default-psl-url*
  "URL of the default Mozilla Public Suffix List file."
  "https://publicsuffix.org/list/public_suffix_list.dat")

(def ^:dynamic *network-timeout-ms*
  "Default connection and read timeout, in milliseconds, for PSL refreshes."
  10000)

(defn load
  "Load a Mozilla Public Suffix List format file from the Reader `source`.

  The optional `opts` map has this option:

  - `:sections`: A set from `#{:icann :private}`. It selects sections of the
    list to include. The PSL has the `// ===BEGIN ICANN DOMAINS===` and
    `// ===BEGIN PRIVATE DOMAINS===` marker comments. Pass `#{:icann}` to
    ignore the user-contributed PRIVATE section. The default has both sections."
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
           ;; Track the current section. Entries before a marker count as
           ;; ICANN. The list starts with the ICANN section after its license.
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

(def ^:private bundled-resource "effective_tld_names.dat")

(defn- bundled-load []
  (when-let [resource (io/resource bundled-resource)]
    (with-open [source (io/reader resource)]
      (load source))))

(defn- open-url [url timeout-ms]
  (let [^java.net.URLConnection connection (.openConnection (io/as-url url))]
    (.setConnectTimeout connection (int timeout-ms))
    (.setReadTimeout connection (int timeout-ms))
    (io/reader (.getInputStream connection))))

(def ^:private bundled-psl
  (delay (bundled-load)))

(def ^:private psl-cache
  (atom {*default-psl-url* @bundled-psl}))

(defn- memo-load [url timeout-ms]
  (if-let [cached (get @psl-cache url)]
    cached
    (try
      (let [loaded (with-open [^java.io.BufferedReader source (open-url url timeout-ms)]
                     (load source))]
        (swap! psl-cache assoc url loaded)
        loaded)
      (catch Exception _
        (or @bundled-psl)))))

(defn refresh!
  "Refresh the cached default PSL.

  With no arguments, fetch `*default-psl-url*`. An optional map supports
  `:timeout-ms`; a URL and options map can also be supplied for testing or
  applications with more than one PSL. A failed refresh returns the last
  known-good list for that URL, or the bundled snapshot, and leaves the cache
  unchanged."
  ([] (refresh! *default-psl-url* nil))
  ([url-or-opts]
   (if (map? url-or-opts)
     (refresh! *default-psl-url* url-or-opts)
     (refresh! url-or-opts nil)))
  ([url {:keys [timeout-ms] :or {timeout-ms *network-timeout-ms*}}]
   (let [previous (or (get @psl-cache url) @bundled-psl)]
     (try
       (let [loaded (with-open [^java.io.BufferedReader source (open-url url timeout-ms)]
                      (load source))]
         (swap! psl-cache assoc url loaded)
         loaded)
       (catch Exception _ previous)))))

(defn- default-load []
  (or (get @psl-cache *default-psl-url*)
      (let [loaded (memo-load *default-psl-url* *network-timeout-ms*)]
        (swap! psl-cache assoc *default-psl-url* loaded)
        loaded)))

(defn lookup
  "Determine the E2LD of `domain` from the PSL in `psl`. Use the default PSL
from `*default-psl-url*` if `psl` is not supplied. Return `nil` if `domain`
does not match `psl`."
  ([dom] (lookup (default-load) dom))
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

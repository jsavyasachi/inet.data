(ns inet.data.format.psl-test
  (:require [inet.data.format.psl :as psl]
            [inet.data.dns :as dns]
            [clojure.java.io :as io])
  (:use [clojure.test]))

(defn use-local-psl
  [f] (let [psl-url (io/resource "effective_tld_names.dat")]
        (binding [psl/*default-psl-url* psl-url] (f))))

(use-fixtures :once use-local-psl)

(deftest test-psl
  (letfn [(check-psl [dom exp]
            (if (nil? exp)
              (nil? (psl/lookup dom))
              (zero? (dns/domain-compare exp (psl/lookup dom)))))]
    (testing "Tests from http://publicsuffix.org/list/test.txt"
      (testing "NULL input"
        (is (check-psl nil nil)))
      (testing "Mixed case"
        (is (check-psl "COM" nil))
        (is (check-psl "example.COM" "example.com"))
        (is (check-psl "WwW.example.COM" "example.com")))
      (testing "Unlisted TLD"
        (is (check-psl "example" nil))
        (is (check-psl "example.example" nil))
        (is (check-psl "b.example.example" nil))
        (is (check-psl "a.b.example.example" nil)))
      (testing "TLD with only 1 rule"
        (is (check-psl "biz" nil))
        (is (check-psl "domain.biz" "domain.biz"))
        (is (check-psl "b.domain.biz" "domain.biz"))
        (is (check-psl "a.b.domain.biz" "domain.biz")))
      (testing "TLD with some 2-level rules"
        (is (check-psl "com" nil))
        (is (check-psl "example.com" "example.com"))
        (is (check-psl "b.example.com" "example.com"))
        (is (check-psl "a.b.example.com" "example.com"))
        (is (check-psl "uk.com" nil))
        (is (check-psl "example.uk.com" "example.uk.com"))
        (is (check-psl "b.example.uk.com" "example.uk.com"))
        (is (check-psl "a.b.example.uk.com" "example.uk.com"))
        (is (check-psl "test.ac" "test.ac")))
      (testing "TLD with only 1 (wildcard) rule"
        (is (check-psl "cy" nil))
        (is (check-psl "c.cy" nil))
        (is (check-psl "b.c.cy" "b.c.cy"))
        (is (check-psl "a.b.c.cy" "b.c.cy")))
      (testing "More complex TLD"
        (is (check-psl "jp" nil))
        (is (check-psl "test.jp" "test.jp"))
        (is (check-psl "www.test.jp" "test.jp"))
        (is (check-psl "ac.jp" nil))
        (is (check-psl "test.ac.jp" "test.ac.jp"))
        (is (check-psl "www.test.ac.jp" "test.ac.jp"))
        (is (check-psl "kyoto.jp" nil))
        (is (check-psl "c.kyoto.jp" nil))
        (is (check-psl "b.c.kyoto.jp" "b.c.kyoto.jp"))
        (is (check-psl "a.b.c.kyoto.jp" "b.c.kyoto.jp"))
        (is (check-psl "pref.kyoto.jp" "pref.kyoto.jp"))
        (is (check-psl "www.pref.kyoto.jp" "pref.kyoto.jp"))
        (is (check-psl "city.kyoto.jp" "city.kyoto.jp"))
        (is (check-psl "www.city.kyoto.jp" "city.kyoto.jp")))
      (testing "TLD with a wildcard rule and exceptions"
        (is (check-psl "om" nil))
        (is (check-psl "test.om" nil))
        (is (check-psl "b.test.om" "b.test.om"))
        (is (check-psl "a.b.test.om" "b.test.om"))
        (is (check-psl "songfest.om" "songfest.om"))
        (is (check-psl "www.songfest.om" "songfest.om")))
      (testing "US K12"
        (is (check-psl "us" nil))
        (is (check-psl "test.us" "test.us"))
        (is (check-psl "www.test.us" "test.us"))
        (is (check-psl "ak.us" nil))
        (is (check-psl "test.ak.us" "test.ak.us"))
        (is (check-psl "www.test.ak.us" "test.ak.us"))
        (is (check-psl "k12.ak.us" nil))
        (is (check-psl "test.k12.ak.us" "test.k12.ak.us"))
        (is (check-psl "www.test.k12.ak.us" "test.k12.ak.us"))))
    (testing "Additional tests"
      (testing "Dyndns ETLD within gTLD"
        (is (check-psl "org" nil))
        (is (check-psl "dyndns.org" "dyndns.org"))
        (is (check-psl "example.dyndns.org" "example.dyndns.org"))))))

(deftest test-psl-sections
  (let [text  (str "// ===BEGIN ICANN DOMAINS===\n"
                   "com\n"
                   "// ===BEGIN PRIVATE DOMAINS===\n"
                   "blogspot.com\n")
        load* (fn [opts] (psl/load (io/reader (java.io.StringReader. text)) opts))
        e2ld  (fn [psl dom] (psl/lookup psl dom))]
    (testing "With both sections, the lookup uses a PRIVATE suffix"
      ;; blogspot.com is a private suffix. It is the E2LD boundary.
      (is (zero? (dns/domain-compare
                  "foo.blogspot.com" (e2ld (load* nil) "foo.blogspot.com")))))
    (testing "With ICANN only, the lookup ignores the PRIVATE suffix and uses com"
      (is (zero? (dns/domain-compare
                  "blogspot.com"
                  (e2ld (load* {:sections #{:icann}}) "foo.blogspot.com")))))))

(deftest test-bundled-default-does-not-use-network
  (let [network-called (atom false)]
    (with-redefs [psl/open-url (fn [& _] (reset! network-called true))]
      (binding [psl/*default-psl-url* "https://publicsuffix.org/list/public_suffix_list.dat"]
        (is (= "example.com" (str (psl/lookup "www.example.com"))))))
    (is (false? @network-called))))

(deftest test-refresh-updates-the-cache
  (let [url "mock://psl-refresh"
        loaded (io/reader (java.io.StringReader. "com\n"))]
    (with-redefs [psl/open-url (fn [_ _] loaded)]
      (psl/refresh! url))
    (binding [psl/*default-psl-url* url]
      (is (= "example.com" (str (psl/lookup "www.example.com")))))))

(deftest test-refresh-passes-configured-timeout
  (let [timeout (atom nil)
        url "mock://psl-timeout"]
    (with-redefs [psl/open-url (fn [_ timeout-ms]
                                   (reset! timeout timeout-ms)
                                   (io/reader (java.io.StringReader. "com\n")))]
      (psl/refresh! url {:timeout-ms 1234}))
    (is (= 1234 @timeout))))

(deftest test-failed-refresh-keeps-last-known-good-cache
  (let [url "mock://psl-fallback"
        good (io/reader (java.io.StringReader. "com\n"))]
    (with-redefs [psl/open-url (fn [_ _] good)]
      (psl/refresh! url))
    (with-redefs [psl/open-url (fn [_ _] (throw (java.io.IOException. "offline")))]
      (binding [psl/*default-psl-url* url]
        (is (= "example.com" (str (psl/lookup "www.example.com"))))
        (is (= "example.com" (str (psl/lookup (psl/refresh! url) "www.example.com"))))))))

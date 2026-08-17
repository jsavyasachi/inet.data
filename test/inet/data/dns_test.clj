(ns inet.data.dns-test
  (:require [inet.data.dns :as dns])
  (:use [clojure.test]))

(deftest test-strict-domain-coercion-and-operations
  (doseq [[f args]
          [[dns/->domain ["bad..com"]]
           [dns/->domain [nil]]
           [dns/domain-compare ["bad..com" "example.com"]]
           [dns/domain-compare [nil "example.com"]]
           [dns/domain-contains? ["example.com" "bad..com"]]
           [dns/domain-contains? [nil "example.com"]]
           [dns/domain-subdomain? ["example.com" nil]]
           [dns/domain-parent ["bad..com"]]
           [dns/domain-ancestors ["bad..com"]]
           [dns/domain-next ["bad..com"]]
           [dns/domain-next ["example.com" nil]]]]
    (is (thrown? inet.data.dns.DNSDomainException (apply f args))
        (str f " rejects invalid input"))))

(deftest test-strict-domain-operations-accept-direct-values
  (let [domain (dns/domain "example.com")
        bytes (byte-array [3 99 111 109 7 101 120 97 109 112 108 101])]
    (doseq [value [domain bytes "example.com"]]
      (is (dns/domain? (dns/->domain value))))
    (is (zero? (dns/domain-compare domain bytes)))
    (is (dns/domain-contains? domain (dns/domain "www.example.com")))
    (is (= (dns/domain "com") (dns/domain-parent domain)))))

(deftest test-domain?
  (testing "String domains"
    (is (dns/domain? "www.foobar.com") "Accepts valid string domains")
    (is (dns/domain? "this-is-a-valid-domain-even-though-it-has-a-quite-loooong-label.com")
        "Accepts domains with the maximum label length")
    (is (not (dns/domain? "this-is-an-invalid-domain-due-to-having-an-overlong-label-by-one.com"))
        "Rejects domains with overlong labels")
    (is (not (dns/domain? "this-is-an-invalid-domain-because-it-has-an-overlong-label-by-two.com"))
        "Rejects domains with even overlong-er labels"))
  (testing "Byte domains"
    (is (dns/domain? (byte-array (map byte [3 64 64 64 3 99 111 109])))
        "Accepts valid byte domains")
    (is (not (dns/domain? (byte-array (map byte [3 64 64 64 3 99 111]))))
        "Rejects domains which end mid-label")))

(deftest test-domain-roundtrip
  (testing "Round-tripping"
    (is (= "www.example.com" (-> "www.example.com" dns/domain str))
        "Fully ASCII domain names are identical")
    (is (= "www.ExaMple.com" (-> "www.ExaMple.com" dns/domain str))
        "Mixed-case ASCII domain names are identical")
    (is (= "www.xn--exmple-xta.com" (-> "www.exâmple.com" dns/domain str))
        "IDNs are left Punycode-encoded")
    (is (= "www.exâmple.com" (-> "www.exâmple.com" dns/domain dns/idn-str))
        "Explicit IDN string form decodes Punycode")))

(deftest test-domain-compare
  (testing "Domain comparison"
    (testing "Identical domains compare equal"
      (is (= 0 (dns/domain-compare "example.com" "example.com")))
      (is (= 0 (compare (dns/domain "example.com")
                        (dns/domain "example.com")))))
    (let [dom (dns/domain "example.com")]
      (is (not= 0 (dns/domain-compare dom (dns/domain-next dom)))
          "Differing domains do not compare as equal")
      (is (= 0 (dns/domain-compare dom (dns/domain-next dom "com")))
          "Equal derived domains do compare as equal"))
    (testing "Case sensitivity"
      (testing "Case-differing domains compare as equal"
        (is (= 0 (dns/domain-compare "example.com" "eXaMpLe.com")))
        (is (= 0 (compare (dns/domain "example.com")
                          (dns/domain "eXaMpLe.com")))))
      (is (not= (dns/domain "example.com") (dns/domain "eXaMpLe.com"))
          "Case-differing domains are not equal"))))

(deftest test-domain-contains?
  (is (thrown? inet.data.dns.DNSDomainException
               (dns/domain-contains? nil "example.com"))
      "Nil is rejected by strict operation entry points")
  (is (dns/domain-contains? "com" "com") "Domain contains itself")
  (is (dns/domain-contains? "com" "example.com") "TLD contains immediate child")
  (is (dns/domain-contains? "com" "www.example.com") "TLD contains descendent")
  (is (not (dns/domain-contains? "com" "example.net"))
      "TLD does not contain non-descedent")
  (is (not (dns/domain-contains? "example.com" "wwwexample.com"))
      "Domain does not contain purely lexicographic suffix"))

(deftest test-domain-next
  (is (nil? (dns/domain-next "www.example.com" "example.net"))
      "Same-length unrelated parents do not derive children")
  (is (= (dns/domain "www.example.com")
         (dns/domain-next "www.example.com" "example.com"))
      "Genuine parents derive their immediate child"))

(deftest test-domain-subdomain?
  (is (thrown? inet.data.dns.DNSDomainException
               (dns/domain-subdomain? nil "example.com"))
      "Nil is rejected by strict operation entry points")
  (is (not (dns/domain-subdomain? "com" "com"))
      "Domain is not a subdomain of self")
  (is (dns/domain-subdomain? "com" "example.com")
      "TLD has children as subdomains")
  (is (dns/domain-subdomain? "com" "www.example.com")
      "TLD has descendants as subdomains")
  (is (not (dns/domain-subdomain? "com" "example.net"))
      "TLD does not have non-descendants as subdomains")
  (is (not (dns/domain-subdomain? "example.com" "wwwexample.com"))
      "Domain does not have purely lexicographic suffixes as subdomains"))

(deftest test-domain-hostname?
  (is (dns/domain-hostname? "com"))
  (is (dns/domain-hostname? "example.com"))
  (is (not (dns/domain-hostname? "192.168.1.1")))
  (is (not (dns/domain-hostname? "example..com")))
  (is (not (dns/domain-hostname? "-example-.com")))
  (is (not (dns/domain-hostname? "example_underbar.com")))
  (is (dns/domain-hostname? "example_underbar.com" true))
  (is (not (dns/domain-hostname? "example.com/bar"))))

(deftest test-domain-utility
  (is (= '("www" "example" "com") (dns/domain-labels "www.example.com"))
      "Turn domain into sequence of labels.")
  (is (= (-> "www.google.com" dns/domain dns/domain-parent)
         (-> "google.com" dns/domain))
      "Get immediate parent of domain."))

(deftest test-domain-ancestors
  (is (= (map dns/domain ["com" "foo.com" "bar.foo.com"])
         (dns/domain-ancestors "bar.foo.com")))
  (is (thrown? inet.data.dns.DNSDomainException
               (dns/domain-ancestors
                "foo\u002bar\u0000\ufffd\u00da\ufffd\ufffd"))))

(deftest test-domain-set-to-array-jdk-11
  (let [expected [(dns/domain "example.com")
                 (dns/domain "example.net")]
        domains (dns/domain-set "example.com" "example.net")]
    (is (= expected (vec (.toArray domains))))
    (is (= expected (vec (.toArray domains (into-array expected)))))
    (is (= expected (vec domains)))
    (is (= expected (into [] domains)))))

(deftest test-domain-set-edn
  (let [doms (dns/domain-set "example.com" "example.net")]
    (is (= doms (-> doms pr-str read-string)))))

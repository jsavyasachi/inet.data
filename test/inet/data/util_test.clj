(ns inet.data.util-test
  (:require [inet.data.util :as util]
            [clojure.test :refer [deftest is testing]]))

(deftest test-longest-run
  (testing "Returns the first run when the longest runs tie"
    (is (= [2 2] (util/longest-run 0 [1 1 0 0 1 0 0 1]))))
  (testing "Returns a longer later run over an earlier shorter run"
    (is (= [4 3] (util/longest-run 0 [0 1 1 1 0 0 0]))))
  (testing "Returns nil when the value is absent"
    (is (nil? (util/longest-run 0 [1 1 1])))))

(ns inet.data.ip-perf-test
  (:require [clojure.test :refer [deftest is testing]]
            [inet.data.ip :as ip])
  (:import [java.util Random]))

(set! *warn-on-reflection* true)

(defn- synthetic-prefixes
  [n]
  (let [random (Random. 0x4e6574776f726b53)]
    (mapv (fn [_]
            (let [value (.nextInt random 0x1000000)]
              (format "%d.%d.%d.0/24"
                      (bit-shift-right value 16)
                      (bit-and 0xff (bit-shift-right value 8))
                      (bit-and 0xff value))))
          (range n))))

(defn- elapsed-nanos
  [f]
  (let [start (System/nanoTime)]
    (f)
    (- (System/nanoTime) start)))

(defn- median
  [values]
  (nth (sort values) (quot (count values) 2)))

(defn- benchmark
  [f]
  (f)
  (median (repeatedly 3 #(elapsed-nanos f))))

(deftest ^:integration aggregate-networks-large-input-benchmark
  (testing "50k-prefix aggregation and network-set construction scale reasonably"
    (let [all-input (synthetic-prefixes 50000)
          small-input (subvec all-input 0 25000)
          large-input all-input
          small-networks (mapv ip/network small-input)
          large-networks (mapv ip/network large-input)
          aggregate-small (benchmark #(ip/aggregate-networks small-input))
          aggregate-large (benchmark #(ip/aggregate-networks large-input))
          network-set-small (benchmark #(apply ip/network-set small-networks))
          network-set-large (benchmark #(apply ip/network-set large-networks))
          aggregate-scale (/ (double aggregate-large) aggregate-small)
          network-set-scale (/ (double network-set-large) network-set-small)]
      (println (format "ip perf: aggregate 25k=%d ns 50k=%d ns scale=%.2fx; network-set 25k=%d ns 50k=%d ns scale=%.2fx"
                       aggregate-small aggregate-large aggregate-scale
                       network-set-small network-set-large network-set-scale))
      (is (= 50000 (count large-input)))
      (is (<= aggregate-scale 8.0)
          (str "aggregate-networks scaling exceeded 8x: " aggregate-scale))
      (is (<= network-set-scale 8.0)
          (str "network-set scaling exceeded 8x: " network-set-scale)))))

(ns com.structmap.spatial.geohash-test
    (:use clojure.repl clojure.pprint)
    (:require [clojure.test :refer :all]
              [com.structmap.spatial.geohash :refer :all]
              [clojure.test.check :as tc]
              [clojure.test.check.generators :as gen]
              [clojure.test.check.properties :as prop]
              [clojure.test.check.clojure-test :refer [defspec]]))

(deftest denmark
  (is (= (encode 11 {:lat 57.64911 :lon 10.40744}) "u4pruydqqvj")))

(defspec roundtrip
  (prop/for-all
   [n    (gen/large-integer* {:min 1 :max 30})
    lat (gen/one-of [(gen/large-integer* {:min -90 :max 90})
                     (gen/double* {:min -90 :max 90 :NaN? false :infinite? false})])
    lon (gen/one-of [(gen/large-integer* {:min -180 :max 180})
                     (gen/double* {:min -180 :max 180 :NaN? false :infinite? false})])]
   (let [geohashed (encode n {:lat lat :lon lon})
         ;_ (println geohashed)
         {:keys [left right bottom top]} (decode geohashed)]
     (and (<= left lon right)
          (<= bottom lat top)))))

; (run-tests)

nil


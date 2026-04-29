(ns com.structmap.spatial.geohash
    (:use clojure.repl clojure.pprint)
    (:require [clojure.clr.io :as io]
              [clojure.set :as set]
              [clojure.string :as string]))

; GeoHash decode and encode library translated from Python script https://gist.github.com/yuribossa/294599

(def base32
  {\0 "00000"
   \1 "00001"
   \2 "00010"
   \3 "00011"
   \4 "00100"
   \5 "00101"
   \6 "00110"
   \7 "00111"
   \8 "01000"
   \9 "01001"
   \b "01010"
   \c "01011"
   \d "01100"
   \e "01101"
   \f "01110"
   \g "01111"
   \h "10000"
   \j "10001"
   \k "10010"
   \m "10011"
   \n "10100"
   \p "10101"
   \q "10110"
   \r "10111"
   \s "11000"
   \t "11001"
   \u "11010"
   \v "11011"
   \w "11100"
   \x "11101"
   \y "11110"
   \z "11111"})

(defn decode [geohash_str]
  (let [dehash
        (fn [bit_array min max] ; 緯度経度の範囲を求める
          (loop [bs bit_array
                 mn min
                 mx max]
            (if-not (empty? bs)
                    (if (first bs)
                      (recur (rest bs) (* 0.5 (+ mn mx)) mx)
                      (recur (rest bs) mn (* 0.5 (+ mn mx))))
                    [mn mx])))
        ; 二進数文字列に変換する
        bit_array            (mapv base32 geohash_str)
        ; 偶数bit
        longtitude_bit_array (atom [])
        ; 奇数bit
        latitude_bit_array   (atom [])]

    ; 偶数bitと奇数bitに分割する
    (loop [i  0
           bs bit_array]
      (when-let [b (first bs)]
        (if (zero? (mod i 2))
          (do
            (swap! longtitude_bit_array conj (get b 0))
            (swap! latitude_bit_array conj (get b 1))
            (swap! longtitude_bit_array conj (get b 2))
            (swap! latitude_bit_array conj (get b 3))
            (swap! longtitude_bit_array conj (get b 4)))
          (do
            (swap! latitude_bit_array conj (get b 0))
            (swap! longtitude_bit_array conj (get b 1))
            (swap! latitude_bit_array conj (get b 2))
            (swap! longtitude_bit_array conj (get b 3))
            (swap! latitude_bit_array conj (get b 4))))
        (recur (inc i) (rest bs))))

    (merge
     (zipmap [:bottom :top] (dehash (map {\0 false \1 true} @latitude_bit_array) -90.0 90.0))
     (zipmap [:left :right]
             (dehash (map {\0 false \1 true} @longtitude_bit_array) -180.0 180.0)))))

;(def ret (future (decode "u4pruydqqvj")))
;
;(mapv base32 (map str "u4pruydqqvj"))
;
;(realized? ret)
;
;(println (clojure.data.json/write-str (com.structmap.util/geojson-rectangle @ret)))

(defn encode [str_len {latitude :lat longtitude :lon}]
  (let [as_bit
        (fn [v bit_len min max] ; 緯度または経度の二進数文字列を求める
          (let [bit_array (atom [])]
            (loop [i  0
                   mn min
                   mx max]
              (when (< i bit_len)
                    (let [mid (* 0.5 (+ mn mx))]
                      (swap! bit_array conj (< mid v))
                      (if (< mid v)
                        (recur (inc i) mid mx)
                        (recur (inc i) mn mid)))))
            @bit_array))

        ; 緯度と経度の二進数文字列を求める
        lat_len              (* 0.5 5 str_len)
        lng_len              (+ (* 0.5 5 str_len) (mod str_len 2))

        latitude_bit_array   (as_bit latitude lat_len -90.0 90.0)
        longtitude_bit_array (as_bit longtitude lng_len -180.0 180.0)

        ; 5桁ごとにまとめる
        mixed_bit_array      (atom [])
        bit_array            (atom [])]
    (loop [i    0
           lons longtitude_bit_array
           lats latitude_bit_array]
      (when (< i (* str_len 5))
            (if (zero? (mod i 2))
              (swap! mixed_bit_array conj (first lons))
              (swap! mixed_bit_array conj (first lats)))
            (when (zero? (mod (inc i) 5))
                  (swap! bit_array conj (apply str (map {true \1 false \0} @mixed_bit_array)))
                  (swap! mixed_bit_array empty))
            (if (zero? (mod i 2))
              (recur (inc i) (rest lons) lats)
              (recur (inc i) lons (rest lats)))))

    ; Base32に変換する
    (apply str (map (set/map-invert base32) @bit_array))))

;(def ret (future (encode 11 {:lat 57.64911 :lon 10.40744})))
;
;(realized? ret)
;
;(pprint @ret)

nil

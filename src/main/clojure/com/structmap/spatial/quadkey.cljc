(ns com.structmap.spatial.quadkey
    (:use clojure.repl clojure.pprint)
    (:require [clojure.clr.io :as io]
              [clojure.string :as string]))

; quadkey translated from C# https://learn.microsoft.com/en-us/bingmaps/articles/bing-maps-tile-system

; To optimize the indexing and storage of tiles, the two-dimensional tile XY coordinates are combined into
; one-dimensional strings called quadtree keys, or “quadkeys” for short. Each quadkey uniquely identifies
; a single tile at a particular level of detail, and it can be used as an key in common database B-tree
; indexes. To convert tile coordinates into a quadkey, the bits of the Y and X coordinates are interleaved,
; and the result is interpreted as a base-4 number (with leading zeros maintained) and converted into a string.

;; port of TileXYToQuadKey from the C# example code
(defn quadkey [{:keys [x y z]}]
      (let [qk (new StringBuilder)]
        (loop [i z]
          (when (pos? i)
                (let [mask (bit-shift-left 1 (- i 1))
                      digit (+ 0 (if-not (zero? (bit-and x mask)) 1 0)
                                 (if-not (zero? (bit-and y mask)) 2 0))]
                  (.Append qk (char (+ 48 digit)))
                  (recur (dec i)))))
        (.ToString qk)))

; (quadkey {:x 2 :y 2 :z 3})

;; port QuadKeyToTileXY from the C# example code
(defn tile [qk]
  (let [z (count qk)]
    (loop [x 0 y 0 i z]
      (if (zero? i)
        {:x x :y y :z z}
        (let [mask (bit-shift-left 1 (- i 1))]
          (case (nth qk (- z i))
                \0 (recur x y (dec i))
                \1 (recur (bit-or x mask) y (dec i))
                \2 (recur x (bit-or y mask) (dec i))
                \3 (recur (bit-or x mask) (bit-or y mask) (dec i))))))))

;(let [input {:x 3 :y 3 :z 7}]
;  (println (= input (tile (quadkey input)))))

;(defn read-base [s n]
;      (->> (reverse s)
;          (map-indexed #(* (math/pow n %1) (- %2 48)))
;          (reduce +)
;          (int)))

;(Convert/ToInt32 "100111" 8)
;(read-base "100111" 8)
;(Convert/ToInt32 "100111" 16)
;(read-base "100111" 16)

nil

(ns com.structmap.globalmercator
    (:require [clojure.math :as math]
              [clojure.string :as string]))

;;; based on https://gist.github.com/geobabbler/9120108

;/*
;    GlobalMercator.cs
;    Copyright (c) 2014 Bill Dollins. All rights reserved.
;    http://blog.geomusings.com
;*************************************************************
;	Based on GlobalMapTiles.js - part of Aggregate Map Tools
;	Version 1.0
;	Copyright (c) 2009 The Bivings Group
;	All rights reserved.
;	Author: John Bafford
;
;	http://www.bivings.com/
;	http://bafford.com/softare/aggregate-map-tools/
;*************************************************************
;	Based on GDAL2Tiles / globalmaptiles.py
;	Original python version Copyright (c) 2008 Klokan Petr Pridal. All rights reserved.
;	http://www.klokan.cz/projects/gdal2tiles/
;
;	Permission is hereby granted, free of charge, to any person obtaining a
;	copy of this software and associated documentation files (the "Software"),
;	to deal in the Software without restriction, including without limitation
;	the rights to use, copy, modify, merge, publish, distribute, sublicense,
;	and/or sell copies of the Software, and to permit persons to whom the
;	Software is furnished to do so, subject to the following conditions:
;
;	The above copyright notice and this permission notice shall be included
;	in all copies or substantial portions of the Software.
;
;	THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
;	OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;	FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;	THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;	LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;	FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;	DEALINGS IN THE SOFTWARE.
;*/

(def equatorialRadius 6378137)
(def tileSize 256)
(def initialResolution (* 2 math/PI equatorialRadius (/ tileSize)))
(def originShift (* 2 math/PI equatorialRadius (/ 2)))

(defn resolution [z]
  (/ initialResolution (bit-shift-left 1 z)))

(defn pixels-to-meters [px py z]
  (let [originShift (* 2 math/PI equatorialRadius (/ 2))
        res         (resolution z)]
    [(- (* px res) originShift) (- (* py res) originShift)]))

(defn tile->quad [{:keys [z x y]}]
  "clockwise perimeter of rectangular tile"
  (let [srs       "EPSG:3857"
        y'        (Convert/ToInt32 (- (- (Math/Pow 2 z) 1) y)) ; fix for Google Maps compability
        tile-size tileSize
        [w s]     (pixels-to-meters (* x tile-size) (* y' tile-size) z)
        [e n]     (pixels-to-meters (* (inc x) tile-size) (* (inc y') tile-size) z)]
    [{:x w :y n :srs srs}
     {:x e :y n :srs srs}
     {:x e :y s :srs srs}
      {:x w :y s :srs srs}]))

;(->> (tile->quad {:z 18 :x 61669 :y 106860}) (map #(format "%f %f" (:x %) (:y %))) (string/join ",") (format "POLYGON((%s))") println)

;(->> (tile->quad {:z 21 :x 475655 :y 867868}) (map #(format "%f %f" (:x %) (:y %))) (string/join ",") (format "POLYGON((%s))") println)
;(->> (tile->quad {:z 16 :x 14863 :y 27120}) (map #(format "%f %f" (:x %) (:y %))) (string/join ",") (format "POLYGON((%s))") println)
;(->> (tile->quad {:z 16 :x 14864 :y 27120}) (map #(format "%f %f" (:x %) (:y %))) (string/join ",") (format "POLYGON((%s))") println)
;(->> (tile->quad {:z 16 :x 14863 :y 27121}) (map #(format "%f %f" (:x %) (:y %))) (string/join ",") (format "POLYGON((%s))") println)
;(->> (tile->quad {:z 16 :x 14864 :y 27121}) (map #(format "%f %f" (:x %) (:y %))) (string/join ",") (format "POLYGON((%s))") println)
;GEOMETRYCOLLECTION(POLYGON((-10948094.670543 3453195.626839,-10948075.561286 3453195.626839,-10948075.561286 3453176.517582,-10948094.670543 3453176.517582)) , POLYGON((-10947822.945742588 3452988.9792680084, -10948238.580319848 3453018.817397532, -10948215.739719497 3453262.567520862, -10947784.25293453 3453237.3837571493, -10947822.945742588 3452988.9792680084)), POLYGON((-10948839.931569 3453730.686037,-10948228.435342 3453730.686037,-10948228.435342 3453119.189811,-10948839.931569 3453119.189811)) ,POLYGON((-10948228.435342 3453730.686037,-10947616.939116 3453730.686037,-10947616.939116 3453119.189811,-10948228.435342 3453119.189811)), POLYGON((-10948839.931569 3453119.189811,-10948228.435342 3453119.189811,-10948228.435342 3452507.693585,-10948839.931569 3452507.693585)), POLYGON((-10948228.435342 3453119.189811,-10947616.939116 3453119.189811,-10947616.939116 3452507.693585,-10948228.435342 3452507.693585)))

(defn tile->centroid [{:keys [z x y]}]
  "center of rectangular tile"
  (let [srs       "EPSG:3857"
        y'        (Convert/ToInt32 (- (- (Math/Pow 2 z) 1) y)) ; fix for Google Maps compability
        tile-size tileSize
        [e n]     (pixels-to-meters (* (+ x 0.5) tile-size) (* (+ y' 0.5) tile-size) z)]
    {:x e :y n :srs srs}))

;https://a.tile.openstreetmap.org/18/78101/97462.png
;
;(->> (tile->quad {:z 20 :x 79933312 :y 99787264}) first ((juxt :x :y)) (apply format "POINT (%f %f)") println)
;(->> (tile->quad {:z 18 :x 78101 :y 97462}) first ((juxt :x :y)) (apply format "POINT (%f %f)") println)
;(->> (tile->quad {:z 20 :x 312405 :y 389848}) first ((juxt :x :y)) (apply format "POINT (%f %f)") println)

(comment

 (clojure.pprint/pprint
  (binding
    [com.structmap.proj/*ctx*
     (com.structmap.proj/proj_context_create)]
    (mapv (partial com.structmap.proj/convert "EPSG:4326")
          (tile->quad {:z 8 :x 64 :y 97}))))

 [{:lat 39.90973623453719, :lon -90.0}
  {:lat 39.90973623453719, :lon -88.59375}
  {:lat 38.822590976177096, :lon -88.59375}
  {:lat 38.822590976177096, :lon -90.0}]

)

nil


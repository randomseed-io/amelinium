(ns

    ^{:doc    "Basic spatial location support for amelinium."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.location

  (:refer-clojure :exclude [parse-long uuid random-uuid type value])

  (:require [clojure.set                  :as         set]
            [clojure.string               :as         str]
            [io.randomseed.lazy-map       :as    lazy-map]
            [amelinium.proto.location     :as           p]
            [amelinium.types.location     :refer     :all]
            [amelinium                    :refer     :all]
            [amelinium.logging            :as         log]
            [geo.io                       :as         gio]
            [geo.jts                      :as          gj]
            [geo.spatial                  :as          gs]
            [io.randomseed.utils.db       :as         rdb]
            [io.randomseed.utils.map      :as         map]
            [io.randomseed.utils.map      :refer [qassoc]]
            [io.randomseed.utils          :refer     :all])

  (:import  (clojure.lang                 Symbol
                                          Keyword
                                          Associative
                                          PersistentVector
                                          IPersistentMap)
            (org.locationtech.jts.geom    Point
                                          CoordinateXY)
            (amelinium                    Location)
            (amelinium.proto.location     Spatial)
            (io.randomseed.lazy_map       LazyMap)))

(def ^:dynamic *default-srid* 4326)

(defn- parse-srid
  ^long [s]
  (or (parse-long s) *default-srid*))

;; 135 in total
;; dpo: 77
;; automatej: 47
;; gspm: 1
;; stargate-exporter: 10

(defn- string->point
  ^String [^String s]
  (if-some [^String s (some-str s)]
    (if-some [m (re-matches #"\s*(\d+(?:\.\d+)?)\s*[,\s]\s*(\d+(?:\.\d+)?)\s*" s)]
      (str "POINT(" (nth m 1) " " (nth m 2) ")")
      s)))

(defn- wkt-string->location
  (^Location [^String s]
   (wkt-string->location s nil))
  (^Location [^String s srid]
   (if-some [^String s (string->point s)]
     (if-some [wkt (gio/read-wkt s)]
       (if (instance? Point wkt)
         (Location. (.getX ^Point wkt) (.getY ^Point wkt) (or srid *default-srid*)))))))

(extend-protocol p/Spatial

  (class (byte-array 0))

  (srid
    ^long [b]
    ())


  String

  (srid
    (^long [s] (long s))
    (^long [x y] *default-srid*)
    (^long [x y s] (parse-srid s)))

  Number

  (make-location
    (^Location [x y]
     (if-some [^BigDecimal x (bigdec x)]
       (if-some [^BigDecimal y (bigdec y)]
         (Location. x y *default-srid*))))
    (^Location [x y s]
     (if-some [^BigDecimal x (bigdec x)]
       (if-some [^BigDecimal y (bigdec y)]
         (Location. x y (parse-srid s))))))

  (srid
    (^long [s] (long s))
    (^long [x y] *default-srid*)
    (^long [x y s] (parse-srid s)))

  (x
    (^BigDecimal [x] (bigdec x))
    (^BigDecimal [x y] (bigdec x))
    (^BigDecimal [x y s] (bigdec x)))

  (y
    (^BigDecimal [y] (bigdec y))
    (^BigDecimal [x y] (bigdec y))
    (^BigDecimal [x y s] (bigdec y)))

  Point

  (make-location
    (^Location [p]   (Location. (.getX ^Point p) (.getY ^Point p) *default-srid*))
    (^Location [p s] (Location. (.getX ^Point p) (.getY ^Point p)  (parse-srid s))))

  (srid
    (^long [p] (or (gj/get-srid p) *default-srid*))
    (^long [p s] (parse-srid s)))

  (x
    (^BigDecimal [p]   (bigdec (.getX ^Point p)))
    (^BigDecimal [p s] (bigdec (.getX ^Point p)))) ;; todo: set SRID

  (y
    (^BigDecimal [y]   (bigdec y))
    (^BigDecimal [x y] (bigdec y)))

  Location

  (make-location
    (^Location [l] l)
    (^Location [l s] (Location. (.X ^Location l) (.Y ^Location l)  (parse-srid s))))

  (srid
    (^long [l] (.SRID ^Location l))
    (^long [l s] (parse-srid s)))

  (x
    (^BigDecimal [l] (.X ^Location l))
    (^BigDecimal [l s] (.X ^Location l)))

  (y
    (^BigDecimal [l] (.Y ^Location l))
    (^BigDecimal [l s] (.Y ^Location l)))

  nil

  (make-location
    ([l] nil)
    ([x y] nil)
    ([x y s] nil))

  (srid
    ([l] nil)
    ([x y] nil)
    ([x y s] nil)))

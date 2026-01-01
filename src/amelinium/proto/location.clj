(ns

    ^{:doc    "amelinium service, location-related protocols and functions."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.proto.location

  (:require [amelinium]
            [amelinium.utils          :refer :all]
            [amelinium.types.location :refer :all])

  (:import  (amelinium Location)))

(defprotocol Spatial
  "This protocol allows to extend known location types."

  (^{:tag 'long}
   srid
   [l] [x y] [x y srid]
   "Returns a long number describing SRID for the given spatial data.")

  (^{:tag BigDecimal}
   x
   [l] [x y] [x y srid]
   "Returns a `BigDecimal` number being the X coordinate of the given spatial data.")

  (^{:tag BigDecimal}
   y
   [l] [x y] [x y srid]
   "Returns a `BigDecimal` number being the Y coordinate of the given spatial data.")

  (^{:tag Location}
   make-location
   [l] [x y] [x y srid]
   "Creates a new `amelinium.Location` record on a basis of `x` and `y` with optional `srid`".))

(ns

    ^{:doc    "amelinium service, location record type."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.types.location

  (:require [amelinium]))

(in-ns 'amelinium.location)

(clojure.core/declare to-str)

(in-ns 'amelinium)

(defrecord Location [^BigDecimal X
                     ^BigDecimal Y
                     ^long       SRID]
  Object
  (toString ^String [^Location l] (amelinium.location/to-str ^Location l)))

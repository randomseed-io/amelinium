(ns

    ^{:doc    "amelinium service, identity record type."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.types.identity

  (:require [amelinium]))

(in-ns 'amelinium.identity)

(clojure.core/declare ->str)

(in-ns 'amelinium)

(defrecord Identity [^clojure.lang.Keyword id-type value]
  Object
  (toString ^String [^Identity i] (amelinium.identity/->str ^Identity i)))

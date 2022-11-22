(ns

    ^{:doc    "amelinium service, error-related protocol."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.proto.errors

  (:require [amelinium.types.errors])
  (:import  [amelinium.types.errors ErrorsConfig]))

(defprotocol ErrorsConfigurable
  (^{:tag ErrorsConfig} config [src]))


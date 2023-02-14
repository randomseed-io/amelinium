(ns

    ^{:doc    "amelinium service, error-related protocol."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.proto.errors

  (:require [amelinium] [amelinium.types.errors])
  (:import  [amelinium ErrorsConfig]))

(defprotocol ErrorsConfigurable
  (^{:tag ErrorsConfig} config [src])
  (^{:tag Boolean} configurable? [src]))

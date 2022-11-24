(ns

    ^{:doc    "amelinium service, error record types."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.types.errors

  (:require [amelinium]))

(in-ns 'amelinium)

(defrecord ErrorsConfig [priorities
                         responses
                         default-response])

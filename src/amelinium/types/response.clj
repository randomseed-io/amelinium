(ns

    ^{:doc    "amelinium service, response record type."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.types.response

  (:require [amelinium]))

(in-ns 'amelinium)

(defrecord Response [^long                        status
                     ^clojure.lang.IPersistentMap headers
                     body])

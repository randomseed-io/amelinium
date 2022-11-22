(ns

    ^{:doc    "amelinium service, error record types."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.types.errors)

(defrecord ErrorsConfig [priorities responses default-response])

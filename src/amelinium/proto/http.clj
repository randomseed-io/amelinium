(ns

    ^{:doc    "amelinium service, HTTP protocol."
      :author "Paweł Wilk"
      :added  "1.0.1"}

    amelinium.proto.http

  (:require [amelinium]
            [amelinium.types.errors]))

(defprotocol HTTP
  (^{:tag Boolean}        request?            [src])
  (^{:tag Boolean}        response?           [src])
  (^{:tag IPersistentMap} response-headers    [src])
  (^{:tag Long}           response-status     [src])
  (                       response-body       [src])
  (                       app-status          [src])
  (                       response-location   [src] [src f] [src f args]))

(ns

    ^{:doc    "Messaging protocol for Amelinium."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.proto.messaging

  (:import (clojure.lang IPersistentMap
                         IPersistentSet)))

(defprotocol Messaging
  (^{:tag IPersistentMap} provider      [driver])
  (^{:tag IPersistentSet} capabilities  [driver])
  (^{:tag IPersistentMap} response-stub [driver])
  (^{:tag IPersistentMap} send!         [driver ^IPersistentMap msg])
  (^{:tag nil}            send-async!   [driver ^IPersistentMap msg ^IPersistentMap opts]))

(extend-protocol Messaging
  nil
  (provider      [_]   nil)
  (capabilities  [_]   nil)
  (response-stub [_]   nil)
  (send!         [_ _] nil)
  (send-async!  ([_ _] nil) ([_ _ _] nil)))

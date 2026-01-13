(ns

    ^{:doc    "Messaging for Amelinium."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.messaging

  (:require [amelinium.proto.messaging :as p])

  (:import (amelinium.proto.messaging Messaging)
           (clojure.lang              IFn
                                      IPersistentMap
                                      IPersistentSet)))

(defn send!
  [^Messaging driver ^IPersistentMap msg]
  (p/send! driver msg))

(defn send-async!
  (^IPersistentMap [^Messaging      driver
                    ^IPersistentMap msg
                    ^IPersistentMap opts]
   (p/send-async! driver msg opts))
  (^IPersistentMap [^Messaging      driver
                    ^IPersistentMap msg
                    ^IFn            on-ok
                    ^IFn            on-err]
   (p/send-async! driver msg {:on-ok on-ok, :on-err on-err})))

(defn capabilities
  ^IPersistentSet [^Messaging driver]
  (p/capabilities driver))

(defn provider
  ^IPersistentMap [^Messaging driver]
  (p/provider driver))

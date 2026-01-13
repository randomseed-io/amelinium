(ns

    ^{:doc    "amelinium service, Twilio client record types."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.types.twilio

  (:require [amelinium]))

(in-ns 'amelinium)

(import [amelinium.proto.twilio TwilioControl]
        [clojure.lang           IPersistentMap IPersistentSet Keyword])

(defrecord TwilioConfig
    [^String                        url
     ^String                        raw-url
     ^String                        account-sid
     ^String                        account-key
     ^String                        account-token
     ^String                        api-sid
     ^String                        api-key
     ^String                        api-token
     ^String                        auth-pub
     ^String                        auth-key
     ^String                        auth-tok
     ^String                        service-sid
     ^String                        service
     ^String                        username
     ^String                        password
     ^clojure.lang.Keyword          accept
     ^clojure.lang.IPersistentMap   parameters
     ^clojure.lang.IPersistentMap   client-opts
     ^clojure.lang.IPersistentMap   request-opts
     ^clojure.lang.IPersistentMap   localized-templates
     ^Boolean                       prepared?
     ^Boolean                       enabled?])

(defrecord TwilioEmailMessaging
    [^TwilioControl  control
     ^IPersistentSet capabilities
     ^IPersistentMap provider
     ^IPersistentMap response-stub
     ^Keyword        channel-type])

(defrecord TwilioSMSMessaging
    [^TwilioControl  control
     ^IPersistentSet capabilities
     ^IPersistentMap provider
     ^IPersistentMap response-stub
     ^Keyword        channel-type])

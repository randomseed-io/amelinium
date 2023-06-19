(ns

    ^{:doc    "amelinium service, Twilio client protocols."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.proto.twilio

  (:refer-clojure :exclude [empty empty?])

  (:require [amelinium] [amelinium.types.twilio])
  (:import  (amelinium TwilioConfig)))

(defprotocol TwilioControl
  "This protocol promises access to Twilio client configuration data and basic actions
  which are configuration-dependent. The operations should keep access to settings
  and/or dynamically generated functions using lexical closures. Therefore, this
  protocol should later be reified, after settings are parsed."

  (^{:tag TwilioConfig}
   config
   [c]
   "Gets a Twilio client configuration settings.")

  (^{:tag TwilioConfig}
   request
   [c] [c params] [c opts params] [c opts params respond] [c opts params respond raise]
   "Performs a Twilio request.")

  (^{:tag String}
   get-template-id
   [c template-group lang fallback-template]
   "Gets template ID for the given `template-group` and `lang` using current
   configuration settings. The `fallback-template` will be used when no template can
   be identified."))

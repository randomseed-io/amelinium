(ns

    ^{:doc    "amelinium service, validators."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.http.validators

  (:refer-clojure :exclude [uuid random-uuid parse-long])

  (:require [amelinium.logging              :as     log]
            [amelinium.system               :as  system]
            [io.randomseed.utils.validators :as       v]
            [io.randomseed.utils.var        :as     var]
            [io.randomseed.utils            :refer :all]))

;; Default validation strategy.
;; If `true` then parameters without validators assigned are considered valid.
;; If `false` then unknown parameters are causing validation to fail.

(def ^:const default-pass?   false)

;; Required parameters checker.
;; If set to `true` then the `required-params` is used (at least 1 must be present).

(def ^:const check-required?  true)

;; Validation map.

(def ^:const common
  {"login"            #"|(^[a-zA-Z0-9_\.+\-]{1,64}@[a-zA-Z0-9\-]{1,64}\.[a-zA-Z0-9\-\.]{1,128}$)"
   "password"         #"|.{5,256}"
   "session-id"       #"|[a-f0-9]{30,128}"})

;; Required parameters which must have empty values.
;; At least 1 of them must be present.

(def ^:const must-be-empty
  ["session-challenge" "weblock-id" "goto-challenge-phrase"
   "mailto-response" "secret-token" "totem-potem" "name-middle" "x-ax"
   "jolt-abyss" "krzyz-panski" "param-fill-15-38" "form-data-02" "do-re-mi-fa" "tokidi"
   "form-token-03" "aks3-dk91-f9ff-0c10f" "primary-validator" "second-title" "bcsk-id"
   "transforming-the-tunes" "gulasz-leszlasz" "primary-wikary" "mp3-jp2"
   "ex-in-tid" "x-aix" "x-accx" "secondary-rsp-token" "captcha-xid"])

(def ^:const required-params
  (when check-required?
    (set must-be-empty)))

(def ^:const default-vmap
  (apply assoc common (interleave must-be-empty (repeat ""))))

(defn validate
  ([]                          true)
  ([m]                         (v/validate m default-vmap default-pass? required-params))
  ([m vmap]                    (v/validate m vmap default-pass? required-params))
  ([m vmap d-pass?]            (v/validate m vmap d-pass? required-params))
  ([m vmap d-pass? req-params] (v/validate m vmap d-pass? req-params)))

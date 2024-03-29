(ns

    ^{:doc    "API helpers for amelinium."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.api

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:require [clojure.string                       :as             str]
            [potemkin.namespaces                  :as               p]
            [lazy-map.core                        :as        lazy-map]
            [ring.util.response]
            [amelinium.http.response              :as            resp]
            [ring.util.request                    :as             req]
            [amelinium.utils                      :refer         :all]
            [amelinium.common                     :as          common]
            [amelinium.errors                     :as          errors]
            [amelinium.i18n                       :as            i18n]
            [amelinium.http.middleware.validators :as      validators]
            [amelinium.http.middleware.session    :as         session]
            [amelinium.types.response             :refer         :all]
            [amelinium                            :refer         :all]
            [io.randomseed.utils.map              :refer    [qassoc
                                                             qupdate]]
            [io.randomseed.utils                  :refer         :all])

  (:import (amelinium     Response)
           (clojure.lang  IFn)
           (reitit.core   Match)
           (lazy_map.core LazyMapEntry
                          LazyMap)))

;; Operations logging

(p/import-vars [amelinium.common
                oplog-config oplog-logger oplog-logger-populated oplog])

;; Routing data and settings helpers

(p/import-vars [amelinium.common
                router-match? on-page? lang-param guess-lang-param
                login-page? auth-page? login-auth-state])

;; Path parsing

(p/import-vars [amelinium.common
                path-variants path-param path-params path-language
                split-query-params-simple split-query-params has-param?
                req-param-path path-template-with-param template-path
                parameterized-page parameterized-page-core
                page localized-page strictly-localized-page
                current-page current-page-id current-page-id-or-path login-page auth-page
                temporary-redirect localized-temporary-redirect
                move-to see-other localized-see-other go-to])

;; Special redirects

(p/import-vars [amelinium.common
                add-slash slash-redir lang-redir])

;; Accounts

(p/import-vars [amelinium.common
                lock-wait-default lock-wait
                hard-lock-time hard-locked?
                soft-lock-time soft-lock-passed soft-locked? soft-lock-remains])

;; Context and roles

(p/import-vars [amelinium.common
                has-any-role? has-role?
                role-required! with-role-only!
                roles-for-context roles-for-contexts default-contexts-labeler
                roles-matrix roles-tabler])

;; Language helpers

(p/import-vars [amelinium.common
                lang-id lang-str lang-config])

;; Response rendering

(p/import-vars [amelinium.common
                add-header add-headers add-status remove-status])

(defn render
  "Returns response body on a basis of a value associated with the `:response/body`
  key of the `req`.

  If `status` is given and `:response/body` is a map, it adds the following
  associations to it: `:status` (with a keyword describing status as value),
  `:status/title` (with a string describing status translated using a detected
  language) and `:status/description` (with a longer string explaining the status,
  based on a translation key created by adding \".full\" to the original status
  key). If any key already exists in `:response/body`, it will not be added.

  If `status` is given and the `:response/body` does not exist in `req`, it adds it
  and associates it with an empty, performing the calculations described above.

  Additionally, if the body map contains `:status/title` or `:status/description` key
  and does not contain `:lang` key, the `:lang` key will be added with an associated
  value of `:language/id` taken from a request map `req`."
  ([]
   (render nil))
  ([req]
   (let [body (get req :response/body)]
     (if (map? body)
       body
       (if (sequential? body)
         (seq body)
         body))))
  ([req status]
   (let [body (get req :response/body {})]
     (if (map? body)
       (if (contains? body :status)
         (common/add-missing-lang body req [:status/title :status/description])
         (if (common/untranslatable? status)
           (-> body
               (qassoc :status status)
               (common/add-missing-lang req [:status/title :status/description]))
           (let [tr-sub (i18n/no-default (common/translator-sub req))]
             (-> body
                 (qassoc :status status)
                 (common/add-missing-translation :status/title status tr-sub)
                 (common/add-missing-translation :status/description status ".full" tr-sub)
                 (common/add-missing-lang req [:status/title :status/description])))))
       (if (sequential? body)
         (seq body)
         body)))))

(defn render-response
  "API response renderer. Uses the `render` function to render the response
  body (unless the `req` is already a valid response - in such case it is returned
  as-is) and `resp-fn` function to construct the response map.

  If `status` is given and `:response/body` is a map, it adds two associations to it:
  `:status` (with a keyword describing HTTP status as value) and `:message` (with a
  string describing HTTP status translated using a detected language). If any key
  already exists in `:response/body`, it will not be added.

  Additionally, if the body map contains `:message` or `message/sub` key and does not
  contain `:lang` key, it will be added with a value of `:language/id` taken from a
  request map `req`."
  (^Response []
   (render-response resp/ok nil))
  (^Response [^IFn resp-fn]
   (render-response resp-fn nil))
  (^Response [^IFn resp-fn req]
   (if (resp/response? req)
     req
     (resp-fn (render req) (or (resp/headers req) {}))))
  (^Response [^IFn resp-fn status req]
   (if (resp/response? req)
     req
     (resp-fn (render req status) (or (resp/headers req) {})))))

(defn render-response-force
  "API response renderer. Uses the `render` function to render the response body and
  `resp-fn` function to construct the response map.

  If `status` is given and `:response/body` is a map, it adds two associations to it:
  `:status` (with a keyword describing HTTP status as value) and `:message` (with a
  string describing HTTP status translated using a detected language). If any key
  already exists in `:response/body`, it will not be added.

  Additionally, if the body map contains `:message` or `message/sub` key and does not
  contain `:lang` key, it will be added with a value of `:language/id` taken from a
  request map `req`."
  (^Response []
   (render-response-force resp/ok nil))
  (^Response [^IFn resp-fn]
   (render-response-force resp-fn nil))
  (^Response [^IFn resp-fn req]
   (resp-fn (render req (or (resp/headers req) {}))))
  (^Response [^IFn resp-fn status req]
   (resp-fn (render req status) (or (resp/headers req) {}))))

;; OK response

(defn render-ok
  "Renders 200 response with possible body taken from a request map (under the
  `:response/body`)."
  (^Response []    (render-response resp/ok nil))
  (^Response [req] (render-response resp/ok :ok/found req)))

(defn render-page
  "Renders 200 response with possible body taken from a request map (under the
  `:response/body`)."
  (^Response []    (render-response resp/ok nil))
  (^Response [req] (render-response resp/ok :ok/found req)))

(defn render-found
  "Renders 200 response with possible body taken from a request map (under the
  `:response/body`)."
  (^Response []    (render-response resp/ok nil))
  (^Response [req] (render-response resp/ok :ok/found req)))

;; Informational responses with bodies

(defn render-early-hints
  "Renders 103 response with possible body taken from a request map (under the
  `:response/body`)."
  (^Response []    (render-response resp/early-hints nil))
  (^Response [req] (render-response resp/early-hints :info/early-hints req)))

;; Success responses with bodies

(defn render-accepted
  "Renders 202 response with possible body taken from a request map (under the
  `:response/body`)."
  (^Response []    (render-response resp/accepted nil))
  (^Response [req] (render-response resp/accepted :ok/accepted req)))

(defn render-in-progress
  "Renders 202 response with possible body taken from a request map (under the
  `:response/body`)."
  (^Response []    (render-response resp/accepted nil))
  (^Response [req] (render-response resp/accepted :ok/in-progress req)))

(defn render-non-authoritative-information
  "Renders 203 response with possible body taken from a request map (under the
  `:response/body`)."
  (^Response []    (render-response resp/non-authoritative-information nil))
  (^Response [req] (render-response resp/non-authoritative-information :ok/non-authoritative-information req)))

(defn render-partial-content
  "Renders 206 response with possible body taken from a request map (under the
  `:response/body`)."
  (^Response []    (render-response resp/partial-content nil))
  (^Response [req] (render-response resp/partial-content :ok/partial-content req)))

(defn render-multi-status
  "Renders 207 response with possible body taken from a request map (under the
  `:response/body`)."
  (^Response []    (render-response resp/multi-status nil))
  (^Response [req] (render-response resp/multi-status :ok/multi-status req)))

(defn render-already-reported
  "Renders 208 response with possible body taken from a request map (under the
  `:response/body`)."
  (^Response []    (render-response resp/already-reported nil))
  (^Response [req] (render-response resp/already-reported :ok/already-reported req)))

(defn render-im-used
  "Renders 226 response with possible body taken from a request map (under the
  `:response/body`)."
  (^Response []    (render-response resp/im-used nil))
  (^Response [req] (render-response resp/im-used :ok/im-used req)))

;; Error responses with possible bodies

(defn render-bad-request
  "Renders 400 response with possible body taken from a request map (under the
  `:response/body`)."
  (^Response []    (render-response resp/bad-request nil))
  (^Response [req] (render-response resp/bad-request :error/bad-request req)))

(defn render-unauthorized
  "Renders 401 response with possible body taken from a request map (under the
  `:response/body`)."
  (^Response []    (render-response resp/unauthorized nil))
  (^Response [req] (render-response resp/unauthorized :error/unauthorized req)))

(defn render-payment-required
  "Renders 402 response with possible body taken from a request map (under the
  `:response/body`)."
  (^Response []    (render-response resp/payment-required nil))
  (^Response [req] (render-response resp/payment-required :error/payment-required req)))

(defn render-forbidden
  "Renders 403 response with possible body taken from a request map (under the
  `:response/body`)."
  (^Response []    (render-response resp/forbidden nil))
  (^Response [req] (render-response resp/forbidden :error/forbidden req)))

(defn render-not-found
  "Renders 404 response with possible body taken from a request map (under the
  `:response/body`)."
  (^Response []    (render-response resp/not-found nil))
  (^Response [req] (render-response resp/not-found :error/not-found req)))

(defn render-method-not-allowed
  "Renders 405 response with possible body taken from a request map (under the
  `:response/body`)."
  (^Response []    (render-response resp/method-not-allowed nil))
  (^Response [req] (render-response resp/method-not-allowed :error/method-not-allowed req)))

(defn render-not-acceptable
  "Renders 406 response with possible body taken from a request map (under the
  `:response/body`)."
  (^Response []    (render-response resp/not-acceptable nil))
  (^Response [req] (render-response resp/not-acceptable :error/not-acceptable req)))

(defn render-proxy-authentication-required
  "Renders 407 response with possible body taken from a request map (under the
  `:response/body`)."
  (^Response []    (render-response resp/proxy-authentication-required nil))
  (^Response [req] (render-response resp/proxy-authentication-required :error/proxy-authentication-required req)))

(defn render-request-timeout
  "Renders 408 response with possible body taken from a request map (under the
  `:response/body`)."
  (^Response []    (render-response resp/request-timeout nil))
  (^Response [req] (render-response resp/request-timeout :error/request-timeout req)))

(defn render-conflict
  "Renders 409 response with possible body taken from a request map (under the
  `:response/body`)."
  (^Response []    (render-response resp/conflict nil))
  (^Response [req] (render-response resp/conflict :error/conflict req)))

(defn render-gone
  "Renders 410 response with possible body taken from a request map (under the
  `:response/body`)."
  (^Response []    (render-response resp/gone nil))
  (^Response [req] (render-response resp/gone :error/gone req)))

(defn render-length-required
  "Renders 411 response with possible body taken from a request map (under the
  `:response/body`)."
  (^Response []    (render-response resp/length-required nil))
  (^Response [req] (render-response resp/length-required :error/length-required req)))

(defn render-precondition-failed
  "Renders 412 response with possible body taken from a request map (under the
  `:response/body`)."
  (^Response []    (render-response resp/precondition-failed nil))
  (^Response [req] (render-response resp/precondition-failed :error/precondition-failed req)))

(defn render-request-entity-too-large
  "Renders 413 response with possible body taken from a request map (under the
  `:response/body`)."
  (^Response []    (render-response resp/request-entity-too-large nil))
  (^Response [req] (render-response resp/request-entity-too-large :error/entity-too-large req)))

(defn render-request-uri-too-long
  "Renders 414 response with possible body taken from a request map (under the
  `:response/body`)."
  (^Response []    (render-response resp/request-uri-too-long nil))
  (^Response [req] (render-response resp/request-uri-too-long :error/request-uri-too-long req)))

(defn render-unsupported-media-type
  "Renders 415 response with possible body taken from a request map (under the
  `:response/body`)."
  (^Response []    (render-response resp/unsupported-media-type nil))
  (^Response [req] (render-response resp/unsupported-media-type :error/unsupported-media-type req)))

(defn render-requested-range-not-satisfiable
  "Renders 416 response with possible body taken from a request map (under the
  `:response/body`)."
  (^Response []    (render-response resp/requested-range-not-satisfiable nil))
  (^Response [req] (render-response resp/requested-range-not-satisfiable :error/requested-range-not-satifiable req)))

(defn render-expectation-failed
  "Renders 417 response with possible body taken from a request map (under the
  `:response/body`)."
  (^Response []    (render-response resp/expectation-failed nil))
  (^Response [req] (render-response resp/expectation-failed :error/expectation-failed req)))

(defn render-im-a-teapot
  "Renders 418 response with possible body taken from a request map (under the
  `:response/body`)."
  (^Response []    (render-response resp/im-a-teapot nil))
  (^Response [req] (render-response resp/im-a-teapot :error/im-a-teapot req)))

(defn render-enhance-your-calm
  "Renders 420 response with possible body taken from a request map (under the
  `:response/body`)."
  (^Response []    (render-response resp/enhance-your-calm nil))
  (^Response [req] (render-response resp/enhance-your-calm :error/enhance-your-calm req)))

(defn render-misdirected-request
  "Renders 421 response with possible body taken from a request map (under the
  `:response/body`)."
  (^Response []    (render-response resp/misdirected-request nil))
  (^Response [req] (render-response resp/misdirected-request :error/misdirected-request req)))

(defn render-unprocessable-entity
  "Renders 422 response with possible body taken from a request map (under the
  `:response/body`)."
  (^Response []    (render-response resp/unprocessable-entity nil))
  (^Response [req] (render-response resp/unprocessable-entity :error/unprocessable-entity req)))

(defn render-bad-params
  "Renders 422 response with possible body taken from a request map (under the
  `:response/body`)."
  (^Response []    (render-response resp/unprocessable-entity nil))
  (^Response [req] (render-response resp/unprocessable-entity :error/bad-parameters req)))

(defn render-locked
  "Renders 423 response with possible body taken from a request map (under the
  `:response/body`)."
  (^Response []    (render-response resp/locked nil))
  (^Response [req] (render-response resp/locked :error/locked req)))

(defn render-failed-dependency
  "Renders 424 response with possible body taken from a request map (under the
  `:response/body`)."
  (^Response []    (render-response resp/failed-dependency nil))
  (^Response [req] (render-response resp/failed-dependency :error/failed-dependency req)))

(defn render-unordered-collection
  "Renders 425 response with possible body taken from a request map (under the
  `:response/body`)."
  (^Response []    (render-response resp/unordered-collection nil))
  (^Response [req] (render-response resp/unordered-collection :error/unordered-collection req)))

(defn render-too-early
  "Renders 425 response with possible body taken from a request map (under the
  `:response/body`)."
  (^Response []    (render-response resp/unordered-collection nil))
  (^Response [req] (render-response resp/unordered-collection :error/too-early req)))

(defn render-upgrade-required
  "Renders 426 response with possible body taken from a request map (under the
  `:response/body`)."
  (^Response []    (render-response resp/upgrade-required nil))
  (^Response [req] (render-response resp/upgrade-required :error/upgrade-required req)))

(defn render-precondition-required
  "Renders 428 response with possible body taken from a request map (under the
  `:response/body`)."
  (^Response []    (render-response resp/precondition-required nil))
  (^Response [req] (render-response resp/precondition-required :error/precondition-failed req)))

(defn render-too-many-requests
  "Renders 429 response with possible body taken from a request map (under the
  `:response/body`)."
  (^Response []    (render-response resp/too-many-requests nil))
  (^Response [req] (render-response resp/too-many-requests :error/too-many-requests req)))

(defn render-request-header-fields-too-large
  "Renders 431 response with possible body taken from a request map (under the
  `:response/body`)."
  (^Response []    (render-response resp/request-header-fields-too-large nil))
  (^Response [req] (render-response resp/request-header-fields-too-large :error/request-header-fields-too-large req)))

(defn render-retry-with
  "Renders 449 response with possible body taken from a request map (under the
  `:response/body`)."
  (^Response []    (render-response resp/retry-with nil))
  (^Response [req] (render-response resp/retry-with :error/retry-with req)))

(defn render-blocked-by-windows-parental-controls
  "Renders 450 response with possible body taken from a request map (under the
  `:response/body`)."
  (^Response []    (render-response resp/blocked-by-windows-parental-controls nil))
  (^Response [req] (render-response resp/blocked-by-windows-parental-controls :error/blocked-by-windows-parental-controls req)))

(defn render-unavailable-for-legal-reasons
  "Renders 451 response with possible body taken from a request map (under the
  `:response/body`)."
  (^Response []    (render-response resp/unavailable-for-legal-reasons nil))
  (^Response [req] (render-response resp/unavailable-for-legal-reasons :error/unavailable-for-legal-reasons req)))

(defn render-internal-server-error
  "Renders 500 response with possible body taken from a request map (under the
  `:response/body`)."
  (^Response []    (render-response resp/internal-server-error nil))
  (^Response [req] (render-response resp/internal-server-error :server-error/internal req)))

(defn render-not-implemented
  "Renders 501 response with possible body taken from a request map (under the
  `:response/body`)."
  (^Response []    (render-response resp/not-implemented nil))
  (^Response [req] (render-response resp/not-implemented :server-error/not-implemented req)))

(defn render-bad-gateway
  "Renders 502 response with possible body taken from a request map (under the
  `:response/body`)."
  (^Response []    (render-response resp/bad-gateway nil))
  (^Response [req] (render-response resp/bad-gateway :server-error/bad-gateway req)))

(defn render-service-unavailable
  "Renders 503 response with possible body taken from a request map (under the
  `:response/body`)."
  (^Response []    (render-response resp/service-unavailable nil))
  (^Response [req] (render-response resp/service-unavailable :server-error/service-unavailable req)))

(defn render-gateway-timeout
  "Renders 504 response with possible body taken from a request map (under the
  `:response/body`)."
  (^Response []    (render-response resp/gateway-timeout nil))
  (^Response [req] (render-response resp/gateway-timeout :server-error/gateway-timeout req)))

(defn render-http-version-not-supported
  "Renders 505 response with possible body taken from a request map (under the
  `:response/body`)."
  (^Response []    (render-response resp/http-version-not-supported nil))
  (^Response [req] (render-response resp/http-version-not-supported :server-error/http-version-not-supported req)))

(defn render-variant-also-negotiates
  "Renders 506 response with possible body taken from a request map (under the
  `:response/body`)."
  (^Response []    (render-response resp/variant-also-negotiates nil))
  (^Response [req] (render-response resp/variant-also-negotiates :server-error/variant-also-negotiates req)))

(defn render-insufficient-storage
  "Renders 507 response with possible body taken from a request map (under the
  `:response/body`)."
  (^Response []    (render-response resp/insufficient-storage nil))
  (^Response [req] (render-response resp/insufficient-storage :server-error/insufficient-storage req)))

(defn render-loop-detected
  "Renders 508 response with possible body taken from a request map (under the
  `:response/body`)."
  (^Response []    (render-response resp/loop-detected nil))
  (^Response [req] (render-response resp/loop-detected :server-error/loop-detected req)))

(defn render-bandwidth-limit-exceeded
  "Renders 509 response with possible body taken from a request map (under the
  `:response/body`)."
  (^Response []    (render-response resp/bandwidth-limit-exceeded nil))
  (^Response [req] (render-response resp/bandwidth-limit-exceeded :server-error/bandwidth-limit-exceeded req)))

(defn render-not-extended
  "Renders 510 response with possible body taken from a request map (under the
  `:response/body`)."
  (^Response []    (render-response resp/not-extended nil))
  (^Response [req] (render-response resp/not-extended :server-error/not-extended req)))

(defn render-network-authentication-required
  "Renders 511 response with possible body taken from a request map (under the
  `:response/body`)."
  (^Response []    (render-response resp/network-authentication-required nil))
  (^Response [req] (render-response resp/network-authentication-required :server-error/network-authentication-required req)))

(defn render-network-read-timeout
  "Renders 598 response with possible body taken from a request map (under the
  `:response/body`)."
  (^Response []    (render-response resp/network-read-timeout nil))
  (^Response [req] (render-response resp/network-read-timeout :server-error/network-read-timeout req)))

(defn render-network-connect-timeout
  "Renders 599 response with possible body taken from a request map (under the
  `:response/body`)."
  (^Response []    (render-response resp/network-connect-timeout nil))
  (^Response [req] (render-response resp/network-connect-timeout :server-error/network-connect-timeout req)))

;; Resource creation success, redirect with a possible body

(defn render-created
  "Renders 201 response with a redirect (possibly localized if a destination path is
  language-parameterized) and possible body taken from a request map (under the
  `:response/body`). If a name or a path is not given as `name-or-path`, it is looked
  up in `req` under the `:response/location` key."
  (^Response []
   (resp/created))
  (^Response [req]
   (resp/created (render req :ok/created)
                 (or (resp/headers req) {})
                 (resp/location req page)))
  (^Response [req data]
   (resp/created (render req :ok/created)
                 (or (resp/headers req) {})
                 (resp/location req page)))
  (^Response [req data view]
   (resp/created (render req :ok/created)
                 (or (resp/headers req) {})
                 (resp/location req page)))
  (^Response [req data view layout]
   (resp/created (render req :ok/created)
                 (or (resp/headers req) {})
                 (resp/location req page)))
  (^Response [req data view layout lang]
   (resp/created (render req :ok/created)
                 (or (resp/headers req) {})
                 (resp/location req page lang)))
  (^Response [req data view layout lang name-or-path]
   (resp/created (render req :ok/created)
                 (or (resp/headers req) {})
                 (if (is-url? name-or-path)
                   name-or-path
                   (page req name-or-path lang))))
  (^Response [req data view layout lang name-or-path params]
   (resp/created (render req :ok/created)
                 (or (resp/headers req) {})
                 (if (is-url? name-or-path)
                   name-or-path
                   (page req name-or-path lang params))))
  (^Response [req data view layout lang name-or-path params query-params]
   (resp/created (render req :ok/created)
                 (or (resp/headers req) {})
                 (if (is-url? name-or-path)
                   name-or-path
                   (page req name-or-path lang params query-params))))
  (^Response [req data view layout lang name-or-path params query-params & more]
   (resp/created (render req :ok/created)
                 (or (resp/headers req) {})
                 (if (is-url? name-or-path)
                   name-or-path
                   (apply page req name-or-path lang params query-params more)))))

(defn render-localized-created
  "Renders 201 response with a localized redirect and possible body taken from a
  request map (under the `:response/body`). If a name or a path is not given as
  `name-or-path`, it is looked up in `req` under the `:response/location` key."
  (^Response []
   (resp/created))
  (^Response [req]
   (resp/created (render req :ok/created)
                 (or (resp/headers req) {})
                 (resp/location req localized-page)))
  (^Response [req data]
   (resp/created (render req :ok/created)
                 (or (resp/headers req) {})
                 (resp/location req localized-page)))
  (^Response [req data view]
   (resp/created (render req :ok/created)
                 (or (resp/headers req) {})
                 (resp/location req localized-page)))
  (^Response [req data view layout]
   (resp/created (render req :ok/created)
                 (or (resp/headers req) {})
                 (resp/location req localized-page)))
  (^Response [req data view layout lang]
   (resp/created (render req :ok/created)
                 (or (resp/headers req) {})
                 (resp/location req localized-page lang)))
  (^Response [req data view layout lang name-or-path]
   (resp/created (render req :ok/created)
                 (or (resp/headers req) {})
                 (if (is-url? name-or-path)
                   name-or-path
                   (localized-page req name-or-path lang))))
  (^Response [req data view layout lang name-or-path params]
   (resp/created (render req :ok/created)
                 (or (resp/headers req) {})
                 (if (is-url? name-or-path)
                   name-or-path
                   (localized-page req name-or-path lang params))))
  (^Response [req data view layout lang name-or-path params query-params]
   (resp/created (render req :ok/created)
                 (or (resp/headers req) {})
                 (if (is-url? name-or-path)
                   name-or-path
                   (localized-page req name-or-path lang params query-params))))
  (^Response [req data view layout lang name-or-path params query-params & more]
   (resp/created (render req :ok/created)
                 (or (resp/headers req) {})
                 (if (is-url? name-or-path)
                   name-or-path
                   (apply localized-page req name-or-path lang params query-params more)))))

;; Responses without a body

(defn render-continue
  "Renders 100 response without a body."
  (^Response []        (resp/continue))
  (^Response [req]     (resp/continue nil (resp/headers req)))
  (^Response [req & _] (resp/continue nil (resp/headers req))))

(defn render-switching-protocols
  "Renders 101 response without a body."
  (^Response []        (resp/switching-protocols))
  (^Response [req]     (resp/switching-protocols nil (resp/headers req)))
  (^Response [req & _] (resp/switching-protocols nil (resp/headers req))))

(defn render-processing
  "Renders 102 response without a body."
  (^Response []        (resp/processing))
  (^Response [req]     (resp/processing nil (resp/headers req)))
  (^Response [req & _] (resp/processing nil (resp/headers req))))

(defn render-no-content
  "Renders 204 response without a body."
  (^Response []        (resp/no-content))
  (^Response [req]     (resp/no-content nil (resp/headers req)))
  (^Response [req & _] (resp/no-content nil (resp/headers req))))

(defn render-reset-content
  "Renders 205 response without a body."
  (^Response []        (resp/reset-content))
  (^Response [req]     (resp/reset-content nil (resp/headers req)))
  (^Response [req & _] (resp/reset-content nil (resp/headers req))))

;; Rendering based on application-logic error

(defn add-missing-sub-status-to-response
  ([req out sub-status]
   (add-missing-sub-status-to-response req out sub-status :sub-status :body
                                       :sub-status/title :sub-status/description
                                       :status/see-also))
  ([req out sub-status sub-key]
   (add-missing-sub-status-to-response req out sub-status sub-key :body))
  ([req out sub-status sub-key main-key]
   (add-missing-sub-status-to-response req out sub-status sub-key main-key nil))
  ([req out sub-status sub-key main-key tr-sub]
   (let [sub-ns        (name sub-key)
         sub-title-key (keyword sub-ns "title")
         sub-desc-key  (keyword sub-ns "description")]
     (add-missing-sub-status-to-response req out sub-status sub-key main-key
                                         sub-title-key sub-desc-key :status/see-also tr-sub)))
  ([req out sub-status sub-key main-key sub-title-key sub-desc-key see-also-key]
   (add-missing-sub-status-to-response req out sub-status sub-key main-key
                                       sub-title-key sub-desc-key see-also-key nil))
  ([req out sub-status sub-key main-key sub-title-key sub-desc-key see-also-key tr-sub]
   (if sub-status
     (let [body (get out main-key)]
       (if (or (nil? body) (map? body))
         (qassoc
          out main-key
          (if (contains? body sub-key)
            (common/add-missing-lang body req [sub-title-key sub-desc-key])
            (let [see-also (conj (or (get body see-also-key) []) sub-key)
                  body     (qassoc body sub-key sub-status see-also-key see-also)]
              (if (common/untranslatable? sub-status)
                (common/add-missing-lang body req [sub-title-key sub-desc-key])
                (let [tr-sub (or tr-sub (i18n/no-default (common/translator-sub req)))]
                  (-> body
                      (common/add-missing-translation sub-title-key sub-status tr-sub)
                      (common/add-missing-translation sub-desc-key  sub-status ".full" tr-sub)
                      (common/add-missing-lang req [sub-title-key sub-desc-key])))))))
         out))
     out)))

(defn add-missing-sub-status
  ([req sub-status]
   (add-missing-sub-status-to-response req req sub-status :sub-status :response/body
                                       :sub-status/title :sub-status/description
                                       :status/see-also))
  ([req sub-status sub-key]
   (add-missing-sub-status-to-response req req sub-status sub-key :response/body))
  ([req sub-status sub-key main-key]
   (add-missing-sub-status-to-response req req sub-status sub-key main-key nil))
  ([req sub-status sub-key main-key tr-sub]
   (add-missing-sub-status-to-response req req sub-status sub-key main-key tr-sub))
  ([req sub-status sub-key main-key sub-title-key sub-desc-key see-also-key]
   (add-missing-sub-status-to-response req req sub-status sub-key main-key
                                       sub-title-key sub-desc-key see-also-key nil))
  ([req sub-status sub-key main-key sub-title-key sub-desc-key see-also-key tr-sub]
   (add-missing-sub-status-to-response req req sub-status sub-key main-key
                                       sub-title-key sub-desc-key see-also-key tr-sub)))

(defn render-status
  "Renders a status response for the given request map and optional `sub-status`
  (a keyword, mapped to a response rendering function, using a map passed under the
  `:errors/config` key in a route data). If it is not given (or its value is `nil` or
  `false`) then `render-ok` will be used to generate the response.

  The resulting response body will have the `:status` key set by the rendering
  function and `:sub-status` added by this function. Additionally, `:status/title`,
  `:status/description`, `:sub-status/title` and `:sub-status/description` will be
  populated by strings explaining the status and sub-status.

  Example:

  `(render-status req :verify/bad-token)`

  Will create a response with the following body:

  ```
  {:status                 :error/unauthorized
   :status/title           \"Unauthorized\"
   :status/description     \"You are not authorized to perform this action.\"
   :sub-status             :verify/bad-token
   :sub-status/title       \"Bad token\"
   :sub-status/description \"The given token is malformed of has expired.\"
   :lang                   :en}
  ```"
  {:arglists '(^Response []
               ^Response [req]
               ^Response [req sub-status]
               ^Response [req sub-statuses]
               ^Response [req sub-status default]
               ^Response [req sub-statuses default]
               ^Response [req sub-status default & more]
               ^Response [req sub-statuses default & more])}
  (^Response []
   (resp/ok))
  (^Response [req]
   (errors/render req nil render-ok req))
  (^Response [req sub-status]
   (let [err-config (errors/config req)
         sub-status (errors/most-significant err-config sub-status)]
     (if-some [resp (errors/render err-config sub-status render-ok req)]
       (add-missing-sub-status-to-response req resp sub-status))))
  (^Response [req sub-status default]
   (let [err-config (errors/config req)
         sub-status (errors/most-significant err-config sub-status)]
     (if-some [resp (errors/render err-config sub-status (or default render-ok) req)]
       (add-missing-sub-status-to-response req resp sub-status))))
  (^Response [req sub-status default & more]
   (let [err-config (errors/config req)
         sub-status (errors/most-significant err-config sub-status)]
     (if-some [resp (apply errors/render err-config sub-status (or default render-ok) req more)]
       (add-missing-sub-status-to-response req resp sub-status)))))

(defn render-error
  "Renders an error response for the given request map and optional `sub-status`
  (a keyword, mapped to a response rendering function, using a map passed under the
  `:errors/config` key in a route data). If it is not given (or its value is `nil` or
  `false`) then `render-internal-server-error` will be used to generate the response.

  The resulting response body will have the `:status` key set by the rendering
  function and `:sub-status` added by this function. Additionally, `:status/title`,
  `:status/description`, `:sub-status/title` and `:sub-status/description` will be
  populated by strings explaining the status and sub-status.

  Example:

  `(render-error req :verify/bad-token)`

  Will create a response with the following body:

  ```
  {:status                 :error/unauthorized
   :status/title           \"Unauthorized\"
   :status/description     \"You are not authorized to perform this action.\"
   :sub-status             :verify/bad-token
   :sub-status/title       \"Bad token\"
   :sub-status/description \"The given token is malformed of has expired.\"
   :lang                   :en}
  ```"
  {:arglists '(^Response []
               ^Response [req]
               ^Response [req sub-status]
               ^Response [req sub-statuses]
               ^Response [req sub-status default]
               ^Response [req sub-statuses default]
               ^Response [req sub-status default & more]
               ^Response [req sub-statuses default & more])}
  (^Response []
   (resp/internal-server-error))
  (^Response [req]
   (errors/render req nil render-internal-server-error req))
  (^Response [req sub-status]
   (let [err-config (errors/config req)
         sub-status (errors/most-significant err-config sub-status)]
     (if-some [resp (errors/render err-config sub-status render-internal-server-error req)]
       (add-missing-sub-status-to-response req resp sub-status))))
  (^Response [req sub-status default]
   (let [err-config (errors/config req)
         sub-status (errors/most-significant err-config sub-status)]
     (if-some [resp (errors/render err-config sub-status (or default render-internal-server-error) req)]
       (add-missing-sub-status-to-response req resp sub-status))))
  (^Response [req sub-status default & more]
   (let [err-config (errors/config req)
         sub-status (errors/most-significant err-config sub-status)]
     (if-some [resp (apply errors/render err-config sub-status (or default render-internal-server-error) req more)]
       (add-missing-sub-status-to-response req resp sub-status)))))

;; Linking helpers

(p/import-vars [amelinium.common
                path localized-path])

(defn anti-spam-code
  "Generates anti-spam value pairs string containing randomly selected fields and
  values using `validators/gen-required`."
  ([config]
   (anti-spam-code config 1 nil))
  ([config num]
   (anti-spam-code config num nil))
  ([config num rng]
   (let [r       (validators/gen-required config num rng)
         k-some  (seq (get r :some))
         k-blank (seq (get r :blank))
         k-any   (seq (get r :any))
         r       (concat
                  (if k-some  (map vector k-some  (repeatedly random-uuid)))
                  (if k-blank (map vector k-blank (repeat "")))
                  (if k-any   (map vector k-any   (repeatedly #(random-uuid-or-empty rng)))))]
     (if (seq r)
       (into {} r)))))

;; Other helpers

(defn lang-url
  [req path-or-name lang params query-params lang-settings]
  (common/lang-url true req path-or-name lang params query-params lang-settings))

(defn body-add-lang
  ([req]
   (qassoc req :response/body
           (let [body (get req :response/body)
                 k    (common/lang-param req)
                 v    (common/lang-id req)]
             (qassoc body k v))))
  ([req lang]
   (qassoc req :response/body
           (let [body (get req :response/body)
                 k    (common/lang-param req)
                 v    (or lang (common/lang-id req))]
             (qassoc body k v))))
  ([req lang field]
   (qassoc req :response/body
           (let [body (get req :response/body)
                 k    (or field (common/lang-param req))
                 v    (or lang  (common/lang-id req))]
             (qassoc body k v)))))

(defn body-add-session-id
  "Adds session ID field to the response body (`:response/body`) of the given request
  map `req`. Works only on a valid sessions, having `:id` field set."
  {:arglists '([req]
               [req smap]
               [req session-key]
               [req smap field]
               [req session-key field])}
  ([req]
   (if-some [smap (session/of req)]
     (body-add-session-id req smap)
     req))
  ([req smap]
   (let [smap (if (keyword? smap) (session/of req smap) (session/of smap))
         k    (or (session/id-field smap) :session-id)
         k    (if (string? k) (keyword k) k)
         sid  (session/id smap)]
     (if sid
       (qupdate req :response/body qassoc k sid)
       req)))
  ([req smap field]
   (let [smap (if (keyword? smap) (session/of req smap) (session/of smap))
         k    (or field (session/id-field smap) :session-id)
         k    (if (string? k) (keyword k) k)
         sid  (session/id smap)]
     (if sid
       (qupdate req :response/body qassoc k sid)
       req))))

(defn session-status
  "Returns session status for the given session map `smap`."
  [smap]
  (if-not (session/session? smap)
    :session/missing
    (or (some-keyword (get (session/error smap) :cause)) :session/unknown-error)))

(defn body-add-session-status
  "Gets the value of `:app/status` key of the given `req` and if it is set to
  `:auth/session-error` or `:error/session`, adds `:session-status` to a response
  body with a value set to a result of calling `session-status` on a current
  session. If there is no session error detected, it simply calls `body-add-session`
  to add session ID to the response body."
  {:arglists '([req]
               [req smap]
               [req session-key]
               [req smap translate-sub]
               [req session-key translate-sub])}
  ([req]
   (body-add-session-status req nil nil))
  ([req smap]
   (body-add-session-status req smap nil))
  ([req smap translate-sub]
   (let [rstatus (get req :app/status)
         smap    (if (keyword? smap) (session/of req smap) (session/of smap))]
     (if (or (identical? rstatus :auth/session-error)
             (identical? rstatus :error/session)
             (session/error? smap))
       (add-missing-sub-status req (session-status smap) :session-status :response/body translate-sub)
       (if (identical? rstatus :auth/ok)
         (-> req
             (add-missing-sub-status :session/created :session-status :response/body translate-sub)
             (body-add-session-id smap))
         (body-add-session-id req smap))))))

(defmacro response
  "Creates a response block. If the given `req` is already a response then it is simply
  returned. Otherwise the expressions from `code` are evaluated."
  [req & code]
  (if (and (seq? code) (> (count code) 1))
    `(let [req# ~req] (if (resp/response? req#) req# (do ~@code)))
    `(let [req# ~req] (if (resp/response? req#) req# ~@code))))

(defmacro add-body
  "Adds a response body to a request map `req` under its key `:response/body` using
  `qassoc`. The body is a result of evaluating expressions passed as additional
  arguments (`body`). Returns updated `req`. Assumes that `req` is always a map."
  [req & body]
  (if (and (seq? body) (> (count body) 1))
    `(qassoc ~req :response/body (do ~@body))
    `(qassoc ~req :response/body ~@body)))

(defn update-body
  "Updates the `:response/body` in a request map `req` with a result of calling the
  function `f` on the previous value and optional arguments. Uses
  `io.randomseed.utils.map/qassoc`. Returns updated `req` with a map under
  `:response/body` key."
  ([req]                (add-body req))
  ([req ^IFn f]         (qassoc req :response/body (f (get req :response/body))))
  ([req ^IFn f a]       (qassoc req :response/body (f (get req :response/body) a)))
  ([req ^IFn f a b]     (qassoc req :response/body (f (get req :response/body) a b)))
  ([req ^IFn f a b c]   (qassoc req :response/body (f (get req :response/body) a b c)))
  ([req ^IFn f a b c d] (qassoc req :response/body (f (get req :response/body) a b c d)))
  ([req ^IFn f a b c d & more]
   (qassoc req :response/body (apply f (get req :response/body) a b c d more))))

(defmacro assoc-body
  "Adds keys with associated values to `:response/body` map of the `req` using
  `qassoc`. If any key argument is a literal keyword, a character, or a literal
  string, it will be converted to a keyword literal and placed as `qassoc`
  argument. Otherwise it will be left as is and wrapped into a call to
  `io.randomseed.utils/some-keyword` to ensure the result is a keyword
  run-time. Missing last value, if any, will be padded with `nil`. If there is no
  body or the body is empty, it will initialize it with a map expression, otherwise
  it will use `assoc`. Assumes that `req` is always a map."
  ([req k v]
   (let [k (if (or (keyword? k) (string? k) (char? k))
             (some-keyword k)
             (cons `some-keyword (cons k nil)))]
     `(let [req# ~req]
        (qassoc req# :response/body (qassoc (get req# :response/body) ~k ~v)))))
  ([req k v & more]
   (let [pairs  (cons k (cons v more))
         names  (take-nth 2 pairs)
         values (concat (take-nth 2 (rest pairs)) '(nil))
         pairs  (map #(cons (if (or (keyword? %1)
                                    (string?  %1)
                                    (char?    %1))
                              (some-keyword %1)
                              (cons `some-keyword (cons %1 nil)))
                            (cons %2 nil))
                     names values)
         pairs  (apply concat pairs)
         names  (take-nth 2 pairs)
         dups?  (not= (count names) (count (distinct names)))]
     (if dups?
       `(let [req# ~req]
          (qassoc req# :response/body (qassoc (get req# :response/body) ~@pairs)))
       `(let [req# ~req
              bod# (get req# :response/body)]
          (qassoc req# :response/body
                  (if (pos? (count bod#))
                    (qassoc bod# ~@pairs)
                    {~@pairs ~@[]})))))))

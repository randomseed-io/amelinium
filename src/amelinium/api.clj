(ns

    ^{:doc    "API helpers for amelinium."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.api

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:require [clojure.string                       :as          str]
            [potemkin.namespaces                  :as            p]
            [lazy-map.core                        :as     lazy-map]
            [ring.util.response]
            [ring.util.http-response              :as         resp]
            [ring.util.request                    :as          req]
            [amelinium.common                     :as       common]
            [amelinium.errors                     :as       errors]
            [amelinium.i18n                       :as         i18n]
            [amelinium.http.middleware.validators :as   validators]
            [io.randomseed.utils                  :refer      :all])

  (:import [reitit.core Match]
           [lazy_map.core LazyMapEntry LazyMap]))

;; Database

(p/import-vars [amelinium.common
                auth-config auth-db])

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
                page localized-page localized-or-regular-page
                current-page current-page-id current-page-id-or-path login-page auth-page
                temporary-redirect localized-temporary-redirect
                move-to see-other localized-see-other go-to])

;; Language

(p/import-vars [amelinium.common
                pick-language pick-language-without-fallback
                pick-language-str pick-language-str-without-fallback])

;; Special redirects

(p/import-vars [amelinium.common
                add-slash slash-redir lang-redir])

;; Accounts

(p/import-vars [amelinium.common
                lock-wait-default lock-wait
                hard-lock-time hard-locked?
                soft-lock-time soft-lock-passed soft-locked? soft-lock-remains])

;; Sessions

(p/import-vars [amelinium.common
                session-field session-variable-get-failed?
                allow-expired allow-soft-expired allow-hard-expired])

;; Context and roles

(p/import-vars [amelinium.common
                has-any-role? has-role?
                role-required! with-role-only!
                roles-for-context roles-for-contexts default-contexts-labeler
                roles-matrix roles-tabler])

;; Data structures

(p/import-vars [amelinium.common
                empty-lazy-map])

;; Filesystem operations

(p/import-vars [amelinium.common
                some-resource])

;; Language helpers

(p/import-vars [amelinium.common
                lang-id lang-str lang-config])

;; Response rendering

(defn- add-missing-lang
  [body req translation-keys]
  (if (contains? body :lang)
    body
    (if (some #(contains? body %) translation-keys)
      (if-some [l (get req :language/id)]
        (assoc body :lang l)
        body)
      body)))

(defn- add-missing-translation
  ([body new-k k translation-fn]
   (if (contains? body new-k)
     body
     (if-some [t (translation-fn k)] (assoc body new-k t) body)))
  ([body new-k k suffix translation-fn]
   (if (contains? body new-k)
     body
     (if-some [t (translation-fn (namespace k) (str (name k) suffix))]
       (assoc body new-k t)
       body))))

(defn render
  "Returns response body on a basis of a value associated with the `:response/body` key
  of the `req`.

  If `status` is given and `:response/body` is a map, it adds the following
  associations to it: `:status` (with a keyword describing status as value),
  `:status/title` (with a string describing status translated using a detected
  language) and `:status/description` (with a longer string explaining the status,
  based on a translation key created by adding \".full\" to the original status
  key). If any key already exists in `:response/body`, it will not be added.

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
   (let [body (get req :response/body)]
     (if (map? body)
       (if (contains? body :status)
         (add-missing-lang body req [:status/title :status/description])
         (let [tr-sub (i18n/no-default (common/translator-sub req))]
           (-> body
               (assoc :status status)
               (add-missing-translation :status/title status tr-sub)
               (add-missing-translation :status/description status ".full" tr-sub)
               (add-missing-lang req [:status/title :status/description]))))
       (if (sequential? body)
         (seq body)
         body)))))

(defn response?
  "Returns `true` if the `req` context map is already an API response."
  [req]
  (and (map? req)
       (integer?  (:status  req))
       (or (map?  (:headers req))
           (coll? (:body    req)))))

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
  ([]
   (render-response resp/ok nil))
  ([resp-fn]
   (render-response resp-fn nil))
  ([resp-fn req]
   (if (response? req)
     req
     (let [resp (resp-fn (render req))]
       (if-some [headers (get req :response/headers)]
         (assoc resp :headers headers)
         resp))))
  ([resp-fn status req]
   (if (response? req)
     req
     (let [resp (resp-fn (render req status))]
       (if-some [headers (get req :response/headers)]
         (assoc resp :headers headers)
         resp)))))

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
  ([]
   (render-response-force resp/ok nil))
  ([resp-fn]
   (render-response-force resp-fn nil))
  ([resp-fn req]
   (let [resp (resp-fn (render req))]
     (if-some [headers (get req :response/headers)]
       (assoc resp :headers headers)
       (resp-fn resp))))
  ([resp-fn status req]
   (let [resp (resp-fn (render req status))]
     (if-some [headers (get req :response/headers)]
       (assoc resp :headers headers)
       (resp-fn resp)))))

;; OK response

(defn render-ok
  "Renders 200 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/ok nil))
  ([req] (render-response resp/ok :ok/found req)))

(defn render-page
  "Renders 200 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/ok nil))
  ([req] (render-response resp/ok :ok/found req)))

(defn render-found
  "Renders 200 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/ok nil))
  ([req] (render-response resp/ok :ok/found req)))

;; Informational responses with bodies

(defn render-early-hints
  "Renders 103 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response common/early-hints nil))
  ([req] (render-response common/early-hints :info/early-hints req)))

;; Success responses with bodies

(defn render-accepted
  "Renders 202 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/accepted nil))
  ([req] (render-response resp/accepted :ok/accepted req)))

(defn render-in-progress
  "Renders 202 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/accepted nil))
  ([req] (render-response resp/accepted :ok/in-progress req)))

(defn render-non-authoritative-information
  "Renders 203 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/non-authoritative-information nil))
  ([req] (render-response resp/non-authoritative-information :ok/non-authoritative-information req)))

(defn render-partial-content
  "Renders 206 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/partial-content nil))
  ([req] (render-response resp/partial-content :ok/partial-content req)))

(defn render-multi-status
  "Renders 207 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/multi-status nil))
  ([req] (render-response resp/multi-status :ok/multi-status req)))

(defn render-already-reported
  "Renders 208 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/already-reported nil))
  ([req] (render-response resp/already-reported :ok/already-reported req)))

(defn render-im-used
  "Renders 226 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/im-used nil))
  ([req] (render-response resp/im-used :ok/im-used req)))

;; Error responses with possible bodies

(defn render-bad-request
  "Renders 400 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/bad-request nil))
  ([req] (render-response resp/bad-request :error/bad-request req)))

(defn render-unauthorized
  "Renders 401 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/unauthorized nil))
  ([req] (render-response resp/unauthorized :error/unauthorized req)))

(defn render-payment-required
  "Renders 402 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/payment-required nil))
  ([req] (render-response resp/payment-required :error/payment-required req)))

(defn render-forbidden
  "Renders 403 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/forbidden nil))
  ([req] (render-response resp/forbidden :error/forbidden req)))

(defn render-not-found
  "Renders 404 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/not-found nil))
  ([req] (render-response resp/not-found :error/not-found req)))

(defn render-method-not-allowed
  "Renders 405 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/method-not-allowed nil))
  ([req] (render-response resp/method-not-allowed :error/method-not-allowed req)))

(defn render-not-acceptable
  "Renders 406 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/not-acceptable nil))
  ([req] (render-response resp/not-acceptable :error/not-acceptable req)))

(defn render-proxy-authentication-required
  "Renders 407 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/proxy-authentication-required nil))
  ([req] (render-response resp/proxy-authentication-required :error/proxy-authentication-required req)))

(defn render-request-timeout
  "Renders 408 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/request-timeout nil))
  ([req] (render-response resp/request-timeout :error/request-timeout req)))

(defn render-conflict
  "Renders 409 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/conflict nil))
  ([req] (render-response resp/conflict :error/conflict req)))

(defn render-gone
  "Renders 410 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/gone nil))
  ([req] (render-response resp/gone :error/gone req)))

(defn render-length-required
  "Renders 411 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/length-required nil))
  ([req] (render-response resp/length-required :error/length-required req)))

(defn render-precondition-failed
  "Renders 412 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/precondition-failed nil))
  ([req] (render-response resp/precondition-failed :error/precondition-failed req)))

(defn render-request-entity-too-large
  "Renders 413 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/request-entity-too-large nil))
  ([req] (render-response resp/request-entity-too-large :error/entity-too-large req)))

(defn render-request-uri-too-long
  "Renders 414 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/request-uri-too-long nil))
  ([req] (render-response resp/request-uri-too-long :error/request-uri-too-long req)))

(defn render-unsupported-media-type
  "Renders 415 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/unsupported-media-type nil))
  ([req] (render-response resp/unsupported-media-type :error/unsupported-media-type req)))

(defn render-requested-range-not-satisfiable
  "Renders 416 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/requested-range-not-satisfiable nil))
  ([req] (render-response resp/requested-range-not-satisfiable :error/requested-range-not-satifiable req)))

(defn render-expectation-failed
  "Renders 417 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/expectation-failed nil))
  ([req] (render-response resp/expectation-failed :error/expectation-failed req)))

(defn render-im-a-teapot
  "Renders 418 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response common/im-a-teapot nil))
  ([req] (render-response common/im-a-teapot :error/im-a-teapot req)))

(defn render-enhance-your-calm
  "Renders 420 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/enhance-your-calm nil))
  ([req] (render-response resp/enhance-your-calm :error/enhance-your-calm req)))

(defn render-misdirected-request
  "Renders 421 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response common/misdirected-request nil))
  ([req] (render-response common/misdirected-request :error/misdirected-request req)))

(defn render-unprocessable-entity
  "Renders 422 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/unprocessable-entity nil))
  ([req] (render-response resp/unprocessable-entity :error/unprocessable-entity req)))

(defn render-bad-params
  "Renders 422 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/unprocessable-entity nil))
  ([req] (render-response resp/unprocessable-entity :error/bad-parameters req)))

(defn render-locked
  "Renders 423 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/locked nil))
  ([req] (render-response resp/locked :error/locked req)))

(defn render-failed-dependency
  "Renders 424 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/failed-dependency nil))
  ([req] (render-response resp/failed-dependency :error/failed-dependency req)))

(defn render-unordered-collection
  "Renders 425 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/unordered-collection nil))
  ([req] (render-response resp/unordered-collection :error/unordered-collection req)))

(defn render-too-early
  "Renders 425 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/unordered-collection nil))
  ([req] (render-response resp/unordered-collection :error/too-early req)))

(defn render-upgrade-required
  "Renders 426 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/upgrade-required nil))
  ([req] (render-response resp/upgrade-required :error/upgrade-required req)))

(defn render-precondition-required
  "Renders 428 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/precondition-required nil))
  ([req] (render-response resp/precondition-required :error/precondition-failed req)))

(defn render-too-many-requests
  "Renders 429 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/too-many-requests nil))
  ([req] (render-response resp/too-many-requests :error/too-many-requests req)))

(defn render-request-header-fields-too-large
  "Renders 431 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/request-header-fields-too-large nil))
  ([req] (render-response resp/request-header-fields-too-large :error/request-header-fields-too-large req)))

(defn render-retry-with
  "Renders 449 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/retry-with nil))
  ([req] (render-response resp/retry-with :error/retry-with req)))

(defn render-blocked-by-windows-parental-controls
  "Renders 450 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/blocked-by-windows-parental-controls nil))
  ([req] (render-response resp/blocked-by-windows-parental-controls :error/blocked-by-windows-parental-controls req)))

(defn render-unavailable-for-legal-reasons
  "Renders 451 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/unavailable-for-legal-reasons nil))
  ([req] (render-response resp/unavailable-for-legal-reasons :error/unavailable-for-legal-reasons req)))

(defn render-internal-server-error
  "Renders 500 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/internal-server-error nil))
  ([req] (render-response resp/internal-server-error :server-error/internal req)))

(defn render-not-implemented
  "Renders 501 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/not-implemented nil))
  ([req] (render-response resp/not-implemented :server-error/not-implemented req)))

(defn render-bad-gateway
  "Renders 502 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/bad-gateway nil))
  ([req] (render-response resp/bad-gateway :server-error/bad-gateway req)))

(defn render-service-unavailable
  "Renders 503 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/service-unavailable nil))
  ([req] (render-response resp/service-unavailable :server-error/service-unavailable req)))

(defn render-gateway-timeout
  "Renders 504 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/gateway-timeout nil))
  ([req] (render-response resp/gateway-timeout :server-error/gateway-timeout req)))

(defn render-http-version-not-supported
  "Renders 505 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/http-version-not-supported nil))
  ([req] (render-response resp/http-version-not-supported :server-error/http-version-not-supported req)))

(defn render-variant-also-negotiates
  "Renders 506 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/variant-also-negotiates nil))
  ([req] (render-response resp/variant-also-negotiates :server-error/variant-also-negotiates req)))

(defn render-insufficient-storage
  "Renders 507 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/insufficient-storage nil))
  ([req] (render-response resp/insufficient-storage :server-error/insufficient-storage req)))

(defn render-loop-detected
  "Renders 508 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/loop-detected nil))
  ([req] (render-response resp/loop-detected :server-error/loop-detected req)))

(defn render-bandwidth-limit-exceeded
  "Renders 509 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/bandwidth-limit-exceeded nil))
  ([req] (render-response resp/bandwidth-limit-exceeded :server-error/bandwidth-limit-exceeded req)))

(defn render-not-extended
  "Renders 510 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/not-extended nil))
  ([req] (render-response resp/not-extended :server-error/not-extended req)))

(defn render-network-authentication-required
  "Renders 511 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/network-authentication-required nil))
  ([req] (render-response resp/network-authentication-required :server-error/network-authentication-required req)))

(defn render-network-read-timeout
  "Renders 598 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/network-read-timeout nil))
  ([req] (render-response resp/network-read-timeout :server-error/network-read-timeout req)))

(defn render-network-connect-timeout
  "Renders 599 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/network-connect-timeout nil))
  ([req] (render-response resp/network-connect-timeout :server-error/network-connect-timeout req)))

;; Resource creation success, redirect with a possible body

(defn render-created
  "Renders 201 response with a redirect (possibly localized if a destination path is
  language-parameterized) and possible body taken from a request map (under the
  `:response/body`)."
  ([]
   (common/render resp/created))
  ([req]
   (if-some [resp (common/created req)]
     (assoc resp :body (render req :ok/created))))
  ([req name-or-path]
   (if-some [resp (common/created req name-or-path)]
     (assoc resp :body (render req :ok/created))))
  ([req name-or-path lang]
   (if-some [resp (common/created req name-or-path lang)]
     (assoc resp :body (render req :ok/created))))
  ([req name-or-path lang params]
   (if-some [resp (common/created req name-or-path lang params)]
     (assoc resp :body (render req :ok/created))))
  ([req name-or-path lang params query-params]
   (if-some [resp (common/created req name-or-path lang params query-params)]
     (assoc resp :body (render req :ok/created))))
  ([req name-or-path lang params query-params & more]
   (if-some [resp (apply common/created req name-or-path lang params query-params more)]
     (assoc resp :body (render req :ok/created)))))

(defn render-localized-created
  "Renders 201 response with a localized redirect and possible body taken from a
  request map (under the `:response/body`)."
  ([]
   (render-response resp/created))
  ([req]
   (if-some [resp (common/localized-created req)]
     (assoc resp :body (render req :ok/created))))
  ([req name-or-path]
   (if-some [resp (common/localized-created req name-or-path)]
     (assoc resp :body (render req :ok/created))))
  ([req name-or-path lang]
   (if-some [resp (common/localized-created req name-or-path lang)]
     (assoc resp :body (render req :ok/created))))
  ([req name-or-path lang params]
   (if-some [resp (common/localized-created req name-or-path lang params)]
     (assoc resp :body (render req :ok/created))))
  ([req name-or-path lang params query-params]
   (if-some [resp (common/localized-created req name-or-path lang params query-params)]
     (assoc resp :body (render req :ok/created))))
  ([req name-or-path lang params query-params & more]
   (if-some [resp (apply common/localized-created req name-or-path lang params query-params more)]
     (assoc resp :body (render req :ok/created)))))

;; Responses without a body

(defn render-continue
  "Renders 100 response without a body."
  ([]           (resp/continue))
  ([req]        (common/render resp/continue req))
  ([req & more] (common/render resp/continue req)))

(defn render-switching-protocols
  "Renders 101 response without a body."
  ([]           (resp/switching-protocols))
  ([req]        (common/render resp/switching-protocols req))
  ([req & more] (common/render resp/switching-protocols req)))

(defn render-processing
  "Renders 102 response without a body."
  ([]           (resp/processing))
  ([req]        (common/render resp/processing req))
  ([req & more] (common/render resp/processing req)))

(defn render-no-content
  "Renders 204 response without a body."
  ([]           (resp/no-content))
  ([req]        (common/render resp/no-content req))
  ([req & more] (common/render resp/no-content req)))

(defn render-reset-content
  "Renders 205 response without a body."
  ([]           (resp/reset-content))
  ([req]        (common/render resp/reset-content req))
  ([req & more] (common/render resp/reset-content req)))

;; Rendering based on application-logic error

(defn- add-missing-sub-status
  [req resp sub-status]
  (if sub-status
    (update resp :body
            (fn [body]
              (if (contains? body :sub-status)
                (add-missing-lang body req [:sub-status/title :sub-status/description])
                (let [tr-sub (i18n/no-default (common/translator-sub req))]
                  (-> body
                      (assoc :sub-status sub-status)
                      (add-missing-translation :sub-status/title sub-status tr-sub)
                      (add-missing-translation :sub-status/description sub-status ".full" tr-sub)
                      (add-missing-lang req [:sub-status/title :sub-status/description]))))))
    resp))

(defn render-status
  "Renders an error response for the given request map and optional `sub-status`
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
  ([]
   (resp/ok))
  ([req]
   (errors/render req nil render-ok req))
  ([req sub-status]
   (if-some [resp (errors/render req sub-status render-ok req)]
     (add-missing-sub-status req resp sub-status)))
  ([req sub-status default]
   (if-some [resp (errors/render req sub-status (or default render-ok) req)]
     (add-missing-sub-status req resp sub-status)))
  ([req sub-status default & more]
   (if-some [resp (apply errors/render req sub-status (or default render-ok) req more)]
     (add-missing-sub-status req resp sub-status))))

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
  ([]
   (resp/internal-server-error))
  ([req]
   (errors/render req nil render-internal-server-error req))
  ([req sub-status]
   (if-some [resp (errors/render req sub-status render-internal-server-error req)]
     (add-missing-sub-status req resp sub-status)))
  ([req sub-status default]
   (if-some [resp (errors/render req sub-status (or default render-internal-server-error) req)]
     (add-missing-sub-status req resp sub-status)))
  ([req sub-status default & more]
   (if-some [resp (apply errors/render req sub-status (or default render-internal-server-error) req more)]
     (add-missing-sub-status req resp sub-status))))

;; Linking helpers

(p/import-vars [amelinium.common
                path localized-path])

;; Anti-spam

(p/import-vars [amelinium.common
                random-uuid-or-empty])

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
   (update req :response/body assoc
           (common/lang-param req)
           (common/lang-id req)))
  ([req lang]
   (update req :response/body assoc
           (common/lang-param req)
           (or lang (common/lang-id req))))
  ([req lang field]
   (update req :response/body assoc
           (or field (common/lang-param req))
           (or lang (common/lang-id req)))))

(defn body-add-session-id
  ([req]
   (if-some [smap (common/session req)]
     (body-add-session-id req smap)
     req))
  ([req smap]
   (update req :response/body assoc
           (or (get smap :session-id-field) :session-id)
           (get smap :id)))
  ([req smap field]
   (update req :response/body assoc
           (or field (get smap :session-id-field) :session-id)
           (get smap :id))))

(defn session-status
  [smap]
  (if-not smap
    :missing
    (or (some-keyword-simple (get (get smap :error) :cause)) :unknown-error)))

(defn body-add-session-errors
  ([req]
   (body-add-session-errors req (common/session req) nil nil))
  ([req smap]
   (body-add-session-errors req smap nil nil))
  ([req smap translate-sub]
   (body-add-session-errors req smap translate-sub nil))
  ([req smap translate-sub lang]
   (if (get smap :valid?)
     req
     (let [translate-sub (or translate-sub (i18n/no-default (common/translator-sub req lang)))
           status        (session-status smap)
           message       (translate-sub :session status)]
       (update req :response/body assoc
               :session/status  status
               :session/message message)))))

(ns

    ^{:doc    "API helpers for amelinium."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.api

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:require [clojure.set                          :as          set]
            [clojure.string                       :as          str]
            [clojure.core.memoize                 :as          mem]
            [clojure.java.io                      :as           io]
            [potemkin.namespaces                  :as            p]
            [tick.core                            :as            t]
            [lazy-map.core                        :as     lazy-map]
            [reitit.core                          :as            r]
            [reitit.ring                          :as         ring]
            [ring.util.response]
            [ring.util.http-response              :as         resp]
            [ring.util.request                    :as          req]
            [amelinium.common                     :as       common]
            [amelinium.i18n                       :as         i18n]
            [amelinium.http                       :as         http]
            [amelinium.http.middleware.roles      :as        roles]
            [amelinium.http.middleware.language   :as     language]
            [amelinium.http.middleware.session    :as      session]
            [amelinium.http.middleware.db         :as       mid-db]
            [amelinium.http.middleware.validators :as   validators]
            [amelinium.common.oplog.auth          :as   oplog-auth]
            [amelinium.model.user                 :as         user]
            [amelinium.logging                    :as          log]
            [amelinium.db                         :as           db]
            [io.randomseed.utils.time             :as         time]
            [io.randomseed.utils.vec              :as          vec]
            [io.randomseed.utils.map              :as          map]
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

(defn render
  "Returns response body on a basis of a value associated with the `:response/body` key
  of the `req`."
  ([]
   (render nil))
  ([req]
   (let [body (get req :response/body)]
     (if (map? body)
       body
       (if (sequential? body)
         (seq body)
         body)))))

(defn response?
  "Returns `true` if the `req` context map is already an API response."
  [req]
  (and (map? req)
       (integer?  (:status req))
       (or (map?  (:headers req))
           (coll? (:body req)))))

(defn render-response
  "API response renderer. Uses the `render` function to render the response body unless
  the `req` is already a valid response (then it is returned as-is)."
  ([]
   (render-response resp/ok nil))
  ([resp-fn]
   (render-response resp-fn nil))
  ([resp-fn req]
   (if (response? req)
     req
     (if-some [headers (get req :response/headers)]
       (assoc (resp-fn (render req)) :headers headers)
       (resp-fn (render req))))))

(defn render-response-force
  "API response renderer. Uses the `render` function to render the response body."
  ([]
   (render-response-force resp/ok nil))
  ([resp-fn]
   (render-response-force resp-fn nil))
  ([resp-fn req]
   (if-some [headers (get req :response/headers)]
     (assoc (resp-fn (render req)) :headers headers)
     (resp-fn (render req)))))

;; OK response

(defn render-ok
  "Renders 200 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/ok nil))
  ([req] (render-response resp/ok req)))

;; Success responses with bodies

(defn render-accepted
  "Renders 202 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/accepted nil))
  ([req] (render-response resp/accepted req)))

(defn render-non-authoritative-information
  "Renders 203 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/non-authoritative-information nil))
  ([req] (render-response resp/non-authoritative-information req)))

(defn render-partial-content
  "Renders 206 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/partial-content nil))
  ([req] (render-response resp/partial-content req)))

(defn render-multi-status
  "Renders 207 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/multi-status nil))
  ([req] (render-response resp/multi-status req)))

(defn render-already-reported
  "Renders 208 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/already-reported nil))
  ([req] (render-response resp/already-reported req)))

(defn render-im-used
  "Renders 226 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/im-used nil))
  ([req] (render-response resp/im-used req)))

;; Error responses with possible bodies

(defn render-bad-request
  "Renders 400 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/bad-request nil))
  ([req] (render-response resp/bad-request req)))

(defn render-unauthorized
  "Renders 401 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/unauthorized nil))
  ([req] (render-response resp/unauthorized req)))

(defn render-payment-required
  "Renders 402 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/payment-required nil))
  ([req] (render-response resp/payment-required req)))

(defn render-forbidden
  "Renders 403 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/forbidden nil))
  ([req] (render-response resp/forbidden req)))

(defn render-not-found
  "Renders 404 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/not-found nil))
  ([req] (render-response resp/not-found req)))

(defn render-method-not-allowed
  "Renders 405 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/method-not-allowed nil))
  ([req] (render-response resp/method-not-allowed req)))

(defn render-not-acceptable
  "Renders 406 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/not-acceptable nil))
  ([req] (render-response resp/not-acceptable req)))

(defn render-proxy-authentication-required
  "Renders 407 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/proxy-authentication-required nil))
  ([req] (render-response resp/proxy-authentication-required req)))

(defn render-request-timeout
  "Renders 408 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/request-timeout nil))
  ([req] (render-response resp/request-timeout req)))

(defn render-conflict
  "Renders 409 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/conflict nil))
  ([req] (render-response resp/conflict req)))

(defn render-gone
  "Renders 410 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/gone nil))
  ([req] (render-response resp/gone req)))

(defn render-length-required
  "Renders 411 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/length-required nil))
  ([req] (render-response resp/length-required req)))

(defn render-precondition-failed
  "Renders 412 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/precondition-failed nil))
  ([req] (render-response resp/precondition-failed req)))

(defn render-request-entity-too-large
  "Renders 413 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/request-entity-too-large nil))
  ([req] (render-response resp/request-entity-too-large req)))

(defn render-request-uri-too-long
  "Renders 414 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/request-uri-too-long nil))
  ([req] (render-response resp/request-uri-too-long req)))

(defn render-unsupported-media-type
  "Renders 415 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/unsupported-media-type nil))
  ([req] (render-response resp/unsupported-media-type req)))

(defn render-requested-range-not-satisfiable
  "Renders 416 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/requested-range-not-satisfiable nil))
  ([req] (render-response resp/requested-range-not-satisfiable req)))

(defn render-expectation-failed
  "Renders 417 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/expectation-failed nil))
  ([req] (render-response resp/expectation-failed req)))

(defn render-im-a-teapot
  "Renders 418 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response common/im-a-teapot nil))
  ([req] (render-response common/im-a-teapot req)))

(defn render-enhance-your-calm
  "Renders 420 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/enhance-your-calm nil))
  ([req] (render-response resp/enhance-your-calm req)))

(defn render-misdirected-request
  "Renders 421 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response common/misdirected-request nil))
  ([req] (render-response common/misdirected-request req)))

(defn render-unprocessable-entity
  "Renders 422 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/unprocessable-entity nil))
  ([req] (render-response resp/unprocessable-entity req)))

(defn render-bad-params
  "Renders 422 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/unprocessable-entity nil))
  ([req] (render-response resp/unprocessable-entity req)))

(defn render-locked
  "Renders 423 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/locked nil))
  ([req] (render-response resp/locked req)))

(defn render-failed-dependency
  "Renders 424 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/failed-dependency nil))
  ([req] (render-response resp/failed-dependency req)))

(defn render-unordered-collection
  "Renders 425 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/unordered-collection nil))
  ([req] (render-response resp/unordered-collection req)))

(defn render-too-early
  "Renders 425 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/unordered-collection nil))
  ([req] (render-response resp/unordered-collection req)))

(defn render-upgrade-required
  "Renders 426 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/upgrade-required nil))
  ([req] (render-response resp/upgrade-required req)))

(defn render-precondition-required
  "Renders 428 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/precondition-required nil))
  ([req] (render-response resp/precondition-required req)))

(defn render-too-many-requests
  "Renders 429 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/too-many-requests nil))
  ([req] (render-response resp/too-many-requests req)))

(defn render-request-header-fields-too-large
  "Renders 431 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/request-header-fields-too-large nil))
  ([req] (render-response resp/request-header-fields-too-large req)))

(defn render-retry-with
  "Renders 449 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/retry-with nil))
  ([req] (render-response resp/retry-with req)))

(defn render-blocked-by-windows-parental-controls
  "Renders 450 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/blocked-by-windows-parental-controls nil))
  ([req] (render-response resp/blocked-by-windows-parental-controls req)))

(defn render-unavailable-for-legal-reasons
  "Renders 451 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/unavailable-for-legal-reasons nil))
  ([req] (render-response resp/unavailable-for-legal-reasons req)))

(defn render-internal-server-error
  "Renders 500 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/internal-server-error nil))
  ([req] (render-response resp/internal-server-error req)))

(defn render-not-implemented
  "Renders 501 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/not-implemented nil))
  ([req] (render-response resp/not-implemented req)))

(defn render-bad-gateway
  "Renders 502 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/bad-gateway nil))
  ([req] (render-response resp/bad-gateway req)))

(defn render-service-unavailable
  "Renders 503 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/service-unavailable nil))
  ([req] (render-response resp/service-unavailable req)))

(defn render-gateway-timeout
  "Renders 504 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/gateway-timeout nil))
  ([req] (render-response resp/gateway-timeout req)))

(defn render-http-version-not-supported
  "Renders 505 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/http-version-not-supported nil))
  ([req] (render-response resp/http-version-not-supported req)))

(defn render-variant-also-negotiates
  "Renders 506 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/variant-also-negotiates nil))
  ([req] (render-response resp/variant-also-negotiates req)))

(defn render-insufficient-storage
  "Renders 507 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/insufficient-storage nil))
  ([req] (render-response resp/insufficient-storage req)))

(defn render-loop-detected
  "Renders 508 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/loop-detected nil))
  ([req] (render-response resp/loop-detected req)))

(defn render-bandwidth-limit-exceeded
  "Renders 509 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/bandwidth-limit-exceeded nil))
  ([req] (render-response resp/bandwidth-limit-exceeded req)))

(defn render-not-extended
  "Renders 510 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/not-extended nil))
  ([req] (render-response resp/not-extended req)))

(defn render-network-authentication-required
  "Renders 511 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/network-authentication-required nil))
  ([req] (render-response resp/network-authentication-required req)))

(defn render-network-read-timeout
  "Renders 598 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/network-read-timeout nil))
  ([req] (render-response resp/network-read-timeout req)))

(defn render-network-connect-timeout
  "Renders 599 response with possible body taken from a request map (under the
  `:response/body`)."
  ([]    (render-response resp/network-connect-timeout nil))
  ([req] (render-response resp/network-connect-timeout req)))

;; Resource creation success, redirect with a possible body

(defn render-created
  "Renders 201 response with a redirect (possibly localized if a destination path is
  language-parameterized) and possible body taken from a request map (under the
  `:response/body`)."
  ([]
   (render-response resp/created))
  ([req]
   (render-response-force
    (common/created req)))
  ([req name-or-path]
   (render-response-force
    (common/created req name-or-path)))
  ([req name-or-path lang]
   (render-response-force
    (common/created req name-or-path lang)))
  ([req name-or-path lang params]
   (render-response-force
    (common/created req name-or-path lang params)))
  ([req name-or-path lang params query-params]
   (render-response-force
    (common/created req name-or-path lang params query-params)))
  ([req name-or-path lang params query-params & more]
   (render-response-force
    (apply common/created req name-or-path lang params query-params more))))

(defn render-localized-created
  "Renders 201 response with a localized redirect and possible body taken from a
  request map (under the `:response/body`)."
  ([]
   (render-response resp/created))
  ([req]
   (render-response-force
    (common/localized-created req)))
  ([req name-or-path]
   (render-response-force
    (common/localized-created req name-or-path)))
  ([req name-or-path lang]
   (render-response-force
    (common/localized-created req name-or-path lang)))
  ([req name-or-path lang params]
   (render-response-force
    (common/localized-created req name-or-path lang params)))
  ([req name-or-path lang params query-params]
   (render-response-force
    (common/localized-created req name-or-path lang params query-params)))
  ([req name-or-path lang params query-params & more]
   (render-response-force
    (apply common/localized-created req name-or-path lang params query-params more))))

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
     (let [translate-sub (or translate-sub (i18n/translator-sub req lang))
           status        (session-status smap)
           message       (translate-sub :session status)]
       (update req :response/body assoc
               :session/status  status
               :session/message message)))))

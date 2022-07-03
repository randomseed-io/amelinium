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
                login-page? auth-page? login-auth-state ])

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
  [req]
  (and (map? req)
       (integer?  (:status req))
       (or (map?  (:headers req))
           (coll? (:body req)))))

(defn render-response-force
  "Universal response renderer. Uses the render function to render the response body."
  ([]
   (render-response-force resp/ok nil))
  ([resp-fn]
   (render-response-force resp-fn nil))
  ([resp-fn req]
   (if-some [headers (get req :response/headers)]
     (assoc (resp-fn (render req)) :headers headers)
     (resp-fn (render req)))))

(defn render-response
  "Universal response renderer. Uses the render function to render the response body
  unless the `req` is already a valid response (then it is returned as-is)."
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

;; OK response

(defn render-ok
  ([]    (render-response resp/ok nil))
  ([req] (render-response resp/ok req)))

;; Responses with bodies

(defn render-accepted
  ([]    (render-response resp/accepted nil))
  ([req] (render-response resp/accepted req)))

(defn render-non-authoritative-information
  ([]    (render-response resp/non-authoritative-information nil))
  ([req] (render-response resp/non-authoritative-information req)))

(defn render-partial-content
  ([]    (render-response resp/partial-content nil))
  ([req] (render-response resp/partial-content req)))

(defn render-multi-status
  ([]    (render-response resp/multi-status nil))
  ([req] (render-response resp/multi-status req)))

(defn render-already-reported
  ([]    (render-response resp/already-reported nil))
  ([req] (render-response resp/already-reported req)))

(defn render-im-used
  ([]    (render-response resp/im-used nil))
  ([req] (render-response resp/im-used req)))

;; Error responses with possible bodies

(defn render-bad-request
  ([]    (render-response resp/bad-request nil))
  ([req] (render-response resp/bad-request req)))

(defn render-bad-params
  ([]    (render-response resp/unprocessable-entity nil))
  ([req] (render-response resp/unprocessable-entity req)))

(defn render-unprocessable-entity
  ([]    (render-response resp/unprocessable-entity nil))
  ([req] (render-response resp/unprocessable-entity req)))

(defn render-not-found
  ([]    (render-response resp/not-found nil))
  ([req] (render-response resp/not-found req)))

(defn render-unauthorized
  ([]    (render-response resp/unauthorized nil))
  ([req] (render-response resp/unauthorized req)))

(defn render-payment-required
  ([]    (render-response resp/payment-required nil))
  ([req] (render-response resp/payment-required req)))

(defn render-forbidden
  ([]    (render-response resp/forbidden nil))
  ([req] (render-response resp/forbidden req)))

(defn render-method-not-allowed
  ([]    (render-response resp/method-not-allowed nil))
  ([req] (render-response resp/method-not-allowed req)))

(defn render-not-acceptable
  ([]    (render-response resp/not-acceptable nil))
  ([req] (render-response resp/not-acceptable req)))

(defn render-proxy-authentication-required
  ([]    (render-response resp/proxy-authentication-required nil))
  ([req] (render-response resp/proxy-authentication-required req)))

;; Redirect with a possible body

(defn render-created
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
  ([]    (resp/continue))
  ([req] (resp/continue)))

(defn render-switching-protocols
  ([]    (resp/switching-protocols))
  ([req] (resp/switching-protocols)))

(defn render-processing
  ([]    (resp/processing))
  ([req] (resp/processing)))

(defn render-no-content
  ([]    (resp/no-content))
  ([req] (resp/no-content)))

(defn render-not-modified
  ([]    (resp/not-modified))
  ([req] (resp/not-modified)))

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
                  (when k-some  (map vector k-some  (repeatedly random-uuid)))
                  (when k-blank (map vector k-blank (repeat "")))
                  (when k-any   (map vector k-any   (repeatedly #(random-uuid-or-empty rng)))))]
     (when (seq r)
       (into {} r)))))

;; Other helpers

(defn lang-url
  [req path-or-name lang params query-params lang-settings]
  (common/lang-url true req path-or-name lang params query-params lang-settings))

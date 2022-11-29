(ns

    ^{:doc    "amelinium service, common populators."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.common.populators

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:require [reitit.core                        :as          r]
            [amelinium.auth                     :as       auth]
            [amelinium.model.user               :as       user]
            [io.randomseed.utils.map            :as        map]
            [io.randomseed.utils                :refer    :all]
            [amelinium.http.middleware.session  :as    session]
            [amelinium.http.middleware.coercion :as   coercion]
            [amelinium.i18n                     :as       i18n]
            [amelinium.common                   :as     common]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data population

(defn route-data
  "Injects route data directly into a request map."
  [req _ _]
  (get (get req ::r/match) :data))

(defn auth-db
  "Injects authorization data source directly into a request map. Uses global
  authentication configuration from a current route data."
  [req _ _]
  (get (or (get (get req :route/data) :auth/setup)
           (get req :auth/setup))
       :db))

(defn auth-types
  "Injects authorization configurations directly into a request map. Uses global
  authentication configuration from a current route data."
  [req _ _]
  (get (or (get (get req :route/data) :auth/setup)
           (get req :auth/setup))
       :types))

(defn oplog-logger
  "Injects operations logger function into a request map."
  [req _ _]
  (delay (common/oplog-logger req)))

(defn user-lang
  "Injects user's preferred language into a request map."
  [req _ _]
  (delay
    (if-some [db (auth/db req)]
      (if-some [smap (session/of req)]
        (if-some [user-id (session/user-id smap)]
          (let [supported (get (get req :language/settings) :supported)]
            (contains? supported (user/setting db user-id :language))))))))

(defn i18n-translator
  "Creates shared translator for currently detected language."
  [req _ _]
  (delay (i18n/translator req)))

(defn i18n-translator-sub
  "Creates shared translator (supporting namespaces and keys) for currently detected
  language."
  [req _ _]
  (delay (i18n/translator-sub req)))

(defn i18n-translator-nd
  "Creates shared translator for currently detected language. The translator returns
  `nil` if the key is not found."
  [req _ _]
  (delay (i18n/no-default (i18n/translator req))))

(defn i18n-translator-sub-nd
  "Creates shared translator (supporting namespaces and keys) for currently detected
  language. The translator returns `nil` if the key is not found."
  [req _ _]
  (delay (i18n/no-default (i18n/translator-sub req))))

(defn populate-form-errors
  "Tries to obtain form errors from previously visited page, saved as a session
  variable `:form-errors` or as a query parameter `form-errors`. The result is a map
  of at least 3 keys: `:errors` (parsed errors), `:dest` (destination URI, matching
  current URI if using a session variable), `:params` (map of other parameters and
  their values, only when session variable is a source)."
  [req _ _]
  (delay
    (if-some [qp (get req :query-params)]
      (if-let [query-params-errors (get qp "form-errors")]
        (or (if-let [smap (session/valid-of req)]
              (let [sess-var (session/fetch-var! smap :form-errors)]
                (if (map? sess-var)
                  (let [expected-uri (get sess-var :dest)]
                    (if (or (not expected-uri) (= expected-uri (get req :uri)))
                      (let [sess-var-errors (get sess-var :errors)]
                        (if (and (map? sess-var-errors) (not-empty sess-var-errors))
                          (map/qassoc sess-var :errors (coercion/parse-errors sess-var-errors)))))))))
            {:errors (coercion/parse-errors query-params-errors)
             :dest   (get req :uri)
             :params nil})))))

(def form-errors
  {:compile (fn [data _ _]
              (if data
                populate-form-errors))})

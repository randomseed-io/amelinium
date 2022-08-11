(ns

    ^{:doc    "amelinium service, common populators."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.common.populators

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:require [potemkin.namespaces                :as          p]
            [clojure.string                     :as        str]
            [reitit.ring                        :as       ring]
            [ring.middleware.keyword-params     :as    ring-kw]
            [ring.util.http-response            :as       resp]
            [ring.util.request                  :as        req]
            [ring.util.codec                    :as      codec]
            [reitit.core                        :as          r]
            [reitit.ring                        :as       ring]
            [lazy-map.core                      :as   lazy-map]
            [tick.core                          :as          t]
            [amelinium.logging                  :as        log]
            [amelinium.model.user               :as       user]
            [io.randomseed.utils.time           :as       time]
            [io.randomseed.utils.var            :as        var]
            [io.randomseed.utils.map            :as        map]
            [io.randomseed.utils                :refer    :all]
            [amelinium.http.middleware.language :as   language]
            [amelinium.http.middleware.session  :as    session]
            [amelinium.i18n                     :as       i18n]
            [amelinium.common                   :as     common]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data population

(defn route-data
  "Injects route data directly into a request map."
  [req _ _]
  (get (get req ::r/match) :data))

(defn auth-db
  "Injects authorization data source directly into a request map."
  [req _ _]
  (get (or (get (get req :route/data) :auth/config)
           (get req :auth/config))
       :db))

(defn auth-types
  "Injects authorization configurations directly into a request map."
  [req _ _]
  (get (or (get (get req :route/data) :auth/config)
           (get req :auth/config))
       :types))

(defn oplog-logger
  "Injects operations logger function into a request map."
  [req _ _]
  (delay (common/oplog-logger req)))

(defn user-lang
  "Injects user's preferred language into a request map."
  [req _ _]
  (delay
    (if-some [db (common/auth-db req)]
      (if-some [smap (common/session req)]
        (if-some [user-id (get smap :user/id)]
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

(defn form-errors
  "Tries to obtain form errors from previously visited page, saved as a
  session variable `:form-errors` or as a query parameter `form-errors`."
  [req _ _]
  (delay
    (if-some [qp (get req :query-params)]
      (if-some [query-params-errors (get qp "form-errors")]
        (let [current-form-params (or (get req :form-params) {})]
          (if-some [query-params-errors (some-str query-params-errors)]
            (->> (str/split query-params-errors #",")
                 (map #(if % (str/trim %)))
                 (filter identity)
                 (filter (partial contains? current-form-params))
                 (map keyword) seq set)
            (let [[opts smap]  (common/config+session req)
                  svar         (if (and opts smap (get smap :valid?)) (session/fetch-var! opts smap :form-errors))
                  expected-uri (if svar (get svar :uri))
                  uri-ok?      (or (not expected-uri) (= expected-uri (get req :uri)))
                  errors       (if (and uri-ok? svar) (get svar :errors))]
              (if errors
                (->> errors
                     (map some-str)
                     (filter identity)
                     (filter (partial contains? current-form-params))
                     (map keyword) seq set)))))))))

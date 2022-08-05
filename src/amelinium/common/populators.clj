(ns

    ^{:doc    "amelinium service, common populators."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.common.populators

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:require [potemkin.namespaces                :as          p]
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
            [amelinium.i18n                     :as       i18n]
            [amelinium.common                   :as     common]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data population

(defn route-data
  "Injects route data directly into a request map."
  [req _]
  (get (get req ::r/match) :data))

(defn auth-db
  "Injects authorization data source directly into a request map."
  [req _]
  (get (or (get (get req :route/data) :auth/config)
           (get req :auth/config))
       :db))

(defn auth-types
  "Injects authorization configurations directly into a request map."
  [req _]
  (get (or (get (get req :route/data) :auth/config)
           (get req :auth/config))
       :types))

(defn oplog-logger
  "Injects operations logger function into a request map."
  [req _]
  (delay (common/oplog-logger req)))

(defn user-lang
  "Injects user's preferred language into a request map."
  [req _]
  (delay
    (if-some [db (common/auth-db req)]
      (if-some [smap (common/session req)]
        (if-some [user-id (get smap :user/id)]
          (let [supported (get (get req :language/settings) :supported)]
            (contains? supported (user/setting db user-id :language))))))))

(defn i18n-translator
  "Creates shared translator for currently detected language."
  [req _]
  (delay (i18n/translator req)))

(defn i18n-translator-sub
  "Creates shared translator (supporting namespaces and keys) for currently detected
  language."
  [req _]
  (delay (i18n/translator-sub req)))

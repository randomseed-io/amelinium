(ns

    ^{:doc    "amelinium service, Twilio client."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    amelinium.http.client.twilio

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:require [clojure.set                  :as        set]
            [clojure.string               :as        str]
            [tick.core                    :as          t]
            [hato.client                  :as         hc]
            [amelinium.db                 :as         db]
            [amelinium.logging            :as        log]
            [amelinium.system             :as     system]
            [io.randomseed.utils          :refer    :all]
            [io.randomseed.utils.time     :as       time]
            [io.randomseed.utils.var      :as        var]
            [io.randomseed.utils.map      :as        map]))

(def ^:const config-tag (re-pattern ":([a-zA-Z][a-zA-Z0-9_\\-]+)"))

(defn replace-tags
  [config s]
  (when-some [url (some-str s)]
    (str/replace url config-tag #(get config (keyword (nth % 1)) (nth % 0)))))

(defn prep-auth
  [{:keys [api-sid api-key api-token account-sid account-key account-token username password]
    :as   config}]
  (cond
    (and username password)         (assoc config :auth-pub username    :auth-key password)
    (and api-sid api-token)         (assoc config :auth-pub api-sid     :auth-tok api-token)
    (and api-sid api-key)           (assoc config :auth-pub api-sid     :auth-key api-key)
    (and account-sid account-token) (assoc config :auth-pub account-sid :auth-tok account-token)
    (and account-sid account-key)   (assoc config :auth-pub account-sid :auth-key account-key)
    api-token                       (assoc config :auth-tok api-token)
    api-key                         (assoc config :auth-key api-key)
    api-sid                         (assoc config :auth-pub api-sid)
    account-token                   (assoc config :auth-tok account-token)
    account-key                     (assoc config :auth-key account-key)
    account-sid                     (assoc config :auth-pub account-sid)))

(defn is-json?
  [config]
  (-> (get config :content-type)
      #{:json :application/json "application/json" "json"}
      boolean))

(defn prep-params
  [{:keys [parameters]
    :as   config}]
  (if-not (and parameters (map? parameters) (valuable? parameters))
    (dissoc config :parameters)
    (if (is-json? config)
      config
      (assoc config :parameters
             (-> parameters
                 (update (partial map/map-vals (partial replace-tags config)))
                 (update (partial map/map-keys some-str)))))))

(defn prep-twilio
  [{:keys [enabled? prepared?]
    :or   {enabled? true prepared? false}
    :as   config}]
  (if (:prepared? config)
    config
    (-> config
        (assoc  :prepared?    true)
        (assoc  :enabled?     (boolean enabled?))
        (map/update-existing  :account-sid   some-str)
        (map/update-existing  :account-key   some-str)
        (map/update-existing  :account-token some-str)
        (map/update-existing  :api-sid       some-str)
        (map/update-existing  :api-key       some-str)
        (map/update-existing  :api-token     some-str)
        (map/update-existing  :service-sid   some-str)
        (map/update-existing  :service       some-str)
        (map/update-existing  :username      some-str)
        (map/update-existing  :password      some-str)
        prep-auth
        prep-params
        (update :url          (partial replace-tags config))
        (update :client-opts  #(if   (and (map? %) (valuable? %)) % {})))))

(defn init-twilio
  [config]
  (if-not (:enabled? config)
    (constantly nil)
    (let [auth-pub   (:auth-pub config)
          auth-key   (:auth-key config)
          auth-tok   (:auth-tok config)
          cli-opts   (-> (or (:client-opts config) {})
                         (map/assoc-missing
                          :authenticator (when (and auth-pub auth-key)
                                           {:user auth-pub :pass auth-key}))
                         (map/update-existing
                          :connect-timeout #(when %
                                              (time/milliseconds
                                               (time/parse-duration % :second)))))
          config     (assoc config :client-opts cli-opts)
          url        (get config :url)
          req-method (or (get cli-opts :request-method) :post)
          accept     (or (get config :accept) :json)
          client     (hc/build-http-client (:client-opts config))
          req-opts   {:url            url
                      :accept         accept
                      :request-method req-method
                      :http-client    client}
          req-opts   (if auth-tok (assoc req-opts :oauth-token auth-tok) req-opts)
          req-opts   (if content-type
                       (assoc req-opts :content-type content-type)
                       req-opts)]
      (if-some [default-params (:parameters config)]
        (fn twilio-request
          ([opts parameters]
           (let [opts       (into req-opts opts)
                 parameters (if (is-json? opts) parameters (map/map-keys some-str parameters))
                 opts       (update opts :form-params
                                    #(if %
                                       (into (into default-params %) parameters)
                                       (into default-params parameters)))]
             (hc/request opts)))
          ([parameters]
           (let [parameters (if (is-json? req-opts) parameters (map/map-keys some-str parameters))
                 opts       (assoc req-opts :form-params (into default-params parameters))]
             (hc/request opts)))
          ([]
           (let [opts (assoc req-opts :form-params default-params)]
             (hc/request opts))))
        (fn twilio-request
          ([opts parameters]
           (let [opts       (into req-opts opts)
                 parameters (if (is-json? opts) parameters (map/map-keys some-str parameters))
                 opts       (update opts :form-params #(if % (into % parameters) parameters))]
             (hc/request opts)))
          ([parameters]
           (let [parameters (if (is-json? req-opts) parameters (map/map-keys some-str parameters))
                 opts       (assoc req-opts :form-params parameters)]
             (hc/request opts)))
          ([]
           (hc/request req-opts)))))))

(defn sendmail
  ([f email]
   (f {:personalizations [{:to          [{:email (str email)}]
                           :template-id (str template-id)}]}))
  ([f email template-data]
   (f {:personalizations [{:to                    [{:email (str email)}]
                           :template-id           (str template-id)
                           :dynamic_template_data template-data}]})))

(system/add-init  ::default [k config] (var/make k (init-twilio (prep-twilio config))))
(system/add-prep  ::default [_ config] (prep-twilio config))
(system/add-halt! ::default [k config] (var/make k nil))

(derive ::sms    ::default)
(derive ::email  ::default)
(derive ::verify ::default)
